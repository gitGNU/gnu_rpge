/*
Copyright Remco Bras 2007,2008
This file is part of RPGE.

RPGE is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3 of the License, or
(at your option) any later version.

RPGE is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>
*/

#include "event.h"

eventstack global_usereventstack;

/*Some macros to make locking stacks easier*/
#define LOCK_STACK(s) s->lock? SDL_mutexP(s->lock) : -1;
#define UNLOCK_STACK(s) s->lock? SDL_mutexV(s->lock) : -1;

S_CONVERTORS(event,EVENT);
S_CONVERTORS(user,USER);

event
make_event (SCM type, SCM data)
{
  event e;
  e.type = type;
  e.data = data;
  scm_gc_protect_object (type);
  scm_gc_protect_object (data);
  return e;
}

eventstack
eventstack_init (void)
{
  eventstack es;
  es.indices = es.events = sequence_init ();
  es.lock = SDL_CreateMutex();
  es.block = SDL_CreateCond();
  return es;
}


void
eventstack_clear (eventstack * stackptr)
{
  sequence_free (stackptr->events);
  sequence_free (stackptr->indices);
  stackptr->events = stackptr->indices = sequence_init ();
}

void
eventstack_addevent (eventstack * stackptr, event e)
{
  /*Only add events if someone cares to get them.*/
  if(stackptr->indices.objcount)
    {
      LOCK_STACK(stackptr);
      sequence_append (&stackptr->events, make_event_obj (e));
      UNLOCK_STACK(stackptr);
      SDL_CondBroadcast(stackptr->block);
    } 
} 

event 
eventstack_getfirstevent (eventstack * stackptr)
{ 
  if (!stackptr->events.objcount)
    return make_event (SCM_UNDEFINED, SCM_UNDEFINED);
  else 
    {
      event event = get_obj_event (stackptr->events.data[0]);
      LOCK_STACK(stackptr);
      sequence_remove_at (&stackptr->events, 0);
      UNLOCK_STACK(stackptr);
      return event;
    }
}

int
find_empty_luser (eventstack es)
{
  for (int i = 0; i < es.indices.objcount; i++)
    {
      if (((user *) es.indices.data[i].data)->index == -1)
	return i;
    }
  return -1;
}

unsigned long int 
eventstack_open (eventstack * es,char block)
{
  int index = find_empty_luser (*es);
  if (index != -1)
    {
      LOCK_STACK(es);
      ((user *) es->indices.data[index].data)->index = 0;
      UNLOCK_STACK(es);
      return index;
    }
  else
    {
      LOCK_STACK(es);
      user u = {0,0};
      u.block = block;
      int index = sequence_append(&es->indices,make_user_obj(u));
      UNLOCK_STACK(es);
      return index;
    }
}


void
eventstack_close (eventstack * es, unsigned long int luser)
{
  LOCK_STACK(es);
  ((user *) es->indices.data[luser].data)->index = -1;
  UNLOCK_STACK(es);
}

int
eventstack_get_index_of_user (eventstack es, unsigned long int luser)
{
  if (luser >= es.indices.objcount)
    return -1;
  else
    return ((user *) es.indices.data[luser].data)->index;
}

/*
A simple function to clean up surplus indices and events.
*/

void
eventstack_clean_stack (eventstack * es)
{
  int index, lowest = -1;
  user checked;
  LOCK_STACK(es);
  for (int i = 0; i < es->indices.objcount; i++)
    {
      checked = get_obj_user (es->indices.data[i]);
      index = checked.index;
      if (!index)
	{
	  lowest = -1;
	  break;
	}
      else if (index == -1)
	continue;
      else
	{
	  if (index < lowest || lowest == -1)
	    lowest = index;
	}
    }
  if (lowest > 0)
    {
      /*
         Performance note: This bit here's sluggish, there should be a sequence_remove_upto, which I'll get around to some other time. Also deprotects objects in the events.
       */
      for (int j = 0; j < lowest; j++)
	{
	  scm_gc_unprotect_object (((event *) es->events.data[0].data)->type);
	  scm_gc_unprotect_object (((event *) es->events.data[0].data)->data);
	  sequence_remove_at (&(es->events), 0);
	}
      for (int j = 0; j < es->indices.objcount; j++)
	{
	  ((user *) es->indices.data[j].data)->index -= lowest;
	}
    }
  UNLOCK_STACK(es);
}

event
eventstack_get_first_of_user (eventstack * es, unsigned long int luser)
{
  int index,status;
  eventstack_clean_stack (es);
  char block = ((user*)es->indices.data[luser].data)->block;
  /*
  Note: index must be reset every iteration, just in case our waits actually are interrupted early and/or someone else collected events, changing our index
  */
  while((index = eventstack_get_index_of_user(*es,luser)) >= es->events.objcount && block)
    {
      LOCK_STACK(es);
      while((status = SDL_CondWaitTimeout(es->block,es->lock,250))==SDL_MUTEX_TIMEDOUT)
	{
	  /*This lock/unlock cycle is necessary to prevent a deadlock if other threads need this mutex before they can SCM_TICK */
	  UNLOCK_STACK(es);
	  SCM_TICK;
	  LOCK_STACK(es);
	}
      if(status == -1)
	{
	  fprintf(stderr,"Error: %s\n",SDL_GetError());
	}
      UNLOCK_STACK(es);
    }
  if (index != -1 && index < es->events.objcount)
    {
      LOCK_STACK(es);
      (((user *) es->indices.data[luser].data)->index)++;
      UNLOCK_STACK(es);
      return *((event *) es->events.data[index].data);
    }
  else
    return make_event (SCM_EOL, SCM_EOL);
}
