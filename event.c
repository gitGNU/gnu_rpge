/*
Copyright Remco Bras 2007
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

convertors(event);

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
  sequence_append (&stackptr->events, make_event_obj (e));
}

event
eventstack_getfirstevent (eventstack * stackptr)
{
  if (!stackptr->events.objcount)
    return make_event (SCM_UNDEFINED, SCM_UNDEFINED);
  else
    {
      event event = get_obj_event (stackptr->events.data[0]);
      sequence_remove_at (&stackptr->events, 0);
      return event;
    }
}

int
find_empty_luser (eventstack es)
{
  for (int i = 0; i < es.indices.objcount; i++)
    {
      if (*((int *) es.indices.data[i].data) == -1)
	return i;
    }
  return -1;
}

unsigned long int
eventstack_open (eventstack * es)
{
  int index = find_empty_luser (*es);
  if (index != -1)
    {
      *((int *) es->indices.data[index].data) = 0;
      return index;
    }
  else
    {
      return sequence_append (&es->indices, make_int_obj (0));
    }
}


void
eventstack_close (eventstack * es, unsigned long int luser)
{
  *((int *) es->indices.data[luser].data) = -1;
}

int
eventstack_get_index_of_user (eventstack es, unsigned long int luser)
{
  if (luser >= es.indices.objcount)
    return -1;
  else
    return *((int *) es.indices.data[luser].data);
}

/*
A simple function to clean up surplus indices and events.
*/

void
eventstack_clean_stack (eventstack * es)
{
  int index, lowest = -1;
  for (int i = 0; i < es->indices.objcount; i++)
    {
      index = get_obj_int (es->indices.data[i]);
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
	  *((int *) es->indices.data[j].data) -= lowest;
	}
    }
}

event
eventstack_get_first_of_user (eventstack * es, unsigned long int luser)
{
  int index;
  eventstack_clean_stack (es);
  index = eventstack_get_index_of_user (*es, luser);
  if (index != -1 && index < es->events.objcount)
    {
      (*((int *) es->indices.data[luser].data))++;
      return *((event *) es->events.data[index].data);
    }
  else
    return make_event (SCM_EOL, SCM_EOL);
}
