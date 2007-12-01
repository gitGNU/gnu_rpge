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

event
make_event(SCM type, SCM data)
{
  event e;
  e.type = type;
  e.data = data;
  return e;
}

eventstack
eventstack_init(void)
{
  eventstack es;
  es.events = NULL;
  es.indices = NULL;
  es.stacksize = es.indexcount = 0;
  return es;
}


void
eventstack_clear(eventstack* stackptr)
{
  free(stackptr->events);
  stackptr->stacksize = 0;
  stackptr->events = NULL;
}

void
eventstack_addevent(eventstack* stackptr, event e)
{
  event* neweventsptr = malloc((stackptr->stacksize+1)*sizeof(event));
  if(stackptr->events)
    memcpy(neweventsptr,stackptr->events,sizeof(event)*stackptr->stacksize);
  stackptr->events = neweventsptr;
  stackptr->events[stackptr->stacksize] = e;
  stackptr->stacksize++;
}

event
eventstack_getfirstevent(eventstack* stackptr)
{
  if(!stackptr->stacksize)
    return make_event(SCM_UNDEFINED, SCM_UNDEFINED);
  else
    {
      event* newevents = malloc(sizeof(event)*(stackptr->stacksize-1));
      event event = stackptr->events[0];
      memcpy(newevents,stackptr->events+1,(stackptr->stacksize-1)*sizeof(event));
      free(stackptr->events);
      stackptr->stacksize--;
      stackptr->events = newevents;
      return event;
    }
}

int
find_empty_luser(eventstack es)
{
    for(int i = 0; i < es.indexcount; i++)
      {
         if(es.indices[i] == -1)
           return i;
      }
  return -1;
}

unsigned long int 
eventstack_open(eventstack* es)
{
  int index = find_empty_luser(*es);
  if(index != -1)
    {
      es->indices[index] = 0;
      return index;
    }
  else
    {
       unsigned long int* newindices = malloc(sizeof(unsigned long int)*(es->indexcount+1));
      memcpy(newindices,es->indices,sizeof(unsigned long int)*es->indexcount);
      newindices[es->indexcount] = 0; 
      es->indexcount++;
      free(es->indices);
      es->indices = newindices;
      return es->indexcount - 1;
    }
}

 
void
eventstack_close(eventstack* es, unsigned long int luser)
{
  es->indices[luser] = -1;
}

int 
eventstack_get_index_of_user(eventstack es, unsigned long int luser)
{
  if(luser >= es.indexcount)
    return -1;
  else
    return es.indices[luser];
}

/*
  TODO: Add automatic destruction of first element in case all indices are > 0
*/
event
eventstack_get_first_of_user(eventstack* es, unsigned long int luser)
{
  int index = eventstack_get_index_of_user(*es,luser);
    if(index != -1 && index < es->stacksize)
      {
        es->indices[luser]++;
        return es->events[index];
      }
    else
      return make_event(SCM_EOL, SCM_EOL);
}
