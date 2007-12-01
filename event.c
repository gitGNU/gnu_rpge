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

object
make_event_obj(event e)
{
  object o;
  o.typeinfo = TYPE_EVENT;
  o.data = malloc(sizeof(event));
  *((event*)o.data) = e;
  return o;
}

event
get_obj_event(object o)
{
  return  *((event*)o.data);
}

eventstack
eventstack_init(void)
{
  eventstack es;
  es.indices = es.events = sequence_init();
  return es;
}


void
eventstack_clear(eventstack* stackptr)
{
  sequence_free(stackptr->events);
  sequence_free(stackptr->indices);
  stackptr->events = stackptr->indices = sequence_init();
}

void
eventstack_addevent(eventstack* stackptr, event e)
{
  sequence_append(&stackptr->events,make_event_obj(e));
}

event
eventstack_getfirstevent(eventstack* stackptr)
{
  if(!stackptr->events.objcount)
    return make_event(SCM_UNDEFINED, SCM_UNDEFINED);
  else
    {
      event event = get_obj_event(stackptr->events.data[0]);
      sequence_remove_at(&stackptr->events,0);
      return event;
    }
}

int
find_empty_luser(eventstack es)
{
    for(int i = 0; i < es.indices.objcount; i++)
      {
         if(*((int*)es.indices.data[i].data) == -1)
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
      *((int*)es->indices.data[index].data) = 0;
      return index;
    }
  else
    {
      return sequence_append(&es->indices,make_int_obj(0));
    }
}

 
void
eventstack_close(eventstack* es, unsigned long int luser)
{
  *((int*)es->indices.data[luser].data) = -1;
}

int 
eventstack_get_index_of_user(eventstack es, unsigned long int luser)
{
  if(luser >= es.indices.objcount)
    return -1;
  else
    return *((int*)es.indices.data[luser].data);
}

/*
  TODO: Add automatic destruction of first element in case all indices are > 0
*/
event
eventstack_get_first_of_user(eventstack* es, unsigned long int luser)
{
  int index = eventstack_get_index_of_user(*es,luser);
    if(index != -1 && index < es->events.objcount)
      {
        (*((int*)es->indices.data[luser].data))++;
        return *((event*)es->events.data[index].data);
      }
    else
      return make_event(SCM_EOL, SCM_EOL);
}
