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

eventstack
eventstack_init(void)
{
  eventstack es;
  es.events = NULL;
  es.stacksize = 0;
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
eventstack_addevent(eventstack* stackptr, unsigned long int event)
{
  unsigned long int* neweventsptr = malloc((stackptr->stacksize+1)*sizeof(unsigned long int));
  memcpy(neweventsptr,stackptr->events,sizeof(unsigned long int)*stackptr->stacksize);
  stackptr->events[stackptr->stacksize] = event;
  stackptr->stacksize++;
}

unsigned long int
eventstack_getfirstevent(eventstack* stackptr)
{
  if(!stackptr->stacksize)
    return EVENT_NOTHING;
  else
    {
      unsigned long int* newevents = malloc(sizeof(unsigned long int)*(stackptr->stacksize-1));
      unsigned long int event = stackptr->events[0];
      memcpy(newevents,stackptr->events+1,(stackptr->stacksize-1)*sizeof(unsigned long int));
      free(stackptr->events);
      stackptr->stacksize--;
      stackptr->events = newevents;
      return event;
    }
}
