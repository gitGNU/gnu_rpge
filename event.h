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

#ifndef EVENT_H
#define EVENT_H
#include <stdlib.h>
#include <string.h>
#include <libguile.h>

typedef struct
{
  SCM type;
  SCM data;
}event;

typedef struct
{
  event* events;
  unsigned long int stacksize;
  unsigned long int* indices; /* See docs (currently non-existent) for details, used in regulating access by multiple threads */
  unsigned long int indexcount;
} eventstack;


eventstack eventstack_init();
void eventstack_clear(eventstack* stackptr);
void eventstack_addevent(eventstack* stackptr, event event);
event eventstack_getfirstevent(eventstack* stackptr);
event make_event(SCM type, SCM data);
unsigned long int eventstack_open(eventstack* es);
void eventstack_close(eventstack* es,unsigned long int luser);
event evenstack_get_next_event(eventstack es, unsigned long int luser);
int eventstack_get_index_of_user(eventstack es, unsigned long int luser);
event eventstack_get_first_of_user(eventstack* es, unsigned long int luser);
#endif
