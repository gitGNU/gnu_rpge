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
} eventstack;


eventstack eventstack_init();
void eventstack_clear(eventstack* stackptr);
void eventstack_addevent(eventstack* stackptr, event event);
event eventstack_getfirstevent(eventstack* stackptr);

#endif
