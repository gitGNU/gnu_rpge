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

/*
dispatch.h: define the data structures required for automated dispatching of (SDL) events, declare registration functions etc.
*/

#ifndef DISPATCH_H
#define DISPATCH_H
#include <SDL/SDL.h>
#include <libguile.h>
#include "sequence.h"
  
typedef struct
{
  Uint32 eventid;
  SCM (* typefunc)(SDL_Event);
  SCM (* datafunc)(SDL_Event);
}  dispatch_pair;

extern sequence event_dispatch_pairs;

dispatch_pair make_dispatch_pair(Uint32 eventtype, SCM (* typeproc)(SDL_Event) , SCM(* dataproc)(SDL_Event) );
void add_dispatch_pair(dispatch_pair dp);
void remove_dispatch_pair(dispatch_pair dp);
SCM dispatch(SDL_Event e);

#endif
