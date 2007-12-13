/*
Copyright Remco Bras and Michael de Lang 2007.
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

#ifndef MAIN_INC_GUARD
#define MAIN_INC_GUARD

#include <SDL/SDL.h>
#include <SDL/SDL_ttf.h>
#include <SDL/SDL_image.h>
#include <libguile.h>
#include "tile.h"
#include "video.h"
#include "mobs.h"
#include "imagestack.h"
#include "constants.h"
#include "guile.h"
#include "event.h"
#include "window.h"
#include "text.h"


extern eventstack global_usereventstack; //holds all generic game events, with the minor exception of quitting events.
extern SCM global_userdata; //There's an interesting reason for this one, see nonexistent docs.
#endif
