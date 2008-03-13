/*
Copyright Remco Bras and Michael de Lang 2007,2008.
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

#define VERSION_STRING "GNU RPGE 0.0.1\n\
Copyright (C) 2008 Remco Bras\n\
License GPLv3+: GNU GPL version 3 or later <http://gnu.org/licenses/gpl.html>\n\
This is free software: you are free to change and redistribute it.\n\
There is NO WARRANTY, to the extent permitted by law.\n\
This program is part of the GNU project, released under the aegis of GNU."
#define HELP_STRING "Usage: RPGE <options>"
#define EXIT_SUCCESS 0
#define EXIT_FAILURE -1
  
  
#include <SDL/SDL.h>
#include <SDL/SDL_ttf.h>
#include <SDL/SDL_image.h>
#include <libguile.h>
#include <getopt.h>
#include "xalloc.h"
#include "tile.h"
#include "video.h"
#include "mobs.h"
#include "imagestack.h"
#include "constants.h"
#include "guile.h"
#include "event.h"
#include "window.h"
#include "text.h"
#include "dispatch.h"
#include "config_file.h"

extern SCM global_userdata; //There's an interesting reason for this one, see nonexistent docs.
#endif
