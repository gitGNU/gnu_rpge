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

#define VERSION_STRING "RPGE 0.0.1\nCopyright (C) 2008 Remco Bras\nLicense GPLv3+: GNU GPL version 3 or later <http://gnu.org/licenses/gpl.html>\nThis is free software: you are free to change and redistribute it.\nThere is NO WARRANTY, to the extent permitted by law."
#define HELP_STRING "Usage: RPGE <options>"

#include <SDL/SDL.h>
#include <SDL/SDL_ttf.h>
#include <SDL/SDL_image.h>
#include <libguile.h>
#include <getopt.h>
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

extern eventstack global_usereventstack; //holds all generic game events, with the minor exception of quitting events.
extern SCM global_userdata; //There's an interesting reason for this one, see nonexistent docs.
#endif
