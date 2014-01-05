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

#ifndef VIDEO_INC_GUARD
#define VIDEO_INC_GUARD
#include <SDL/SDL.h>
#include <SDL/SDL_image.h>
#include <SDL/SDL_ttf.h>
#include "constants.h"
#include "config.h"
#include "mobs.h"
#include "tile.h"
#include "text.h"
#include "window.h"
#include "camera.h"

SDL_Surface* load_image (char* filename);
void apply_surface (int x, int y, SDL_Surface* source, SDL_Surface* destination, SDL_Rect* clip);
void render_screen(SDL_Surface* dest);
void render_tilegrid(SDL_Surface* dest, tile** grid, int width, int height);
void render_text(SDL_Surface* dest,text t);
#endif
