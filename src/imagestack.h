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

/*
imagestack.h: Define imagestack structs and declare imagestack functions so images can be loaded only once.
*/
#ifndef IMGSTACK_H
#define IMGSTACK_H
#include "video.h"
#include <SDL/SDL.h>
#include <string.h>
#include "sequence.h"
#include "xalloc.h"
#include "path.h"
  
typedef struct
{
  SDL_Surface* data;
  char* filename;
} image;

extern sequence images;

image make_image (SDL_Surface* data, char* filename);
int  push_image_on_stack(char* filename);
int  find_image(char* filename);
void remove_image(char* filename);
#endif
