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
#ifndef MOBS_H
#define MOBS_H
#include <SDL/SDL.h>
#include <stdlib.h>
#include "video.h"
#include "imagestack.h"

typedef struct
{
  unsigned int x;
  unsigned int y;
  float xpixelalignment;
  float ypixelalignment;
  int imgindex;
  unsigned int animation;
  unsigned int frame;
  unsigned int targetframe;
  unsigned int targetx;
  unsigned int targety;
  float animrate;
  float animtime;
  float xmoverate;
  float xmovetime;
  float ymoverate;
  float ymovetime;
} mob;

typedef struct
{
  unsigned int size;
  mob *mobs;
} mobstack;

extern mobstack mobs;

mob create_mob_using_sprite (unsigned int, unsigned int, char *);
int push_mob_on_array (mob);
void remove_mob(mob);

#endif
