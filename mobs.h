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
  int x;
  int y;
  float xpixelalignment;
  float ypixelalignment;
  int imgindex;
  unsigned int animation;
  unsigned int frame;
  unsigned int targetframe;
  char animlooping;
  char resetonnext;
  int initialtimetonextframe;
  int timetonextframe;
  int xmoverate;
  int xmoveamount;
  int ymoverate;
  int ymoveamount;
} mob;

typedef struct
{
  unsigned int size;
  mob *mobs;
} mobstack;

extern mobstack mobs;

mob create_mob_using_sprite (unsigned int, unsigned int, char *);
int push_mob_on_array (mob);
void remove_mob(int);
void mob_set_animation(mob* m, unsigned int animation, unsigned int startframe, unsigned int targetframe, unsigned int framesperframe, char looping);
void  animate_mobs();
void  move_mobs();
void  mob_set_movement(mob* m, int xam, double xrate, int yam, double yrate);
void  mob_move_all (mob * m, int xtiles, int ytiles, int frames);
void  mob_stop_animation(mob* m);
#endif
