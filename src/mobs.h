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
#ifndef MOBS_H
#define MOBS_H
#include <SDL/SDL.h>
#include <stdlib.h>
#include "config.h"
#include <libguile.h>
#include "sequence.h"
#include "constants.h"
#include "event.h"

typedef struct
{
  int x;
  int y;
  int grid;
  float xpixelalignment; /*Unused as of right about now, but intended for usage in 'offsetting' sprites from the center of their assigned tile*/
  float ypixelalignment; 
  int imgindex;
  unsigned int animation;
  unsigned int frame;
  unsigned int targetframe;
  char animlooping; /*Low-hanging optimization fruit here*/
  char resetonnext;
  int initialtimetonextframe;
  int timetonextframe;
  int xmoverate;
  int xmoveamount;
  int ymoverate;
  int ymoveamount;
  SCM userdata;
  sequence move_descriptors;
  eventstack events; /*For events limited to this mob only*/
} mob;

typedef struct
{
  int tilex, tiley, frames;
}  move_descriptor;

extern sequence mobs;

void mobs_init(void);
mob create_mob_using_sprite (unsigned int, unsigned int,unsigned grid, char *);
int push_mob_on_array (mob);
void remove_mob(int);
void mob_set_animation_by_index(int ind, unsigned int animation, unsigned int startframe, unsigned int targetframe, unsigned int framesperframe, char looping);
void animate_mobs();
void move_mobs();
void mob_move_all_by_index(int ind, int xtiles, int ytiles, int frames);
void mob_add_movement_by_index(int ind, int xtiles, int ytiles, int frames);
void mob_stop_animation_by_index(int ind);
void set_mob_frame_by_index(int ind,unsigned int animation,unsigned int framenum);
void set_mob_userdata_by_index(int ind, SCM newdata);
char set_mob_position_by_index(int ind, int x, int y, int g);
mob* get_mob_by_index(int ind);
void stop_mob_movement_by_index(int ind);

/*Self-dependent include*/
#include "tile.h"
#endif
