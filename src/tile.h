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

/*
 tile.h: Define the tile and tilelayer structures, declare tile usage procedures.
*/
#ifndef TILE_H
#define TILE_H
#include "config.h"
#include <SDL/SDL.h>
#include "xalloc.h"
#include "mobs.h"
#include "sequence.h"

/*These are defined such that they match individual bits, which conveniently also work properly when added. The former property is great for C, the latter is good for GUILE. This does put an upper bound of approximately 32 on our number of allowed variations of blocking, but needing those should be rare.*/
#define BLOCK_LEFT 0x4
#define BLOCK_RIGHT 0x8
#define BLOCK_UP 0x10
#define BLOCK_DOWN 0x20
#define BLOCK_ALL BLOCK_LEFT | BLOCK_RIGHT | BLOCK_UP | BLOCK_DOWN
#define BLOCK_NONE 0x0

typedef struct
{
  int tilesheetindex; 
  SDL_Rect sheetclippinginfo;
  char blocking; 
  mob* occupant; /*This is a pointer to simplify matters*/
} tile;

typedef struct
{
  int index;
  int count;
}  imagecounter;

typedef struct
{
  tile** tilegrid;
  int height;
  int width;
  SDL_Surface* imagebuffer;
  sequence imagecounts;
} tilelayer;

extern sequence tile_layers;
extern int maingrid_index;

#define MAIN_GRID ((tilelayer*)tile_layers.data[maingrid_index].data)

void init_tiles();
tile make_tile(unsigned int tilesheet, SDL_Rect clipping, char blocking);
tile** init_tilegrid(unsigned int width, unsigned int height);
tile** set_all_tiles(int gridid,tile replacement);
tile** set_tile(int gridid,unsigned int x, unsigned int y, tile replacement);
char occupied(int tilex, int tiley,int grid);
void set_occupant(int tilex, int tiley, int grid, mob* new_occupant);
mob* get_occupant(int tilex, int tiley, int grid);
void reset_occupant(int tilex, int tiley,int grid);
int add_tilegrid(tilelayer grid);
void remove_grid_at(int index);
void set_maingrid_index(int ind);

/*Includes which depend on this file*/
#include "video.h"
#endif
