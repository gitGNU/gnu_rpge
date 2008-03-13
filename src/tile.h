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
 tile.h: Define the tile and tilelayer structures, declare tile usage procedures.
*/
#ifndef TILE_H
#define TILE_H

#include <SDL/SDL.h>
#include "xalloc.h"
#include "mobs.h"

/*These are defined such that they match individual bits, which conveniently also work properly when added. The former property is great for C, the latter is good for GUILE. This does put an upper bound of approximately 32 on our number of allowed variations of blocking, but needing those should be rare.*/
#define BLOCK_LEFT 0x4
#define BLOCK_RIGHT 0x8
#define BLOCK_UP 0x10
#define BLOCK_DOWN 0x20
#define BLOCK_ALL_DIRECTIONS BLOCK_LEFT | BLOCK_RIGHT | BLOCK_UP | BLOCK_DOWN
#define BLOCK_NONE 0x0
#define BLOCK_GROUND 0x1
#define BLOCK_AIR 0x2
#define BLOCK_ALL BLOCK_GROUND | BLOCK_AIR
#define BLOCK_ALL_UNDIRECTIONAL BLOCK_ALL | BLOCK_ALL_DIRECTIONS

typedef struct
{
  int tilesheetindex; //in the as-of-yet barely functional global imagestack
  SDL_Rect sheetclippinginfo;//the part of the tilesheet containing the actual tile
  char blocking; 
  mob* occupant; /*This is a pointer to simplify matters*/
} tile;

typedef struct
{
  tile** tilegrid;
  int height;
  int width;
  SDL_Surface* imagebuffer;
} tilelayer;

extern tilelayer main_grid;

SDL_Surface* remake_tilegrid();
tile make_tile(unsigned int tilesheet, SDL_Rect clipping, char blocking);
tile** init_tilegrid(unsigned int width, unsigned int height);
tile** tilegrid_replace_tile(tile** grid, unsigned int x, unsigned int y, tile replacement);
tile** tilegrid_set_all_tiles(tile** grid, unsigned int gridwidth, unsigned int gridheight, tile replacement);
tile** set_all_tiles(tile replacement);
tile** set_tile(unsigned int x, unsigned int y, tile replacement);
char occupied(int tilex, int tiley);
void set_occupant(int tilex, int tiley, mob* new_occupant);
mob* get_occupant(int tilex, int tiley);
void reset_occupant(int tilex, int tiley);

/*Includes which depend on this file*/
#include "video.h"
#endif
