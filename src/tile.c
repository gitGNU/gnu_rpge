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
tile.c: Implement the functions related to tiles and tilelayers. 
*/
#include "tile.h"

/*Declare convertor usage, so sed can find it and take care of the mess*/
inline object
                         make_tilelayer_obj(tilelayer foo)
                         {
                           object o;
                           o.data=xmalloc(sizeof(tilelayer));
                           o.typeinfo = TYPE_TILELAYER;
                           *((tilelayer*)o.data)=foo;
                           return o;
                         }
                         inline tilelayer
                         get_obj_tilelayer (object o)
                         {
                           return *((tilelayer*)o.data);
                         }

/*The sequence of current ready, loaded tilegrids (or tile_layers, whichever). The main idea here is to ensure that whatever links to the main grid is at any and all times 
  loaded. (Technically, the point is to A: be able to load the new grid immediately on transition, B: swap around tilegrids if necessary (a prereq of A, really) and C: make sure
  A carries over transitions).*/
sequence tile_layers;
/*We do need to know what world the user would like us to render*/
int maingrid_index;

tilelayer main_grid;

tile 
make_tile(unsigned int tilesheet, SDL_Rect clipping, char blocking)
{
  tile t;
  t.tilesheetindex = tilesheet;
  t.sheetclippinginfo = clipping;
  t.blocking = blocking;
  t.occupant = NULL;
  return t;
}

/*Note: The caller should handle the memory allocated here. Currently, there is only one tilegrid at all times, this can (and will) change in later versions, in which that property shall become significant.*/
tile**
init_tilegrid(unsigned int width,unsigned int height)
{
  tile** grid =  xmalloc(sizeof(tile*)*width);
  for(int i = 0; i < width; i++)
    {
      grid[i] = xmalloc(sizeof(tile)*height);
    }
  return grid;
}

tile**
tilegrid_replace_tile(tile** grid, unsigned int x, unsigned int y, tile replacement)
{
  grid[x][y]=replacement;
  return grid;
}

tile**
tilegrid_set_all_tiles(tile** grid, unsigned int gridwidth, unsigned int gridheight, tile replacement)
{
  for(int i = 0; i < gridwidth; i++)
    {
      for(int j = 0; j < gridheight; j++)
        {
          grid[i][j] = replacement;
        }
    }
  return grid;
}

/*These convenience functions exist merely to make life easier for users (and guile.c). How they should be rewritten in case we're dealing with any number of tilelayers is something to be seen later.*/
tile** set_tile(unsigned int x, unsigned int y, tile replacement)
{
  if(main_grid.tilegrid)
    {
      main_grid.tilegrid[x][y] = replacement;
      remake_tilegrid();
      return main_grid.tilegrid;
    }
  else
    return NULL;
}

tile** set_all_tiles(tile replacement)
{
  if(main_grid.tilegrid)
    {
      main_grid.tilegrid = tilegrid_set_all_tiles(main_grid.tilegrid,main_grid.width,main_grid.height,replacement);
      remake_tilegrid();
      return main_grid.tilegrid;
    }
  else
    return NULL;
}  

/*This one should be generalised to any tilelayer, but note that it renders the entire tilegrid in advance. This is an optimization hack, to avoid thousands of runtime blits every frame. */
SDL_Surface*
remake_tilegrid()
{
  SDL_Surface* display;
  if(!main_grid.imagebuffer)
    {
      display = SDL_GetVideoSurface();
      main_grid.imagebuffer = SDL_CreateRGBSurface(SDL_HWSURFACE,main_grid.width*TILE_WIDTH,main_grid.height*TILE_HEIGHT,display->format->BitsPerPixel,display->format->Rmask,display->format->Gmask,display->format->Bmask,display->format->Amask);
    }
  else
    SDL_FillRect(main_grid.imagebuffer,NULL,SDL_MapRGB(main_grid.imagebuffer->format,0,0,0));
  render_tilegrid(main_grid.imagebuffer,main_grid.tilegrid,main_grid.width,main_grid.height);
  return main_grid.imagebuffer;
}

inline char 
occupied(int tilex, int tiley)
{
  return main_grid.tilegrid[tilex][tiley].occupant != NULL;
}

inline void 
set_occupant(int tilex, int tiley, mob* new_occupant)
{
  main_grid.tilegrid[tilex][tiley].occupant = new_occupant;
}

inline mob* 
get_occupant(int tilex, int tiley)
{
  return main_grid.tilegrid[tilex][tiley].occupant;
}

inline void
reset_occupant(int tilex, int tiley)
{
  main_grid.tilegrid[tilex][tiley].occupant = NULL;
}
