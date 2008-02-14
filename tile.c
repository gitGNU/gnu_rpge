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

#include "tile.h"

tilelayer main_grid;

tile 
make_tile(unsigned int tilesheet, SDL_Rect clipping, char blocking)
{
  tile t;
  t.tilesheetindex = tilesheet;
  t.sheetclippinginfo = clipping;
  t.blocking = blocking;
  return t;
}

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
