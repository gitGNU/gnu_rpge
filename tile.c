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

tile** tilegrid;
int tilegrid_width;
int tilegrid_height;

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
  tile** grid =  malloc(sizeof(tile*)*width);
  for(int i = 0; i < width; i++)
    {
      grid[i] = malloc(sizeof(tile)*height);
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
