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

#include "video.h"

void
render_mob(SDL_Surface* dest, mob mobby)
{
  SDL_Rect imageclippy = {mobby.animation * SPRITE_WIDTH, mobby.frame * SPRITE_HEIGHT, SPRITE_WIDTH, SPRITE_HEIGHT};
  apply_surface(mobby.x*TILE_WIDTH,mobby.y*TILE_HEIGHT,images.images[mobby.imgindex].data,dest,&imageclippy);
}

void
render_tile(SDL_Surface* dest, int x, int y, tile tily)
{
  apply_surface(x*TILE_WIDTH,y*TILE_HEIGHT,images.images[tily.tilesheetindex].data,dest,&tily.sheetclippinginfo);
}

void
render_tilegrid(SDL_Surface* dest, tile** grid, int width, int height)
{
  for(int i = 0; i < width; i++)
    {
      for(int j = 0; j < height; j++)
        {
          render_tile(dest,i,j,grid[i][j]);
        }
    }
}

SDL_Surface*
load_image (char* filename)
{
  SDL_Surface* loadedImage = IMG_Load ( filename );
  SDL_Surface* optimizedImage;
  if ( loadedImage != NULL )
    {
      optimizedImage = SDL_DisplayFormatAlpha(loadedImage);
      return optimizedImage;
    }
  else
    {
      fprintf (stderr,"Loading of %s failed: %s\n", filename, IMG_GetError());
      return NULL;
    }
}

void
apply_surface ( int x, int y, SDL_Surface* source, SDL_Surface* destination, SDL_Rect* clip)
{
    SDL_Rect offset;

    offset.x = x;
    offset.y = y;

    SDL_BlitSurface ( source, clip, destination, &offset );
}

void
render_screen(SDL_Surface* dest)
{
  SDL_Rect clip = {0,0,tilegrid_width*TILE_WIDTH,tilegrid_height*TILE_HEIGHT};
  apply_surface(0,0,tilegrid_layer,dest,&clip);
  for(int i = 0; i < mobs.size; i++)
    {
      render_mob(dest,mobs.mobs[i]);
    }
}