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

#include "video.h"

void
render_mob_with_offsets(SDL_Surface* dest, mob mobby, int xoffset, int yoffset)
{
  if(mobby.imgindex == -1)
    return;
  SDL_Rect imageclippy = {mobby.animation * SPRITE_WIDTH, mobby.frame * SPRITE_HEIGHT, SPRITE_WIDTH, SPRITE_HEIGHT};  
  apply_surface(mobby.x+xoffset,mobby.y+yoffset,((image*)images.data[mobby.imgindex].data)->data,dest,&imageclippy);
}

void
render_mob(SDL_Surface* dest, mob mobby)
{
  render_mob_with_offsets(dest,mobby,0,0);
}

void
render_tile_with_offsets(SDL_Surface* dest,int x, int y, tile tily, int xoffset, int yoffset)
{
  apply_surface(x*TILE_WIDTH-xoffset,y*TILE_HEIGHT-yoffset,((image*)images.data[tily.tilesheetindex].data)->data,dest,&tily.sheetclippinginfo);
}

void
render_tile(SDL_Surface* dest, int x, int y, tile tily)
{
  render_tile_with_offsets(dest,x,y,tily,0,0);
}

void
render_tilegrid_with_offsets(SDL_Surface* dest, tile** grid, int width, int height,int xoffset, int yoffset)
{
 for(int i = 0; i < width; i++)
    {
      for(int j = 0; j < height; j++)
        {
          render_tile_with_offsets(dest,i,j,grid[i][j],xoffset,yoffset);
        }
    }
}

void
render_tilegrid(SDL_Surface* dest, tile** grid, int width, int height)
{
  render_tilegrid_with_offsets(dest,grid,width,height,0,0); 
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
render_text(SDL_Surface* dest, text t)
{
  unsigned int x = t.x, y = t.y;
  TTF_Font* fon = NULL;
  if(t.fontindex != -1)
    fon = ((font*)fonts.data[t.fontindex].data)->font;
  else
    return;
  int lineskip = TTF_FontLineSkip(fon);
  for(int i = 0; i < t.buffers.objcount; i++)
    {
      SDL_Surface* text = TTF_RenderText_Blended(fon, *((char**)t.buffers.data[i].data),t.color);
      apply_surface(x,y,text,dest,NULL);
      SDL_FreeSurface(text);
      y+=lineskip;
    }
}

void
render_texts(SDL_Surface* dest)
{
  for(int i = 0; i < texts.objcount; i++)
    {
      render_text(dest,*((text*)texts.data[i].data));
    }
}

void
render_screen(SDL_Surface* dest)
{
  int xoffset = get_camera_xoffset(global_camera,SCREEN_WIDTH),yoffset = get_camera_yoffset(global_camera,SCREEN_HEIGHT);
  SDL_FillRect(dest,NULL,SDL_MapRGB(dest->format,0,0,0));
  SDL_Rect clip = {0,0,main_grid.width*TILE_WIDTH,main_grid.height*TILE_HEIGHT};
  apply_surface(xoffset,yoffset,main_grid.imagebuffer,dest,&clip);
  for(int i = 0; i < mobs.objcount; i++)
    {
      render_mob_with_offsets(dest,*((mob*)mobs.data[i].data),xoffset,yoffset);
    }
  render_windows(dest);
  render_texts(dest);
}

