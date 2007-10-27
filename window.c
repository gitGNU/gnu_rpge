/*
Copyright Remco Bras 2007
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

#include "window.h"

window
create_window(unsigned int w, unsigned int h, unsigned int x, unsigned int y, char* spritefilename,unsigned int spritew,unsigned int spriteh)
{
  window win;
  win.width = w;
  win.height = h;
  win.x = x;
  win.y = y;
  win.imageindex = push_image_on_stack(spritefilename);
  win.tilew = spritew;
  win.tileh = spriteh;
  return win;
}

void
render_window(SDL_Surface* dest,window w)
{
  SDL_Rect clip;
  if(w.width <= w.tilew && w.height <= w.tileh)
    {
      clip.x = 0;
      clip.y = 0;
      clip.w = w.width;
      clip.h = w.height;
      return apply_surface(w.x,w.y,images.images[w.imageindex].data,dest,&clip);
    }
  else
    {
      SDL_Rect clip;
      clip.x = clip.y = 0;
      clip.w = w.tilew;
      clip.h = w.tileh;
      for(unsigned int remainingx = w.width, remainingy = w.height; remainingx > 0 && remainingy > 0;)
        {
          if(remainingx < w.tilew || remainingy < w.tileh)
            {
              if(remainingx < w.tilew && remainingy > w.tileh)
                {
                  //ok, so we have to loop until we have enough y done.. easy enough...
                  clip.w = remainingx;
                  while(remainingy > 0)
                    {
                      if(remainingy > w.tileh)
                        {
                          apply_surface(w.x+(w.width-remainingx),w.y+(w.height-remainingy),images.images[w.imageindex].data,dest,&clip);
                          remainingy -= w.tileh;
                        }
                      else
                        {
                          clip.h = remainingy;
                          return apply_surface(w.x+(w.width-remainingx),w.y+(w.height-remainingy),images.images[w.imageindex].data,dest,&clip);
                        }
                    }
                }
              else if(remainingx > w.tilew && remainingy < w.tileh)
                {
                  //pretty much a copy of that loop a single if higher up, need to rewrite this to cut the logic forest down a bit.
                  clip.h = remainingy;
                  while(remainingx > 0)
                    {
                      if(remainingx > w.tilew)
                        {
                          apply_surface(w.x+(w.width-remainingx),w.y+(w.height-remainingy),images.images[w.imageindex].data,dest,&clip);
                          remainingx -= w.tilew;
                        }
                      else
                        {
                          clip.w = remainingx;
                          return apply_surface(w.x+(w.width-remainingx),w.y+(w.height-remainingy),images.images[w.imageindex].data,dest,&clip);
                        }
                    }
                }
              else
                {
                  clip.h = remainingy;
                  clip.w = remainingx;
                  return apply_surface(w.x+(w.width-remainingx),w.y+(w.height-remainingy),images.images[w.imageindex].data,dest,&clip);
                }
            }
          else
            {
              apply_surface(w.x + (w.width-remainingx),w.y+(w.height-remainingy),images.images[w.imageindex].data,dest, &clip);
              remainingx -= w.tilew;
              remainingy -= w.tileh;
            }
        }
    }
}
