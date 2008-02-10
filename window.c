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

sequence windows;
convertors(window);

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
      return apply_surface(w.x,w.y,((image*)images.data[w.imageindex].data)->data,dest,&clip);
    }
  else
    {
      SDL_Rect clip;
      clip.x = clip.y = 0;
      clip.w = w.tilew;
      clip.h = w.tileh;
      unsigned int x = w.x,y,remainingy;
      for(unsigned int remainingx=w.width;remainingx > 0; )
        {
          y = w.y;
          remainingy = w.height;
          if(remainingx < w.tilew)
            {
              clip.w = remainingx;
              remainingx = 0;
            }
          else
            {
              clip.w = w.tilew;
              remainingx -= w.tilew;
            }
          while(remainingy > 0)
            {
              if(remainingy < w.tileh)
                {
                  clip.h = remainingy;
                  remainingy = 0;
                }
              else
                {
                  clip.h = w.tileh;
                  remainingy -= w.tileh;
                  }
              apply_surface(x,y,((image*)images.data[w.imageindex].data)->data,dest,&clip);
              y+= w.tileh;
            }
          x += w.tilew;
        }
    }
}

char windows_equalp(window w1, window w2)
{
  return w1.width == w2.width &&
         w1.height == w2.height &&
         w1.x == w2.x &&
         w1.y == w2.y &&
         w1.imageindex == w2.imageindex &&
         w1.tilew == w2.tilew &&
         w1.tileh == w2.tileh;
}

int find_window(window w)
{
  for(int i = 0; i< windows.objcount; i++)
    {
      if(windows_equalp(*((window*)windows.data[i].data),w))
        return i;
    }
  return -1;
}

window empty_window(void)
{
  window w;
  memset(&w,0,sizeof(window));
  return w;
}

int find_empty_window(void)
{
  return find_window(empty_window());
}

int windowstack_addwindow(window w)
{
  int empty = find_empty_window();
  if(empty != -1)
    {
      *((window*)windows.data[empty].data) = w;
      return empty;
    }
  else
    {
      sequence_append(&windows,make_window_obj(w));
    }
}

void windowstack_removewindow(window w)
{
  int index = find_window(w);
  if(index == -1)
    return;
  else
    {
      *((window*)windows.data[index].data) = empty_window();
      return;
    }
}

void windowstack_remove(int index)
{
  *((window*)windows.data[index].data) = empty_window();
}

void render_windows(SDL_Surface* dest)
{
  for(int i=0;i<windows.objcount;i++)
    {
      render_window(dest,*((window*)windows.data[i].data));
    }
}
