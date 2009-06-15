/*
Copyright Remco Bras 2007,2008
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
SDL_mutex* window_mutex;
S_CONVERTORS(window,WINDOW);

void 
init_windows()
{
  windows = sequence_init();
  window_mutex = SDL_CreateMutex();
}

window
create_window(unsigned int w, unsigned int h, unsigned int x, unsigned int y, char* spritefilename,SDL_Rect clip)
{
  window win;
  win.width = w;
  win.height = h;
  win.x = x;
  win.y = y;
  win.imageindex = push_image_on_stack(spritefilename);
  win.clip_rect = clip;
  return win;
}

void
render_window(SDL_Surface* dest,window w)
{
  SDL_Rect clip = w.clip_rect;
  unsigned int tilew = clip.w, tileh = clip.h;
  if(w.width <= tilew && w.height <= tileh)
    {
      clip.w = w.width;
      clip.h = w.height;
      apply_surface(w.x,w.y,((image*)images.data[w.imageindex].data)->data,dest,&clip);
      return;
    }
  else
    {
      unsigned int x = w.x,y,remainingy;
      for(unsigned int remainingx=w.width;remainingx > 0; )
        {
	  y = w.y;
	  remainingy = w.height;
          if(remainingx < clip.w)
            {
              clip.w = remainingx;
              remainingx = 0;
            }
          else
            {
              clip.w = tilew;
              remainingx -= tilew;
            }
          while(remainingy > 0)
            {
              if(remainingy < clip.h)
                {
                  clip.h = remainingy;
                  remainingy = 0;
                }
              else
                {
                  clip.h = tileh;
                  remainingy -= tileh;
                }
              apply_surface(x,y,((image*)images.data[w.imageindex].data)->data,dest,&clip);
              y+= tileh;
            }
          x += tilew;
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
         w1.clip_rect.x == w2.clip_rect.x &&
         w1.clip_rect.y == w2.clip_rect.y &&
         w1.clip_rect.w == w2.clip_rect.w &&
         w1.clip_rect.h == w2.clip_rect.h;
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
      SDL_mutexP(window_mutex);
      *((window*)windows.data[empty].data) = w;
      SDL_mutexV(window_mutex);
      return empty;
    }
  else
    {
      SDL_mutexP(window_mutex);
      int index = sequence_append(&windows,make_window_obj(w));
      SDL_mutexV(window_mutex);
      return index;
    }
}

void windowstack_removewindow(window w)
{
  int index = find_window(w);
  if(index == -1)
    return;
  else
    {
      SDL_mutexP(window_mutex);
      *((window*)windows.data[index].data) = empty_window();
      SDL_mutexV(window_mutex);
      return;
    }
}

void windowstack_remove(int index)
{
  SDL_mutexP(window_mutex);
  *((window*)windows.data[index].data) = empty_window();
  SDL_mutexV(window_mutex);
}

void render_windows(SDL_Surface* dest)
{
  for(int i=0;i<windows.objcount;i++)
    {
      render_window(dest,*((window*)windows.data[i].data));
    }
}

void
move_window(int index, unsigned x, unsigned y)
{
  window* w = windows.data[index].data;
  SDL_mutexP(window_mutex);
  w->x = x;
  w->y = y;
  SDL_mutexV(window_mutex);
}

void 
resize_window(int index, unsigned w, unsigned h)
{
  window* win = windows.data[index].data;
  SDL_mutexP(window_mutex);
  win->width = w;
  win->height = h;
  SDL_mutexV(window_mutex);
}
