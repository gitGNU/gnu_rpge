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

#ifndef WINDOW_H
#define WINDOW_H

#include "video.h"
#include "imagestack.h"
#include <SDL/SDL.h>

typedef struct
{
  unsigned int width, height,x, y;
  int imageindex; 
  unsigned int tilew, tileh;
} window;

extern sequence windows;

void init_windows();
window  create_window(unsigned int w, unsigned int h, unsigned int x, unsigned int y, char* spritefilename,unsigned int spritew,unsigned int spriteh);
void render_window(SDL_Surface* dest,window w);
void clear_windowstack(void);
int windowstack_addwindow(window w);
void windowstack_remove(int index);
void windowstack_removewindow(window w);
void render_windows(SDL_Surface* dest);
window get_obj_window(object o);
object make_window_obj(window w);
void move_window(int index, unsigned x, unsigned y);
void resize_window(int index, unsigned w, unsigned h);

#endif
