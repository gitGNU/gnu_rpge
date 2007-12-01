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

typedef struct
{
  unsigned int width, height,x, y;
  int imageindex; //note: this baby should be automatically... err... I dunno.. scaled? tiled? manipulated in some strange way? to fit the window. Heck.. for now, I'll go with "tiled".
  unsigned int tilew, tileh;
} window;

typedef struct
{
  window* windows;
  unsigned int size;
} windowstack; /*discussion and testing needed of these versus generic stacks*/

extern windowstack windows;

window  create_window(unsigned int w, unsigned int h, unsigned int x, unsigned int y, char* spritefilename,unsigned int spritew,unsigned int spriteh);
void render_window(SDL_Surface* dest,window w);
windowstack init_windowstack(void);
void clear_windowstack(void);
int windowstack_addwindow(window w);
void windowstack_remove(int index);
void windowstack_removewindow(window w);
void render_windows(SDL_Surface* dest);

#endif
