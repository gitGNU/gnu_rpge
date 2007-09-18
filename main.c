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

#include "main.h"

int
main (int argc, char **argv)
{
  SDL_Surface *screen;
  SDL_Surface *notimage = NULL;
  SDL_Event *event = malloc(sizeof(SDL_Event));
  SDL_Rect clip = {0,0,16,16};
  image imageblah;
  int next, now;
  int j;
  printf ("init: %i\n", SDL_Init (SDL_INIT_EVERYTHING));
  screen = SDL_SetVideoMode (800, 600, 32, SDL_HWSURFACE);
  if ( screen == NULL )
  {
    printf ("SDL_SetVideoMode failed: %s\n", SDL_GetError());
    return 1;
  }
  //initialize tilegrid
  tilegrid = init_tilegrid(40,30);
  printf("Debug: %d\n",tilegrid);
  SDL_WM_SetCaption ("RPGE", "RPGE");
  notimage = load_image ("test.png");
  apply_surface ( 0, 0, notimage, screen, &clip);
  if ( SDL_Flip( screen ) == -1 )
    {
      printf ("SDL_Flip failed: %s\n", SDL_GetError());
      return 1;
    }
  while (1)
    {
      now = SDL_GetTicks ();
      next = now + (int) (1000 / FRAMES_PER_SECOND);
      while (SDL_PollEvent (event))
	{
          switch (event->type)
	    {
	      case SDL_QUIT:
	        return 0;
	        break;
	    }
	}
      now = SDL_GetTicks ();
      if (now < next)
	SDL_Delay (next - now);
    }
  SDL_FreeSurface (notimage);
  free(event);
}
