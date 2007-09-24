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
  mob mobby;
  SDL_Event *event = malloc(sizeof(SDL_Event));
  SDL_Rect clip = {0,0,16,16};
  SDL_Rect clip2 = {0,0,800,640};
  tile tiliditile;
  int next, now;
  int mobindex;
  SDL_Init (SDL_INIT_EVERYTHING);
  screen = SDL_SetVideoMode (800, 640, 32, SDL_HWSURFACE);
  if ( screen == NULL )
  {
    fprintf (stderr,"SDL_SetVideoMode failed: %s\n", SDL_GetError());
    return 1;
  }
  tiliditile = make_tile(push_image_on_stack("test.png"),clip,BLOCK_NONE);
  //initialize tilegrid
  tilegrid = init_tilegrid(50,40);
  tilegrid = tilegrid_set_all_tiles(tilegrid,50,40,tiliditile);
  tilegrid_width = 50;
  tilegrid_height = 40;
  remake_tilegrid();
  SDL_WM_SetCaption ("RPGE", "RPGE");
  //Yeah, I know this is lame, but I needed SOMETHING...
  mobby = create_mob_using_sprite(0,0,"test_sprite.png");
  //testing...again
  mobindex = push_mob_on_array(mobby);
  mob_set_animation(&(mobs.mobs[mobindex]),0,0,9,40,1);
  mob_set_movement(&(mobs.mobs[mobindex]),800,1,640,1);
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
      move_mobs();
      animate_mobs();
      if(mobs.mobs[0].x > 800)
        mobs.mobs[0].x = 0;
      if(mobs.mobs[0].y > 640)
        mobs.mobs[0].y = 0;
      mob_set_movement(&(mobs.mobs[mobindex]),500,1,300,1);
      render_screen(screen);
      if ( SDL_Flip( screen ) == -1 )
        {
          fprintf (stderr, "Cannot render frame, flip failure: %s\n", SDL_GetError());
          return 1;
        }
      now = SDL_GetTicks ();
      if (now < next)
	SDL_Delay (next - now);
    }
  free(event);
}
