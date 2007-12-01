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

eventstack global_usereventstack;
SCM global_userdata = SCM_EOL;

int
exec_guile_shell (void *unused_arg)
{
  scm_init_guile();
  scm_shell(0,0);
  return 0;			//never reached, just here to please gcc.
}

SCM
get_type(SDL_Event e)
{
  switch(e.type)
    {
      case SDL_KEYDOWN:
        return scm_from_locale_symbol("key-down");
        break;
      case SDL_KEYUP:
        return scm_from_locale_symbol("key-up");
        break;
      default: 
        return SCM_EOL;
    }
}

SCM 
get_keysym_symbol(SDL_keysym ks)
{
  short sym = ks.unicode;
  char printable;
  if(!sym)
    return SCM_EOL; /*Return NIL for non-printable (control) chars*/
  else
    {
      if(sym & 0xFF80)/*Non-ASCII char, ignore for now*/
        return SCM_EOL;
      else  /*ASCII char*/
        {
          printable = sym & 0x7F;
          switch(printable)
            {
              case '\n':
                return scm_from_locale_symbol("newline");
                break;
              case '\t':
                return scm_from_locale_symbol("tab");
                break;
              case ' ':
                return scm_from_locale_symbol("space");
                break;
              default:
                return scm_from_locale_symboln(&printable,1);
                break;
            }
        }
    }
}

SCM
get_data(SDL_Event e)
{
  switch(e.type)
    {
      case SDL_KEYDOWN:
      case SDL_KEYUP:
        return get_keysym_symbol(e.key.keysym);
        break;
      default:
        return SCM_EOL;
    }
}

void dispatch(SDL_Event e)
{
  /*TODO: do something or other to extract the relevant data from an event and get a proper symbol to shove into the type, then send all this off to the global eventstack.
  */
  /*For now, we'll just switch in a hardcoded, boring fashion. Technically, this needs some method of autodispatching and some separation into event.c, but the autodispatch and such need a good few more mailing list discussions to prevent creeping featurism, even though supporting every single scheme under the sun in some actually useful way could be good. Of course, this gets rather hairy organization-wise. */
  event ev = make_event(get_type(e),get_data(e));
  if(ev.type != SCM_EOL)
    eventstack_addevent(&global_usereventstack,ev);
}


int
main (int argc, char **argv)
{
  SDL_Surface *screen;
  SDL_Event *event = malloc (sizeof (SDL_Event));
  int next, now;
  SDL_Init (SDL_INIT_EVERYTHING);
  screen = SDL_SetVideoMode (800, 640, 32, SDL_HWSURFACE);
  if (screen == NULL)
    {
      fprintf (stderr, "SDL_SetVideoMode failed: %s\n", SDL_GetError ());
      return 1;
    }
  SDL_WM_SetCaption ("RPGE", "RPGE");
  /*
    Enable UNICODE conversion for keysyms so we can map the ASCII characters to their relevant descriptions, provided they are not equal to a few special chars.
    This does have some overhead according to the docs, so we might want to come up with a different, possibly faster scheme to take care of this later.
  */
  SDL_EnableUNICODE(1);
  scm_init_guile ();
  SDL_CreateThread (exec_guile_shell, 0);
  scm_c_define_gsubr ("create-mob", 3, 0, 0, guile_create_mob);
  scm_c_define_gsubr ("destroy-mob",1,0,0,guile_destroy_mob);
  scm_c_define_gsubr ("create-tile", 3, 0, 0, guile_create_tile);
  scm_c_define_gsubr ("set-tile", 3, 0, 0, guile_set_tile);
  scm_c_define_gsubr ("set-all-tiles", 1, 0, 0, guile_set_all_tiles);
  scm_c_define_gsubr ("move-mob", 4, 0, 0, guile_move_mob_all);
  scm_c_define_gsubr ("init-tilegrid",2,0,0,guile_init_tilegrid);
  scm_c_define_gsubr ("set-mob-animation",6,0,0,guile_set_mob_animation);
  scm_c_define_gsubr ("stop-mob-animation",1,0,0,guile_stop_mob_animation);
  scm_c_define_gsubr ("create-window",7,0,0,guile_make_window);
  scm_c_define_gsubr ("remove-window",1,0,0,guile_destroy_window);
  scm_c_define_gsubr ("open-global-events",0,0,0,guile_open_global_eventstack);
  scm_c_define_gsubr ("close-global-events",1,0,0,guile_close_global_eventstack);
  scm_c_define_gsubr ("get-global-event",1,0,0,guile_get_global_event);
  scm_c_define_gsubr ("get-mob-data",1,0,0,guile_get_mob_userdata);
  scm_c_define_gsubr ("set-mob-data",2,0,0,guile_set_mob_userdata);
  scm_c_define_gsubr ("get-global-data",0,0,0,guile_get_global_userdata);
  scm_c_define_gsubr ("set-global-data",1,0,0,guile_set_global_userdata);
  scm_c_define_gsubr ("load-with-argv",2,0,0,guile_API_exec_script_with_argv);
  scm_c_define_gsubr ("get-argv",0,0,0,guile_get_argv);
  scm_c_primitive_load ("table.guile");
  scm_c_primitive_load ("utils.guile");
  global_usereventstack = eventstack_init();
  windows = images = mobs = argvs = sequence_init();
  while (1)
    {
      SCM_TICK;
      now = SDL_GetTicks ();
      next = now + (int) (1000 / FRAMES_PER_SECOND);
      while (SDL_PollEvent (event))
	{
	  switch (event->type) 
            {
	      case SDL_QUIT:
	        return 0;
	        break;
              default:
                dispatch(*event);
	    }
	}
      move_mobs ();
      animate_mobs ();
      render_screen (screen);
      render_windows(screen);
      if (SDL_Flip (screen) == -1)
	{
	  fprintf (stderr, "Cannot render frame, flip failure: %s\n",
		   SDL_GetError ());
	  return 1;
	}
      now = SDL_GetTicks ();
      if (now < next)
	SDL_Delay (next - now);
    }
  free (event);
}
