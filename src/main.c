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

#include "main.h"

int
exec_guile_shell (void *unused_arg)
{
  scm_init_guile();
  scm_shell(0,0);
  return 0;			//never reached, just here to please gcc.
}

SCM
get_keydown_symbol(SDL_Event e)
{
  return scm_from_locale_symbol("key-down");
}

SCM 
get_keysym_symbol(SDL_Event e)
{
  short sym = e.key.keysym.unicode;
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

void dispatch_event(SDL_Event e)
{
  /*TODO: do something or other to extract the relevant data from an event and get a proper symbol to shove into the type, then send all this off to the global eventstack.
  */
  /*For now, we'll just switch in a hardcoded, boring fashion. Technically, this needs some method of autodispatching and some separation into event.c, but the autodispatch and such need a good few more mailing list discussions to prevent creeping featurism, even though supporting every single scheme under the sun in some actually useful way could be good. Of course, this gets rather hairy organization-wise. */
  SCM c = dispatch(e);
  event ev = make_event(scm_car(c),scm_cdr(c));
  if(ev.type != SCM_EOL)
    eventstack_addevent(&global_usereventstack,ev);
}


int
main (int argc, char **argv)
{
  char* initfile = ".RPGE", option_char;
  int longopt_index = 0;
  struct option options[] = {
  {"version",0,NULL,'v'},
  {"help",0,NULL,'h'}
  };
  while((option_char = getopt_long(argc,argv,"vhf:",options,&longopt_index)) != -1)
    {
      switch(option_char)
        {
          case 'v':
            puts(VERSION_STRING);
            exit(EXIT_SUCCESS);
            break;
          case 'h':
            puts(HELP_STRING);
            exit(EXIT_SUCCESS);
            break;
          case 'f':
            initfile = strdup(optarg);
            break;
        }
    }
  SDL_Surface *screen;
  SDL_Event *event = xmalloc (sizeof (SDL_Event));
  int next, now;
  SDL_Init (SDL_INIT_EVERYTHING);
  TTF_Init ();
  screen = SDL_SetVideoMode (SCREEN_WIDTH, SCREEN_HEIGHT, 32, SDL_HWSURFACE);
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
  /*Define the load mutex as a recursive mutex. This is necessary for safe loading*/
  scm_c_define("load-mutex",scm_make_recursive_mutex());
  scm_c_define_gsubr ("create-mob", 4, 0, 0, guile_create_mob);
  scm_c_define_gsubr ("destroy-mob",1,0,0,guile_destroy_mob);
  scm_c_define_gsubr ("create-tile", 3, 0, 0, guile_create_tile);
  scm_c_define_gsubr ("set-tile", 4, 0, 0, guile_set_tile);
  scm_c_define_gsubr ("set-all-tiles", 2, 0, 0, guile_set_all_tiles);
  scm_c_define_gsubr ("move-mob", 4, 0, 0, guile_move_mob_all);
  scm_c_define_gsubr ("init-tilegrid",2,0,0,guile_make_tilegrid);
  scm_c_define_gsubr ("remove-tilegrid",1,0,0,guile_remove_grid);
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
  scm_c_define_gsubr ("open-font",2,0,0,guile_open_font);
  scm_c_define_gsubr ("close-font",1,0,0,guile_close_font);
  scm_c_define_gsubr ("make-text",7,0,0,guile_make_text);
  scm_c_define_gsubr ("destroy-text",1,0,0,guile_destroy_text);
  scm_c_define_gsubr ("add-mob-movement",4,0,0,guile_add_mob_movement);
  scm_c_define_gsubr ("get-camera-x",0,0,0,guile_get_camera_x);
  scm_c_define_gsubr ("get-camera-y",0,0,0,guile_get_camera_y);
  scm_c_define_gsubr ("set-camera-x",1,0,0,guile_set_camera_x);
  scm_c_define_gsubr ("set-camera-y",1,0,0,guile_set_camera_y);
  scm_c_define_gsubr ("open-mob-events",1,0,0,guile_open_mob_eventstack);
  scm_c_define_gsubr ("get-mob-event",2,0,0,guile_get_mob_event);
  scm_c_define_gsubr ("close-mob-events",2,0,0,guile_close_mob_eventstack);
  scm_c_define_gsubr ("safe-load",1,0,0,guile_safe_load);
  scm_c_define_gsubr ("set-mob-frame",3,0,0,guile_set_mob_frame);
  scm_c_define_gsubr ("get-window-coordinates",1,0,0,guile_get_window_coordinates);
  scm_c_define_gsubr ("get-window-dimensions",1,0,0,guile_get_window_dimensions);
  scm_c_define_gsubr ("move-window",2,0,0,guile_move_window);
  scm_c_define_gsubr ("resize-window",2,0,0,guile_resize_window);
  scm_c_define_gsubr ("move-text",2,0,0,guile_move_text);
  scm_c_define_gsubr ("get-text-coordinates",1,0,0,guile_get_text_coordinates);
  scm_c_define_gsubr ("get-text-lines",1,0,0,guile_get_text_line_list);
  SCM_TICK;
  global_usereventstack = eventstack_init();
  windows = images = mobs = argvs = fonts = tile_layers =  sequence_init();
  image_mutex = SDL_CreateMutex();
  mob_mutex = SDL_CreateMutex();
  window_mutex = SDL_CreateMutex();
  directives_init();
  paths_init();
  add_dispatch_pair(make_dispatch_pair(SDL_KEYDOWN,get_keydown_symbol,get_keysym_symbol));
  scm_gc_protect_object(global_userdata);
  exec_config_file(initfile);
  SDL_CreateThread (exec_guile_shell, 0);
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
                TTF_Quit();
                SDL_Quit();
	        return 0;
	        break;
              default:
                dispatch_event(*event);
	    }
	}
      move_mobs ();
      animate_mobs ();
      render_screen (screen);
      if (SDL_Flip (screen) == -1)
	{
	  fprintf (stderr, "RPGE:Cannot render frame, flip failure: %s\n",
		   SDL_GetError ());
	  return 1;
	}
      now = SDL_GetTicks ();
      if (now < next)
	SDL_Delay (next - now);
    }
  free (event);
}
