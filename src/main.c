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

/*
  Used to signal whether the REPL can run. 
  While unlocked, the REPL may run (and will 
  acquire the lock, evaluate an expression and release the lock). 
*/
static SDL_mutex* repl_signal; 

/*
  Execute the repl.
  Executed by SDL_CreateThread from the initialization code in the rest
  of this file. 
  Communicates with main primarily through the repl-signal mutex.
*/
void*
guile_shell_main(void* unused_arg)
{
  /*Like top-repl (see guile sources) called by scm_shell, evaluate our code in the guile-user module.*/
  scm_set_current_module(scm_c_resolve_module("guile-user"));
  /*Prepare some procedures to deal with guile-level errors*/
  SCM evaluator = scm_c_eval_string("(lambda () (let ((res (primitive-eval (read (current-input-port))))) (if (unspecified? res) '() (if (eof-object? res) (quit) (begin (display res) (newline))))))");
  SCM handler = scm_c_eval_string("(lambda (type . args) (if (eq? type 'quit) (exit) (format (current-output-port) \"ERROR: ~a, with: ~a\" type args)))");
  /*Horribly inefficient*/
  while(1)
    {
      SCM_TICK;
      SDL_mutexP(repl_signal);
      scm_simple_format(scm_current_output_port(),scm_from_locale_string(PROMPT),SCM_EOL);
      scm_catch(SCM_BOOL_T,evaluator,handler);
      SDL_mutexV(repl_signal);
    }
  return NULL;
}
int
exec_guile_shell (void *unused_arg)
{
  /*
    This way to do things is used since it's more portable,
    less ugly and more future-proof than scm_init_guile.
  */
  scm_with_guile(guile_shell_main,NULL);
  return 0;
}

SCM
get_keydown_symbol(SDL_Event e)
{
  return scm_from_locale_symbol("key-down");
}

/*
  To do: Fix this.
  The problem with this code is that it relies on UNICODE conversion,
  which SDL doesn't do at all for keyup events.
  Said conversion also incurs an overhead and SDL has a function
  to get the name of a keysym anyway.
  The added benefit of doing so it that it also works for
  things like left arrow keys without lots of special-purpose
  silliness.
*/
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

static void *
inner_dispatch_event(void* arg)
{
  SDL_Event e = *(SDL_Event*)arg;
  SCM c = dispatch(e);
  event ev = make_event(scm_car(c),scm_cdr(c));
  if(ev.type != SCM_EOL)
    eventstack_addevent(&global_usereventstack,ev);
  return NULL;
}

void dispatch_event(SDL_Event e)
{
  scm_with_guile(inner_dispatch_event,&e);
}

static inline void*
init_scm(void* unused)
{
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
  scm_c_define_gsubr ("create-window",6,0,0,guile_make_window);
  scm_c_define_gsubr ("remove-window",1,0,0,guile_destroy_window);
  scm_c_define_gsubr ("open-global-events",1,0,0,guile_open_global_eventstack);
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
  scm_c_define_gsubr ("stop-mob-movement",1,0,0,guile_stop_mob_movement);
  scm_c_define_gsubr ("get-camera-x",0,0,0,guile_get_camera_x);
  scm_c_define_gsubr ("get-camera-y",0,0,0,guile_get_camera_y);
  scm_c_define_gsubr ("set-camera-x",1,0,0,guile_set_camera_x);
  scm_c_define_gsubr ("set-camera-y",1,0,0,guile_set_camera_y);
  scm_c_define_gsubr ("open-mob-events",2,0,0,guile_open_mob_eventstack);
  scm_c_define_gsubr ("get-mob-event",2,0,0,guile_get_mob_event);
  scm_c_define_gsubr ("close-mob-events",2,0,0,guile_close_mob_eventstack);
  scm_c_define_gsubr ("safe-load",1,0,0,guile_safe_load);
  scm_c_define_gsubr ("set-mob-frame",3,0,0,guile_set_mob_frame);
  scm_c_define_gsubr ("get-mob-frame",1,0,0,guile_get_mob_frame);
  scm_c_define_gsubr ("primitive-get-window-coordinates",1,0,0,guile_get_window_coordinates);
  scm_c_define_gsubr ("primitive-get-window-dimensions",1,0,0,guile_get_window_dimensions);
  scm_c_define_gsubr ("primitive-move-window",2,0,0,guile_move_window);
  scm_c_define_gsubr ("primitive-resize-window",2,0,0,guile_resize_window);
  scm_c_define_gsubr ("move-text",2,0,0,guile_move_text);
  scm_c_define_gsubr ("get-text-coordinates",1,0,0,guile_get_text_coordinates);
  scm_c_define_gsubr ("get-text-lines",1,0,0,guile_get_text_line_list);
  scm_c_define_gsubr ("set-main-grid",1,0,0,guile_set_main_grid);
  scm_c_define_gsubr ("get-main-grid",0,0,0,guile_get_main_grid);
  scm_c_define_gsubr ("run-repl",0,0,0,guile_run_repl);
  scm_c_define_gsubr ("stop-repl",0,0,0,guile_stop_repl);
  scm_c_define_gsubr ("get-mob-position",1,0,0,guile_get_mob_position);
  scm_c_define_gsubr ("set-mob-position",2,0,0,guile_set_mob_position);
  scm_c_define_gsubr ("get-text-color",1,0,0,guile_get_text_color);
  scm_c_define_gsubr ("set-text-color!",2,0,0,guile_set_text_color);
  scm_c_define_gsubr ("get-text-font",1,0,0,guile_get_text_font);
  scm_c_define_gsubr ("set-text-font!",2,0,0,guile_set_text_font);
  scm_gc_protect_object(global_userdata);
  return NULL;
}

static inline void
init()
{
  global_usereventstack = eventstack_init();
  argvs_init();
  text_rendering_init();
  dispatch_init();
  mobs_init();
  images_init();
  init_tiles();
  init_windows();
  directives_init();
  paths_init();
}

/*
  A tiny bit of glue to hook exec_config_file into guile mode
  stuff without affecting everything else.
*/
static
void* run_config_file(void* filename)
{
  exec_config_file(filename);
  return NULL;
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
  SDL_WM_SetCaption ("rpge", "rpge");
  /*
    Enable UNICODE conversion for keysyms so we can map the ASCII characters to their relevant descriptions, provided they are not equal to a few special chars.
    This does have some overhead according to the docs, so we might want to come up with a different, possibly faster scheme to take care of this later.
  */
  SDL_EnableUNICODE(1);
  scm_with_guile(init_scm,NULL);
  init();
  add_dispatch_pair(make_dispatch_pair(SDL_KEYDOWN,get_keydown_symbol,get_keysym_symbol));
  scm_with_guile(run_config_file,initfile);
  /*Set up REPL signal and run REPL thread*/
  repl_signal = SDL_CreateMutex();
  SDL_mutexP(repl_signal);
  SDL_CreateThread (exec_guile_shell, 0);
  while (1)
    {
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
              case SDL_USEREVENT:
		if(event->user.code == RELEASE_REPL_MUTEX)
		  SDL_mutexV(repl_signal);
		else if(event->user.code == ACQUIRE_REPL_MUTEX)
		  SDL_mutexP(repl_signal);
		break;
              default:
                dispatch_event(*event);
	    }
	}
      /*
	Run move_mobs, which needs guile mode now.
      */
      move_mobs ();
      animate_mobs ();  
      render_screen (screen);
      if (SDL_Flip (screen) == -1)
	{
	  fprintf (stderr, "rpge:Cannot render frame, flip failure: %s\n",
		   SDL_GetError ());
	  return 1;
	}
      now = SDL_GetTicks ();
      if (now < next)
	SDL_Delay (next - now);
    }
  free (event);
}
