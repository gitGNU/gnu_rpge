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

#ifndef GUILE_H
#define GUILE_H

#include "event.h"
#include "tile.h"
#include "window.h"
#include "text.h"
#include <libguile.h>
#include <SDL/SDL.h>
#include "guile_mob.h"
#include "guile_camera.h"
  
typedef struct
{
  Uint32 threadid;
  SCM argv;
}  thread_argv;

extern SCM global_userdata;   
extern sequence argvs;

/*Actually a SDL_USEREVENT code, but.. put here to decrease warnings*/
#define RELEASE_REPL_MUTEX 0
#define ACQUIRE_REPL_MUTEX 1

void argvs_init();
SCM guile_create_tile(SCM sprite, SCM partclip, SCM blocking);
SCM guile_set_all_tiles(SCM grid,SCM tile);
SCM guile_set_tile(SCM grid,SCM x, SCM y, SCM tile);
SCM guile_make_tilegrid(SCM width, SCM height);
SCM guile_remove_grid(SCM index);
SCM guile_make_window (SCM width, SCM height, SCM x, SCM y, SCM filename,SCM  rect);
SCM guile_destroy_window(SCM index);
SCM guile_open_global_eventstack(SCM flag);
SCM guile_close_global_eventstack(SCM userindex);
SCM guile_get_global_event(SCM userindex);
SCM guile_get_global_userdata(void);
SCM guile_set_global_userdata(SCM newdata);
void guile_exec_script_with_argv(char* filename, SCM argv);
SCM guile_API_exec_script_with_argv(SCM filename, SCM argv);
SCM guile_get_argv();
SCM guile_open_font(SCM filename, SCM size);
SCM guile_close_font(SCM index);
SCM guile_make_text(SCM x, SCM y, SCM string, SCM font, SCM red, SCM green, SCM blue);
SCM guile_destroy_text(SCM textindex);
SCM scm_c_safe_load(char* filename);
SCM guile_safe_load(SCM filename);
SCM guile_get_window_coordinates(SCM index);
SCM guile_get_window_dimensions(SCM index);
SCM guile_move_window(SCM index, SCM point);
SCM guile_resize_window(SCM index, SCM dimensions);
SCM guile_move_text(SCM index, SCM point);
SCM guile_get_text_coordinates(SCM index);
SCM guile_get_text_line_list(SCM index);
SCM guile_set_main_grid(SCM g);
SCM guile_get_main_grid();
SCM guile_run_repl();
SCM guile_stop_repl();

#endif
