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
#include "config.h"
#include "event.h"
#include "tile.h"
#include "text.h"
#include <libguile.h>
#include <SDL/SDL.h>
#include "guile_mob.h"
#include "guile_camera.h"
#include "guile_window.h"
#include "guile_tile.h"
#include "guile_text.h"
#include "guile_global_event.h"
#include "guile_load.h"
#include "guile_config_file.h"
  

extern SCM global_userdata;   

/*Actually a SDL_USEREVENT code, but.. put here to decrease warnings*/
#define RELEASE_REPL_MUTEX 0
#define ACQUIRE_REPL_MUTEX 1

void argvs_init();
SCM guile_get_global_userdata(void);
SCM guile_set_global_userdata(SCM newdata);
SCM guile_run_repl();
SCM guile_stop_repl();

#endif
