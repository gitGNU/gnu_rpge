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

#include "main.h"
#include "event.h"
#include "mobs.h"
#include "tile.h"
#include "window.h"
#include <libguile.h>

SCM guile_create_mob(SCM,SCM,SCM);
SCM tile_to_list(tile);
tile list_to_tile(SCM);
SCM guile_create_tile(SCM sprite, SCM partclip, SCM blocking);
SCM guile_set_all_tiles(SCM tile);
SCM guile_set_tile(SCM x, SCM y, SCM tile);
SCM guile_move_mob_all(SCM mobindex, SCM tilecountx, SCM tilecounty, SCM frametotal);
SCM guile_init_tilegrid(SCM width, SCM height);
SCM guile_set_mob_animation(SCM mobindex, SCM animation, SCM start, SCM targetframe, SCM framesbetween, SCM loop);
SCM guile_stop_mob_animation(SCM mobindex);
SCM guile_destroy_mob(SCM mobindex);
SCM guile_make_window (SCM width, SCM height, SCM x, SCM y, SCM filename,SCM tilew, SCM tileh);
SCM guile_destroy_window(SCM index);
SCM guile_open_global_eventstack();
SCM guile_close_global_eventstack(SCM userindex);
SCM guile_get_global_event(SCM userindex);
SCM guile_set_mob_userdata(SCM index, SCM newdata);
SCM guile_get_mob_userdata(SCM index);
SCM guile_get_global_userdata(void);
SCM guile_set_global_userdata(SCM newdata);
