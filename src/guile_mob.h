/*
Copyright Remco Bras 2008
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

#ifndef GUILE_MOB_H
#define GUILE_MOB_H
#include "config.h"
#include <libguile.h>
#include "mobs.h"
#include "constants.h"

SCM guile_create_mob(SCM,SCM,SCM,SCM);
SCM guile_move_mob_all(SCM, SCM, SCM, SCM);
SCM guile_set_mob_animation(SCM, SCM, SCM, SCM, SCM, SCM);
SCM guile_stop_mob_animation(SCM);
SCM guile_destroy_mob(SCM);
SCM guile_set_mob_userdata(SCM, SCM);
SCM guile_get_mob_userdata(SCM);
SCM guile_add_mob_movement(SCM, SCM, SCM, SCM);
SCM guile_open_mob_eventstack(SCM, SCM);
SCM guile_get_mob_event(SCM, SCM);
SCM guile_close_mob_eventstack(SCM, SCM);
SCM guile_set_mob_frame(SCM, SCM, SCM);
SCM guile_get_mob_frame(SCM);
SCM guile_get_mob_position(SCM);
SCM guile_set_mob_position(SCM, SCM);
SCM guile_stop_mob_movement(SCM);

#endif /*GUILE_MOB_H*/
