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

#include "guile_mob.h"

SCM
guile_create_mob (SCM x, SCM y, SCM grid, SCM string)
{
  char *filename = scm_to_locale_string (string);
  unsigned int xc = scm_to_uint (x), yc = scm_to_uint (y), gc = scm_to_uint(grid);
  return
    scm_from_int (push_mob_on_array
		  (create_mob_using_sprite (xc, yc, gc, filename)));
}

SCM
guile_move_mob_all (SCM mobindex, SCM tilecountx, SCM tilecounty,
		    SCM frametotal)
{
  mob_move_all_by_index (scm_to_int(mobindex),
			 scm_to_int (tilecountx), scm_to_int (tilecounty),
			 scm_to_int (frametotal));
  return scm_from_int (0);
}

SCM
guile_set_mob_animation (SCM mobindex, SCM animation, SCM start,
			 SCM targetframe, SCM framesbetween, SCM loop)
{
  mob_set_animation_by_index (scm_to_int(mobindex),
			      scm_to_int (animation), scm_to_int (start),
			      scm_to_int (targetframe), scm_to_int (framesbetween),
			      scm_to_bool (loop));
  return scm_from_int (0);
}

SCM
guile_stop_mob_animation (SCM mobindex)
{
  mob_stop_animation_by_index (scm_to_int(mobindex));
  return scm_from_int (0);
}

SCM
guile_destroy_mob (SCM mobindex)
{
  remove_mob (scm_to_int (mobindex));
  return scm_from_int (0);
}

SCM
guile_set_mob_userdata (SCM index, SCM newdata)
{
  set_mob_userdata_by_index(scm_to_int(index),newdata);
  return SCM_UNSPECIFIED;
}

SCM
guile_get_mob_userdata (SCM index)
{
  return ((mob *) mobs.data[scm_to_int (index)].data)->userdata;
}

SCM
guile_add_mob_movement (SCM mob, SCM xtile, SCM ytile, SCM frames)
{
  mob_add_movement_by_index(scm_to_int(mob),scm_to_int(xtile),scm_to_int(ytile),scm_to_int(frames));
  return SCM_UNSPECIFIED;
}

SCM 
guile_open_mob_eventstack(SCM mobindex,SCM flag)
{
  char block = scm_to_blocking_flag(flag);
  return scm_from_uint(eventstack_open(&(((mob*)mobs.data[scm_to_int(mobindex)].data)->events),block));
}

SCM
guile_get_mob_event(SCM mobindex, SCM luser)
{
  event e = eventstack_get_first_of_user(&((mob*)mobs.data[scm_to_int(mobindex)].data)->events,scm_to_uint(luser));
  return scm_cons(e.type,e.data);
}

SCM
guile_close_mob_eventstack(SCM mobindex, SCM luser)
{
  eventstack_close(&(((mob*)mobs.data[scm_to_int(mobindex)].data)->events),scm_to_uint(luser));
  return SCM_UNSPECIFIED;
}

SCM
guile_set_mob_frame(SCM index, SCM animation, SCM framenum)
{
  set_mob_frame_by_index(scm_to_int(index),scm_to_uint(animation),scm_to_uint(framenum));
  return SCM_UNSPECIFIED;
}

SCM
guile_get_mob_frame(SCM index)
{
  mob* m = get_mob_by_index(scm_to_int(index));
  return scm_list_n(scm_from_uint(m->animation),scm_from_uint(m->frame),SCM_UNDEFINED);
}

SCM
guile_get_mob_position(SCM mobby)
{
  int index = scm_to_int(mobby);
  mob* m = get_mob_by_index(index);
  return scm_list_n(scm_from_int(m->x/TILE_WIDTH),scm_from_int(m->y/TILE_HEIGHT),scm_from_int(m->grid),SCM_UNDEFINED);
}

SCM
guile_set_mob_position(SCM mobby, SCM pos)
{  
  if(set_mob_position_by_index(scm_to_int(mobby),scm_to_int(scm_car(pos)),scm_to_int(scm_cadr(pos)),scm_to_int(scm_caddr(pos))))
    return SCM_BOOL_T;
  else
    return SCM_BOOL_F;
}
