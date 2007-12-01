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

#include "guile.h"

SCM
scm_ncar (SCM list, int n)
{
  for (int i = 0; i < n; i++)
    {
      list = scm_car (list);
    }
  return list;
}

SCM
scm_ncdr (SCM list, int n)
{
  for (int i = 0; i < n; i++)
    {
      list = scm_cdr (list);
    }
  return list;
}

SCM
scm_nth (SCM list, int n)
{
  return scm_car (scm_ncdr (list, n - 1));
}

SCM
guile_create_mob (SCM x, SCM y, SCM string)
{
  char *filename = scm_to_locale_string (string);
  unsigned int xc = scm_to_uint (x), yc = scm_to_uint (y);
  return
    scm_from_int (push_mob_on_array
		  (create_mob_using_sprite (xc, yc, filename)));
}

tile
list_to_tile (SCM tilelist)
{
  tile t;
  SDL_Rect clip;
  t.tilesheetindex = scm_to_int (scm_car (tilelist));
  clip.x = scm_to_int16 (scm_cadr (tilelist));
  clip.y = scm_to_int16 (scm_nth (tilelist, 3));
  clip.w = scm_to_uint16 (scm_nth (tilelist, 4));
  clip.h = scm_to_uint16 (scm_nth (tilelist, 5));
  t.blocking = scm_to_char (scm_nth (tilelist, 6));
  t.sheetclippinginfo = clip;
  return t;
}

SCM
tile_to_list (tile t)
{
  SCM l = scm_list_n (scm_from_int (t.tilesheetindex),
		      scm_from_int16 (t.sheetclippinginfo.x),
		      scm_from_int16 (t.sheetclippinginfo.y),
		      scm_from_uint16 (t.sheetclippinginfo.w),
		      scm_from_uint16 (t.sheetclippinginfo.h),
		      scm_from_char (t.blocking),
		      SCM_UNDEFINED);
  return l;
}

SCM
guile_create_tile (SCM sprite, SCM partclip, SCM blocking)
{
  char *spritefilename = scm_to_locale_string (sprite);
  char block = scm_to_char (blocking);
  SDL_Rect r;
  r.x = scm_to_int16 (scm_car (partclip));
  r.y = scm_to_int16 (scm_cadr (partclip));
  r.w = scm_to_uint16 (scm_caddr (partclip));
  r.h = scm_to_uint16 (scm_cadddr (partclip));
  return
    tile_to_list (make_tile (push_image_on_stack (spritefilename), r, block));
}

SCM
guile_set_tile (SCM x, SCM y, SCM tile)
{
  set_tile (scm_to_int16 (x), scm_to_int16 (y), list_to_tile (tile));
  return scm_from_int (0);
}

SCM
guile_set_all_tiles (SCM tile)
{
  set_all_tiles (list_to_tile (tile));
  return scm_from_int (0);
}

SCM
guile_move_mob_all (SCM mobindex, SCM tilecountx, SCM tilecounty,
		    SCM frametotal)
{
  mob_move_all ((mob*)mobs.data[scm_to_int (mobindex)].data, scm_to_int (tilecountx),
		scm_to_int (tilecounty), scm_to_int (frametotal));
  return scm_from_int (0);
}

SCM
guile_set_mob_animation (SCM mobindex, SCM animation, SCM start,
			 SCM targetframe, SCM framesbetween, SCM loop)
{
  mob_set_animation ((mob*)mobs.data[scm_to_int (mobindex)].data,
		     scm_to_int (animation), scm_to_int (start),
		     scm_to_int (targetframe), scm_to_int (framesbetween),
		     scm_to_bool (loop));
  return scm_from_int (0);
}

SCM
guile_stop_mob_animation (SCM mobindex)
{
  mob_stop_animation ((mob*)mobs.data[scm_to_int (mobindex)].data);
  return scm_from_int (0);
}

SCM
guile_destroy_mob (SCM mobindex)
{
  remove_mob (scm_to_int (mobindex));
  return scm_from_int (0);
}

SCM
guile_init_tilegrid (SCM width, SCM height)
{
  main_grid.tilegrid =
    init_tilegrid (scm_to_uint (width), scm_to_uint (height));
  main_grid.width = scm_to_uint (width);
  main_grid.height = scm_to_uint (height);
  return scm_from_int (0);
}

SCM
guile_make_window (SCM width, SCM height, SCM x, SCM y, SCM filename,
		   SCM tilew, SCM tileh)
{
  return
    scm_from_int (windowstack_addwindow
		  (create_window
		   (scm_to_uint (width), scm_to_uint (height),
		    scm_to_uint (x), scm_to_uint (y),
		    scm_to_locale_string (filename), scm_to_uint (tilew),
		    scm_to_uint (tileh))));
}

SCM
guile_destroy_window(SCM index)
{
  windowstack_remove(scm_to_int(index));
  return scm_from_int(0);
}

SCM
guile_open_global_eventstack()
{
  return scm_from_int(eventstack_open(&global_usereventstack));
}

SCM
guile_close_global_eventstack(SCM userindex)
{
  eventstack_close(&global_usereventstack,scm_to_int(userindex));
  return SCM_UNSPECIFIED;
}

SCM
guile_get_global_event(SCM userindex)
{
  event e = eventstack_get_first_of_user(&global_usereventstack, scm_to_int(userindex));
  return scm_cons(e.type,e.data);
}


SCM
guile_set_mob_userdata(SCM index, SCM newdata)
{
  ((mob*)mobs.data[scm_to_int(index)].data)->userdata=newdata;
  return SCM_UNSPECIFIED;
}

SCM
guile_get_mob_userdata(SCM index)
{
  return ((mob*)mobs.data[scm_to_int(index)].data)->userdata;
}

SCM 
guile_get_global_userdata(void)
{
  return global_userdata;
}

SCM
guile_set_global_userdata(SCM newdata)
{
  global_userdata = newdata;
  return SCM_UNSPECIFIED;
}

/*
Yet-another-generic-stupid-stack-that-has-to-be-added-for-no-reason.... I do have some code lying around for this, but as it uses runtime code to do lots and lots of conversions back and forth between a wrapper around a void* and something indicating the type of whatever the void* is pointing to and whatever that actually happens to be it is actually rather slow. What we really need here is a template-like system...
*/

