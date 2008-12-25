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

#include "guile_window.h"

SCM
guile_get_window_coordinates(SCM index)
{
  window w = get_obj_window(windows.data[scm_to_int(index)]);
  return scm_cons(scm_from_int(w.x),scm_from_int(w.y));
}

SCM
guile_get_window_dimensions(SCM index)
{
  window w = get_obj_window(windows.data[scm_to_int(index)]);
  return scm_cons(scm_from_int(w.width),scm_from_int(w.height));
}

SCM
guile_move_window(SCM index, SCM point)
{
  move_window(scm_to_int(index),scm_to_uint(scm_car(point)),scm_to_uint(scm_cdr(point)));
  return SCM_UNSPECIFIED;
}

SCM
guile_resize_window(SCM index, SCM dimensions)
{
  resize_window(scm_to_int(index),scm_to_uint(scm_car(dimensions)),scm_to_uint(scm_cdr(dimensions)));
  return SCM_UNSPECIFIED;
}

SCM
guile_make_window (SCM width, SCM height, SCM x, SCM y, SCM filename,
		   SCM rect)
{
  SDL_Rect clip;
  clip.x = scm_to_uint(scm_list_ref(rect,0));
  clip.y = scm_to_uint(scm_list_ref(rect,1));
  clip.w = scm_to_uint(scm_list_ref(rect,2));
  clip.h = scm_to_uint(scm_list_ref(rect,3));
  return
    scm_from_int (windowstack_addwindow
		  (create_window
		   (scm_to_uint (width), scm_to_uint (height),
		    scm_to_uint (x), scm_to_uint (y),
		    scm_to_locale_string (filename),
		    clip)));
}

SCM
guile_destroy_window (SCM index)
{
  windowstack_remove (scm_to_int (index));
  return scm_from_int (0);
}

