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

#include "guile_text.h"

SCM
guile_open_font (SCM filename, SCM size)
{
  return
    scm_from_int (open_font
		  (scm_to_locale_string (filename), scm_to_int (size)));
}

SCM
guile_close_font (SCM index)
{
  close_font (scm_to_int (index));
  return SCM_UNSPECIFIED;
}

SCM
guile_make_text (SCM x, SCM y, SCM string, SCM font, SCM red, SCM green,
		 SCM blue)
{
  SDL_Color c;
  c.r = scm_to_uint8 (red);
  c.g = scm_to_uint8 (green);
  c.b = scm_to_uint8 (blue);
  char* str = scm_to_locale_string(string);
  SCM retval = 
    scm_from_int (add_text
		  (make_text
		   (scm_to_uint (x), scm_to_uint (y),
		    str, scm_to_int (font), c)));
  free(str);
  return retval;
}

SCM
guile_destroy_text (SCM textindex)
{
  remove_text (scm_to_int (textindex));
  return SCM_UNSPECIFIED;
}

SCM
guile_move_text(SCM index, SCM point)
{
  move_text(scm_to_int(index),scm_to_uint(scm_car(point)),scm_to_uint(scm_cdr(point)));  
  return SCM_UNSPECIFIED;
}

SCM
guile_get_text_coordinates(SCM index)
{
  int ind = scm_to_int(index);
  text* t = (text*)texts.data[ind].data;
  return scm_cons(scm_from_uint(t->x),scm_from_uint(t->y));
}

/*
  Actually slightly more generic than it's being used here...
  Since nothing else uses it anyway, we can leave it here.
*/
SCM
obj_to_scm(object o)
{
  /*To do: implement dispatching*/
  if(o.typeinfo == TYPE_STRING)
    return scm_from_locale_string(*((char**)o.data));
  return SCM_EOL;
}

SCM
guile_sequence_to_list(sequence s)
{
  SCM last = scm_cons(obj_to_scm(s.data[0]),SCM_EOL),current,first = last;
  for(int i = 1; i < s.objcount; i++)
    {
      current = scm_cons(obj_to_scm(s.data[i]),SCM_EOL);
      scm_set_cdr_x(last,current);
      last = current;
    }
  return first;
}

SCM
guile_get_text_line_list(SCM index)
{
  return guile_sequence_to_list(((text*)texts.data[scm_to_int(index)].data)->buffers);
}

