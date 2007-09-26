/*
Copyright Remco Bras 
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

SCM guile_create_mob(SCM x, SCM y, SCM string)
{
  char* filename = scm_to_locale_string(string);
  unsigned int xc = scm_to_uint(x), yc = scm_to_uint(y);
  return scm_from_int(push_mob_on_array(create_mob_using_sprite(xc,yc,filename)));
}

