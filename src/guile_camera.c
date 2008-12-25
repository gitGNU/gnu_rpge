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

#include "guile_camera.h"

SCM
guile_get_camera_x()
{
  return scm_from_int(global_camera.tilex);
}

SCM
guile_get_camera_y()
{
  return scm_from_int(global_camera.tiley);
}

SCM 
guile_set_camera_x(SCM newvalue)
{
  global_camera.tilex = scm_to_int(newvalue);
  return SCM_UNSPECIFIED;
}

SCM
guile_set_camera_y(SCM newvalue)
{
  global_camera.tiley = scm_to_int(newvalue);
  return SCM_UNSPECIFIED;
}
