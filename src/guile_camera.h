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

#ifndef GUILE_CAMERA_H
#define GUILE_CAMERA_H
#include "config.h"
#include <libguile.h>
#include "camera.h"

SCM guile_set_camera_x(SCM newvalue);
SCM guile_set_camera_y(SCM newvalue);
SCM guile_get_camera_x();
SCM guile_get_camera_y();

#endif /*GUILE_CAMERA*/
