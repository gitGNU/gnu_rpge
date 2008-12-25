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

#ifndef GUILE_WINDOW_H
#define GUILE_WINDOW_H
#include "window.h"
#include <libguile.h>

SCM guile_make_window (SCM width, SCM height, SCM x, SCM y, SCM filename,SCM  rect);
SCM guile_destroy_window(SCM index);
SCM guile_get_window_coordinates(SCM index);
SCM guile_get_window_dimensions(SCM index);
SCM guile_move_window(SCM index, SCM point);
SCM guile_resize_window(SCM index, SCM dimensions);

#endif /*GUILE_WINDOW_H*/
