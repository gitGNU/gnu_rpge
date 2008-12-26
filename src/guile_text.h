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

#ifndef GUILE_TEXT_H
#define GUILE_TEXT_H

#include <libguile.h>
#include "text.h"

SCM guile_open_font(SCM filename, SCM size);
SCM guile_close_font(SCM index);
SCM guile_make_text(SCM x, SCM y, SCM string, SCM font, SCM red, SCM green, SCM blue);
SCM guile_destroy_text(SCM textindex);
SCM guile_move_text(SCM index, SCM point);
SCM guile_get_text_coordinates(SCM index);
SCM guile_get_text_line_list(SCM index);
SCM guile_get_text_font(SCM index);
SCM guile_set_text_font(SCM index, SCM font);
SCM guile_get_text_color(SCM index);
SCM guile_set_text_color(SCM index, SCM val);

#endif /*GUILE_TEXT_H*/
