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

#ifndef GUILE_TILE_H
#define GUILE_TILE_H
#include "config.h"
#include "tile.h"
#include <libguile.h>

SCM guile_create_tile(SCM sprite, SCM partclip, SCM blocking);
SCM guile_set_all_tiles(SCM grid,SCM tile);
SCM guile_set_tile(SCM grid,SCM x, SCM y, SCM tile);
SCM guile_make_tilegrid(SCM width, SCM height);
SCM guile_remove_grid(SCM index);
SCM guile_set_main_grid(SCM g);
SCM guile_get_main_grid();

#endif /*GUILE_TILE_H*/
