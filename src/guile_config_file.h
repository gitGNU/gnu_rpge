/*
Copyright Remco Bras 2009
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

#ifndef GUILE_CONFIG_FILE_H
#define GUILE_CONFIG_FILE_H
#include "config_file.h"
#include <libguile.h>

SCM guile_register_directive(SCM name, SCM func);
SCM guile_remove_directive(SCM name);
SCM guile_load_config_file(SCM name);

#endif
