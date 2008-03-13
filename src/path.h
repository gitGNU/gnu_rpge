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

/*
  path.h: Declare path functions
*/

#ifndef PATH_H
#define PATH_H

#include "sequence.h"
#include <string.h>
#include <stdio.h>

extern sequence image_paths;
extern sequence scheme_paths;

void paths_init(void);
void add_path(sequence* pathgroup, char* path);
char* get_path(sequence pathgroup, char* file);

#endif
