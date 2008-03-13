/*
Copyright Remco Bras 2007
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
config_file.h: Here because config_file.c needs an accompanying header.
*/

#ifndef CONFIG_FILE_H
#define CONFIG_FILE_H

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <libguile.h>
#include <ctype.h>
#include "xalloc.h"

#define BLOCK_SIZE 256

char* getline(FILE* stream);
void exec_config_file(char* filename);

#endif
