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

#ifndef GUILE_LOAD_H
#define GUILE_LOAD_H

#include <libguile.h>
#include "path.h"
#include "sequence.h"

typedef struct
{
  Uint32 threadid;
  SCM argv;
}  thread_argv;

extern sequence argvs;

void guile_exec_script_with_argv(char* filename, SCM argv);
SCM guile_API_exec_script_with_argv(SCM filename, SCM argv);
SCM guile_get_argv();
SCM scm_c_safe_load(char* filename);
SCM guile_safe_load(SCM filename);

#endif /*GUILE_LOAD_H*/
