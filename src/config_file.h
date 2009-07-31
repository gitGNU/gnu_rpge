/*
Copyright Remco Bras 2007,2008
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
#include "sequence.h"
#include "path.h"
#include "guile.h" /*We need scm_c_safe_load*/

/*
  Define a few structs so we can use either a guile procedure or C function to handle
  config file directives.
*/
struct directive_t_func
{
  char scmp;
  void (*func)(char *);
};

struct directive_t_scm
{
  char scmp;
  SCM lambda;
};

typedef union
{
  char scmp;
  struct directive_t_func cfunc;
  struct directive_t_scm scmfunc;
} directive_t_callee;

typedef struct
{
  char* name;
  directive_t_callee func;
} directive_t;

extern sequence directives;

#define BLOCK_SIZE 256

char* getline(FILE* stream);
void exec_config_file(char* filename);
void directives_init();
void register_scm_directive(char* name, SCM func);
void register_directive(char* name,void (*func)(char*));
void remove_directive(char* name);
directive_t get_obj_directive_t(object o);
object make_directive_t_obj(directive_t d);

#endif
