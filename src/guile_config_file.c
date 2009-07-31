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

#include "guile_config_file.h"

/*
  Register procedure func as a callback to be called whenever the config file parser sees
  a directive of the form name:<arguments>\n. As long as func is registered here,
  it is protected from the GC.
*/
SCM
guile_register_directive(SCM name, SCM func)
{
  scm_gc_protect_object(func);
  /*
    scm_to_locale_string allocs with malloc, thus automatically satisfying the requirement
    that the name argument to register*directive functions must be free()able.
  */
  register_scm_directive(scm_to_locale_string(name),func);
  return SCM_UNSPECIFIED;
}

/*
  Remove all directives named name.
*/
SCM
guile_remove_directive(SCM name)
{
  char* str = scm_to_locale_string(name);
  /*
    Go through the directives that will be removed, unprotecting
    the SCMs in those.
    This would NOT free any C function pointers that need freeing,
    though those are so rare we can probably not worry about them now.
    Also, d.name is left unfreed, such that remove_directive can handle it.
  */
  for(int i = 0; i < directives.objcount; i++)
    {
      directive_t d = get_obj_directive_t(directives.data[i]);
      if(!strcmp(d.name,str) && d.func.scmp)
	scm_gc_unprotect_object(d.func.scmfunc.lambda);
    }
  remove_directive(str);
  free(str);
  return SCM_UNSPECIFIED;
}

SCM
guile_load_config_file(SCM name)
{
  char* str = scm_to_locale_string(name);
  exec_config_file(str);
  free(str);
  return SCM_UNSPECIFIED;
}
