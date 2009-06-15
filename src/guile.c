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

#include "guile.h"

SCM global_userdata = SCM_EOL;

char
scm_to_blocking_flag(SCM flag)
{
  return flag == SCM_BOOL_T;
}

SCM
guile_get_global_userdata (void)
{
  return global_userdata;
}

SCM
guile_set_global_userdata (SCM newdata)
{
  scm_gc_unprotect_object (global_userdata);
  global_userdata = newdata;
  scm_gc_protect_object (global_userdata);
  return SCM_UNSPECIFIED;
}

SCM
guile_run_repl()
{
  /*Since the repl_signal mutex is locked by the main loop, use an event to signal it needs to be released*/
  SDL_Event* e = malloc(sizeof(SDL_Event));
  e->type = SDL_USEREVENT;
  e->user.code = RELEASE_REPL_MUTEX;
  SDL_PushEvent(e);
  return SCM_UNSPECIFIED;
}

SCM
guile_stop_repl()
{
  SDL_Event* e = malloc(sizeof(SDL_Event));
  e->type = SDL_USEREVENT;
  e->user.code = ACQUIRE_REPL_MUTEX;
  SDL_PushEvent(e);
  return SCM_UNSPECIFIED;
}
