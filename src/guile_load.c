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

#include "guile_load.h"

sequence argvs;
S_CONVERTORS(thread_argv,THREAD_ARGV);
SDL_mutex* argv_lock;

void
argvs_init()
{
  argvs = sequence_init();
  argv_lock = SDL_CreateMutex();
}


/*
Technically, the following is a simple, though somewhat useful scheme to pass a single SCM to a script on boot. The whole point of this is to allow scripts that take 'arguments' to be used, in a rather warped way, somewhat like a procedure.
*/

thread_argv
make_threadargv (Uint32 threadid, SCM argv)
{
  thread_argv ta;
  ta.threadid = threadid;
  ta.argv = argv;
  return ta;
}

char
guile_argv_threadids_equalp (object argv, object threadid)
{
  return get_obj_Uint32 (threadid) == get_obj_thread_argv (argv).threadid;
}

void
guile_exec_script_with_argv (char *filename, SCM argv)
{
  Uint32 threadid = SDL_ThreadID ();
  int index = sequence_position (argvs, make_Uint32_obj (threadid),
				 guile_argv_threadids_equalp);
  SDL_mutexP(argv_lock);
  if (index != -1)
    argvs.data[index] =
      make_thread_argv_obj (make_threadargv (threadid, argv));
  else
    index =
      sequence_append (&argvs,
		       make_thread_argv_obj (make_threadargv
					     (threadid, argv)));
  SDL_mutexV(argv_lock);
  scm_c_primitive_load (filename);
  SDL_mutexP(argv_lock);
  sequence_remove_at (&argvs, index);
  SDL_mutexV(argv_lock);
}

SCM
guile_API_exec_script_with_argv (SCM filename, SCM argv)
{
  guile_exec_script_with_argv (scm_to_locale_string (filename), argv);
  return SCM_UNSPECIFIED;
}

SCM
guile_get_argv ()
{
  int index = sequence_position (argvs, make_Uint32_obj (SDL_ThreadID ()),
				 guile_argv_threadids_equalp);
  if (index != -1)
    return ((thread_argv *) argvs.data[index].data)->argv;
  else
    return SCM_EOL;
}

SCM 
scm_c_safe_load(char* filename)
{
  SCM load_mutex = scm_variable_ref(scm_c_lookup("load-mutex"));
  scm_lock_mutex(load_mutex);
  scm_c_primitive_load(filename);
  scm_unlock_mutex(load_mutex);
  return SCM_UNSPECIFIED;
}

SCM
guile_safe_load(SCM filename)
{
  char* path = get_path(scheme_paths,scm_to_locale_string(filename));
  if(path)
    {
      scm_c_safe_load(path);
      free(path);
    }
  else
    fprintf(stderr,"RPGE: Cannot find Scheme source file in . or Scheme search paths: %s\n",scm_to_locale_string(filename));
  return SCM_UNSPECIFIED;
}

