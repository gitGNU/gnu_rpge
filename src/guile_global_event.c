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

#include "guile_global_event.h"

SCM
guile_open_global_eventstack (SCM flag)
{
  char block = scm_to_blocking_flag(flag);
  return scm_from_int (eventstack_open (&global_usereventstack,block));
}

SCM
guile_close_global_eventstack (SCM userindex)
{
  eventstack_close (&global_usereventstack, scm_to_int (userindex));
  return SCM_UNSPECIFIED;
}

SCM
guile_get_global_event (SCM userindex)
{
  event e = eventstack_get_first_of_user (&global_usereventstack,
					  scm_to_int (userindex));
  return scm_cons (e.type, e.data);
}


