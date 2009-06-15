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
  dispatch.cgen: Implement dispatching.
*/

#include "dispatch.h"
sequence event_dispatch_pairs;
SDL_mutex* dispatch_lock;
S_CONVERTORS(dispatch_pair,DISPATCH_PAIR);

dispatch_pair
make_dispatch_pair (Uint32 eventtype, SCM (*typeproc) (SDL_Event),
		    SCM (*dataproc) (SDL_Event))
{
  dispatch_pair dp;
  dp.eventid = eventtype;
  dp.typefunc = typeproc;
  dp.datafunc = dataproc;
  return dp;
}

void
dispatch_init()
{
  event_dispatch_pairs = sequence_init();
  dispatch_lock = SDL_CreateMutex();
}

void
add_dispatch_pair (dispatch_pair dp)
{
  SDL_mutexP(dispatch_lock);
  sequence_append (&event_dispatch_pairs, make_dispatch_pair_obj (dp));
  SDL_mutexV(dispatch_lock);
}

/*
Although dispatch technically does not allow for multiple dispatches over the same event type, we may eventually implement such (and add_dispatch_pair doesn't block them anyway), so we might as well make this one handle that case.
*/
char
obj_dp_eq_proc (object obj1, object obj2)
{
  return (((dispatch_pair *) obj1.data)->eventid ==
	  ((dispatch_pair *) obj2.data)->eventid)
    && (((dispatch_pair *) obj1.data)->typefunc ==
	((dispatch_pair *) obj2.data)->typefunc)
    && (((dispatch_pair *) obj1.data)->datafunc ==
	((dispatch_pair *) obj2.data)->datafunc);
}

void
remove_dispatch_pair (dispatch_pair dp)
{
  object compobj = make_dispatch_pair_obj (dp);
  SDL_mutexP(dispatch_lock);
  sequence_remove (&event_dispatch_pairs, compobj, obj_dp_eq_proc);
  SDL_mutexV(dispatch_lock);
  free (compobj.data);
}

char
obj_dp_type_eq_proc (object element, object key)
{
  return *((Uint32 *) key.data) == ((dispatch_pair *) element.data)->eventid;
}

dispatch_pair
get_dispatch_pair (Uint32 type)
{
  object key = make_Uint32_obj (type);
  object element =
    sequence_find (event_dispatch_pairs, key, obj_dp_type_eq_proc);
  if (element.data)
    {
      dispatch_pair data = *((dispatch_pair *) element.data);
      free (key.data);
      return data;
    }
  else
    {
      return make_dispatch_pair (0, NULL, NULL);
    }
}

SCM
dispatch (SDL_Event e)
{
  dispatch_pair dp = get_dispatch_pair (e.type);
  if (dp.typefunc && dp.datafunc)
    return scm_cons (dp.typefunc (e), dp.datafunc (e));
  else
    return scm_cons (SCM_EOL, SCM_EOL);
}
