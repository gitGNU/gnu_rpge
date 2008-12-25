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

#include "event.h"
#include <libguile.h>

#ifndef GUILE_GLOBAL_EVENT_H
#define GUILE_GLOBAL_EVENT_H

SCM guile_open_global_eventstack(SCM flag);
SCM guile_close_global_eventstack(SCM userindex);
SCM guile_get_global_event(SCM userindex);

#endif /*GUILE_GLOBAL_EVENT_H*/
