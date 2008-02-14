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
camera.h: Define and declare the required stuff to deal with cameras.
May be smobbed later or otherwise interfaced to GUILE.
*/

#include "constants.h"

typedef struct
{
  int tilex, tiley;
} camera;

extern camera global_camera;

int get_camera_xoffset(camera c, int screenwidth);
int get_camera_yoffset(camera y, int screenheight);
