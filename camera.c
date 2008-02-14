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
camera.c: Implement camera offset math, to be used in rendering. 
*/

#include "camera.h"

inline int
get_camera_xoffset(camera c, int screenwidth)
{
  return c.tilex*TILE_WIDTH - screenwidth/2;
}

inline int
get_camera_yoffset(camera c, int screenheight)
{
  return c.tiley*TILE_WIDTH -screenheight/2;
}
