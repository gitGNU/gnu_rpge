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
camera global_camera = {0,0};

inline int
get_camera_xoffset(camera c, int screenwidth)
{
  return -c.tilex*TILE_WIDTH + screenwidth/2;
}

inline int
get_camera_yoffset(camera c, int screenheight)
{
  return -c.tiley*TILE_HEIGHT + screenheight/2;
}


/*These two seem quite useless, but they are somewhat useful for GUILE and for abstraction purposes */
inline int
set_camera_x(camera c, int value)
{
  c.tilex = value;
}

inline int
set_camera_y(camera c, int value)
{
  c.tiley = value;
}
