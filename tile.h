/*
Copyright Remco Bras and Michael de Lang 2007.
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

#define BLOCK_NONE 0x0
#define BLOCK_GROUND 0x1
#define BLOCK_AIR 0x2
#define BLOCK_ALL BLOCK_GROUND | BLOCK_AIR

typedef struct
{
  int tilesheetindex; //in the as-of-yet barely functional global imagestack
  SDL_Rect sheetclipping info;//the part of the tilesheet containing the actual tile
  char blocking; 
} tile;

