/*
Copyright Remco Bras 
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

SCM scm_ncar (SCM list, int n)
{
  for(int i = 0; i < n; i++)
    {
      list = scm_car(list);
    }
  return list;
}

SCM scm_ncdr (SCM list, int n)
{
  for(int i = 0; i < n; i++)
    {
      list = scm_cdr(list);
    }
  return list;
}

SCM scm_nth (SCM list, int n)
{
  return scm_car(scm_ncdr(list, n-1));
}

SCM guile_create_mob(SCM x, SCM y, SCM string)
{
  char* filename = scm_to_locale_string(string);
  unsigned int xc = scm_to_uint(x), yc = scm_to_uint(y);
  return scm_from_int(push_mob_on_array(create_mob_using_sprite(xc,yc,filename)));
}

tile list_to_tile(SCM tilelist)
{
  tile t;
  SDL_Rect clip;
  t.tilesheetindex = scm_to_int(scm_car(tilelist));
  clip.x = scm_to_int16(scm_cadr(tilelist));
  clip.y = scm_to_int16(scm_nth(tilelist,3));
  clip.w = scm_to_uint16(scm_nth(tilelist,4));
  clip.h = scm_to_uint16(scm_nth(tilelist,5));
  t.blocking = scm_to_char(scm_nth(tilelist,6));
  t.sheetclippinginfo = clip;
  return t;
}

SCM tile_to_list(tile t)
{
  SCM l = scm_list_n(scm_from_int(t.tilesheetindex),
                     scm_from_int16(t.sheetclippinginfo.x),
                     scm_from_int16(t.sheetclippinginfo.y),
                     scm_from_uint16(t.sheetclippinginfo.w),
                     scm_from_uint16(t.sheetclippinginfo.h),
                     scm_from_char(t.blocking),
                     SCM_UNDEFINED);
  return l;
}

SCM guile_create_tile(SCM sprite, SCM partclip, SCM blocking)
{
  char* spritefilename = scm_to_locale_string(sprite);
  char block = scm_to_char(blocking);
  SDL_Rect r;
  r.x = scm_to_int16(scm_car(partclip));
  r.y = scm_to_int16(scm_cadr(partclip));
  r.w = scm_to_uint16(scm_caddr(partclip));
  r.h = scm_to_uint16(scm_cadddr(partclip));
  return tile_to_list(make_tile(push_image_on_stack(spritefilename),r,block));
}

SCM guile_set_tile(SCM x, SCM y, SCM tile)
{
  set_tile(scm_to_int16(x),scm_to_int16(y),list_to_tile(tile));
  return scm_from_int(0);
}

SCM guile_set_all_tiles(SCM tile)
{
  set_all_tiles(list_to_tile(tile));
  return scm_from_int(0);
}

SCM guile_move_mob_right(SCM mobindex, SCM tilecount, SCM frametotal)
{
  mob_move_right(&(mobs.mobs[scm_to_int(mobindex)]),scm_to_int(tilecount),scm_to_int(frametotal));
  return scm_from_int(0);
}

SCM guile_move_mob_left(SCM mobindex, SCM tilecount, SCM frametotal)
{
  mob_move_left(&(mobs.mobs[scm_to_int(mobindex)]),scm_to_int(tilecount),scm_to_int(frametotal));
  return scm_from_int(0);
}

SCM guile_move_mob_down(SCM mobindex, SCM tilecount, SCM frametotal)
{
  mob_move_down(&(mobs.mobs[scm_to_int(mobindex)]),scm_to_int(tilecount),scm_to_int(frametotal));
  return scm_from_int(0);
}

SCM guile_move_mob_up(SCM mobindex, SCM tilecount, SCM frametotal)
{
  mob_move_up(&(mobs.mobs[scm_to_int(mobindex)]),scm_to_int(tilecount),scm_to_int(frametotal));
  return scm_from_int(0);
}
