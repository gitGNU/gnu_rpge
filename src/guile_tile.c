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

#include "guile_tile.h"

static inline tile
list_to_tile (SCM tilelist)
{
  tile t;
  SDL_Rect clip;
  t.tilesheetindex = push_image_on_stack(scm_to_locale_string(scm_cadr(tilelist)));
  clip.x = scm_to_int16 (scm_list_ref (tilelist, scm_from_int(2)));
  clip.y = scm_to_int16 (scm_list_ref (tilelist, scm_from_int(3)));
  clip.w = scm_to_uint16 (scm_list_ref (tilelist, scm_from_int(4)));
  clip.h = scm_to_uint16 (scm_list_ref (tilelist, scm_from_int(5)));
  t.blocking = scm_to_char (scm_list_ref (tilelist, scm_from_int(6)));
  t.sheetclippinginfo = clip;
  t.occupant = NULL;
  return t;
}

static inline SCM
tile_to_list (tile t)
{
  char* fname = get_image_name(t.tilesheetindex);
  SCM l = scm_list_n (scm_from_locale_symbol("tile"),
                      scm_from_locale_string (fname),
		      scm_from_int16 (t.sheetclippinginfo.x),
		      scm_from_int16 (t.sheetclippinginfo.y),
		      scm_from_uint16 (t.sheetclippinginfo.w),
		      scm_from_uint16 (t.sheetclippinginfo.h),
		      scm_from_char (t.blocking),
		      SCM_UNDEFINED);
  free(fname);
  return l;
}

/*
This seems deceptively similar to the above, but the major difference is that tile_to_list is meant for tiles already created, i.e. proper tiles with internalized images. Since it's rather tough (outside of smobs and the like) to keep track of image references in GUILE and it's more logical to not preload images for tiles a user just keeps around, we define the below function to deal with pseudotiles, i.e. tiles without a proper, internalized image. 
*/

static inline SCM
listify_pseudotile(char* filename, SDL_Rect clip, char blocking)
{
  return scm_list_n(scm_from_locale_symbol("tile"),
                    scm_from_locale_string(filename),
                    scm_from_int16(clip.x),
                    scm_from_int16(clip.y),
                    scm_from_uint16(clip.w),
                    scm_from_uint16(clip.h),
                    scm_from_char(blocking),
                    SCM_UNDEFINED);
}

SCM
guile_create_tile (SCM sprite, SCM partclip, SCM blocking)
{
  char *spritefilename = scm_to_locale_string (sprite);
  char block = scm_to_char (blocking);
  SDL_Rect r;
  r.x = scm_to_int16 (scm_car (partclip));
  r.y = scm_to_int16 (scm_cadr (partclip));
  r.w = scm_to_uint16 (scm_caddr (partclip));
  r.h = scm_to_uint16 (scm_cadddr (partclip));
  return
    listify_pseudotile(spritefilename,r,block);
}

SCM
guile_set_tile (SCM grid,SCM x, SCM y, SCM tile)
{
  set_tile (scm_to_int(grid),scm_to_int16 (x), scm_to_int16 (y), list_to_tile (tile));
  return scm_from_int (0);
}

SCM
guile_set_all_tiles (SCM grid,SCM tile)
{
  set_all_tiles (scm_to_int(grid),list_to_tile (tile));
  return scm_from_int (0);
}

SCM
guile_make_tilegrid (SCM width, SCM height)
{
  tilelayer new_grid;
  memset(&new_grid,0,sizeof(tilelayer));
  new_grid.width = scm_to_int(width);
  new_grid.height = scm_to_int(height);
  new_grid.tilegrid=init_tilegrid(new_grid.width,new_grid.height);
  new_grid.imagecounts = sequence_init();
  return scm_from_int(add_tilegrid(new_grid));
}

SCM 
guile_remove_grid(SCM index)
{
  remove_grid_at(scm_to_int(index));
  return SCM_UNSPECIFIED;
}

SCM
guile_set_main_grid(SCM g)
{
  set_maingrid_index(scm_to_int(g));  
  return SCM_UNSPECIFIED;
}

SCM
guile_get_main_grid()
{
  return scm_from_int(maingrid_index);
}
