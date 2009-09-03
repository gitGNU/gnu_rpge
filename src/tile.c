/*
Copyright Remco Bras and Michael de Lang 2007,2008
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
tile.c: Implement the functions related to tiles and tilelayers. 
*/
#include "tile.h"

S_CONVERTORS(tilelayer,TILELAYER);
S_CONVERTORS(imagecounter,IMAGECOUNTER);

/*The sequence of current ready, loaded tilegrids (or tile_layers, whichever). The main idea here is to ensure that whatever links to the main grid is at any and all times 
  loaded. (Technically, the point is to A: be able to load the new grid immediately on transition, B: swap around tilegrids if necessary (a prereq of A, really) and C: make sure
  A carries over transitions).*/
sequence tile_layers;

/*
  For now, let's lock a single mutex for any tilegrid operation, to at least 
  make the system thread-safe. Research and testing should be done to figure
  out exactly what would be the best arrangement of locks. The real problem
  here is that the 'ideal' arrangement of locks could probably be modeled
  as a rather complicated mathematical function of the specific usage rpge's
  put to. Put simply, there's probably no ideal general-purpose locking 
  strategy, but we should at least try to find something decent.
*/

SDL_mutex* tile_mutex;

/*We do need to know what world the user would like us to render*/
int maingrid_index = -1;

#define GRID_PTR(n) ((tilelayer*)tile_layers.data[n].data)

void 
init_tiles()
{
  tile_layers = sequence_init();
  tile_mutex = SDL_CreateMutex();
  maingrid_index = -1;
}

tile 
make_tile(unsigned int tilesheet, SDL_Rect clipping, char blocking)
{
  tile t;
  t.tilesheetindex = tilesheet;
  t.sheetclippinginfo = clipping;
  t.blocking = blocking;
  t.occupant = NULL;
  return t;
}

/*Note: The caller should handle the memory allocated here.*/
tile**
init_tilegrid(unsigned int width,unsigned int height)
{
  tile** grid =  xmalloc(sizeof(tile*)*width);
  for(int i = 0; i < width; i++)
    {
      grid[i] = xmalloc(sizeof(tile)*height);
    }
  return grid;
}

/*
  Locking note: Since these functions act on tile**'s, the caller is 
  responsible for locking if the callee is used on tilegrids from 
  the tile_layers sequence.
  (It's probably sane to declare all these static)
*/

static inline tile**
tilegrid_replace_tile(tile** grid, unsigned int x, unsigned int y, tile replacement)
{
  grid[x][y]=replacement;
  return grid;
}

static inline tile**
tilegrid_set_all_tiles(tile** grid, unsigned int gridwidth, unsigned int gridheight, tile replacement)
{
  for(int i = 0; i < gridwidth; i++)
    {
      for(int j = 0; j < gridheight; j++)
        {
          grid[i][j] = replacement;
        }
    }
  return grid;
}

static inline int
search_for_image_id(sequence ics,int id)
{
  for(int i = 0; i < ics.objcount; i++)
    {
      if(((imagecounter*)ics.data[i].data)->index == id)
	return i;
    }
  return -1;
}

static void
increase_count(tilelayer* grid,int imageid)
{
  int index = search_for_image_id(grid->imagecounts,imageid),index_empty = search_for_image_id(grid->imagecounts,-1);
  if(index != -1)
    {
      ((imagecounter*)grid->imagecounts.data[index].data)->count++;
    }
  else if(index_empty != -1)
    {
      imagecounter* ic = ((imagecounter*)grid->imagecounts.data[index_empty].data);
      ic->index = imageid;
      ic->count = 1;
    }
  else
    {
      imagecounter ic = {index: imageid,count: 1};
      sequence_append(&(grid->imagecounts),make_imagecounter_obj(ic));
    }
}

static void
set_count(tilelayer* grid,int imageid, int val)
{
  int index = search_for_image_id(grid->imagecounts,imageid),index_empty = search_for_image_id(grid->imagecounts,-1);
  if(index != -1)
    {
      ((imagecounter*)grid->imagecounts.data[index].data)->count = val;
    }
  else if(index_empty != -1)
    {
      imagecounter* ic = grid->imagecounts.data[index_empty].data;
      ic->index = imageid;
      ic->count = val;
    }
  else
    {
      imagecounter ic = {index : imageid, count: val};
      sequence_append(&(grid->imagecounts),make_imagecounter_obj(ic));
    }
}

static void
decrease_count(tilelayer* grid,int imageid)
{
  int index = search_for_image_id(grid->imagecounts,imageid);
  if(index == -1)
    {
      fprintf(stderr,"rpge:decrease_count: Warning: potentially corrupted imagecounts on grid %p, cannot find count for %d\n",grid,imageid);
    }
  else
    {
      ((imagecounter*)grid->imagecounts.data[index].data)->count--;
      if(!((imagecounter*)grid->imagecounts.data[index].data)->count)
	{
	  release_image(((imagecounter*)grid->imagecounts.data[index].data)->index);
	  ((imagecounter*)grid->imagecounts.data[index].data)->index = -1;
	}
    }
}


/*Pre-render a tilegrid, avoiding duplication of blitting work when rendering frames*/
static SDL_Surface*
remake_tilegrid(tilelayer* main_grid)
{
  SDL_Surface* display;
  if(!main_grid)
    return NULL;
  if(!main_grid->imagebuffer)
    {
      display = SDL_GetVideoSurface();
      main_grid->imagebuffer = SDL_CreateRGBSurface(SDL_HWSURFACE,main_grid->width*TILE_WIDTH,main_grid->height*TILE_HEIGHT,display->format->BitsPerPixel,display->format->Rmask,display->format->Gmask,display->format->Bmask,display->format->Amask);
    }
  else
    SDL_FillRect(main_grid->imagebuffer,NULL,SDL_MapRGB(main_grid->imagebuffer->format,0,0,0));
  render_tilegrid(main_grid->imagebuffer,main_grid->tilegrid,main_grid->width,main_grid->height);
  return main_grid->imagebuffer;
}

static void
free_grid_image_buffer(tilelayer* grid)
{
  SDL_FreeSurface(grid->imagebuffer);
  grid->imagebuffer = NULL;
}

tile** set_tile(int gridid, unsigned int x, unsigned int y, tile replacement)
{
  tilelayer* grid = GRID_PTR(gridid);
  if(grid->tilegrid)
    {
      SDL_mutexP(tile_mutex);
      /*Janitorial work, dealing with image counts*/
      tile old = grid->tilegrid[x][y];
      if(old.tilesheetindex != replacement.tilesheetindex)
	{
	  decrease_count(grid,old.tilesheetindex);
	  increase_count(grid,replacement.tilesheetindex);
	}
      grid->tilegrid = tilegrid_replace_tile(grid->tilegrid,x,y,replacement);
      if(grid == MAIN_GRID)
	remake_tilegrid(grid);
      SDL_mutexV(tile_mutex);
      return grid->tilegrid;
    }
  else
    return NULL;
}

tile** set_all_tiles(int gridid,tile replacement)
{
  tilelayer* grid = GRID_PTR(gridid);
  if(grid->tilegrid)
    {
      SDL_mutexP(tile_mutex);
      grid->tilegrid = tilegrid_set_all_tiles(grid->tilegrid,grid->width,grid->height,replacement);
      set_count(grid,replacement.tilesheetindex,grid->width*grid->height);
      if(grid == MAIN_GRID)
	remake_tilegrid(grid);
      SDL_mutexV(tile_mutex);
      return grid->tilegrid;
    }
  else
    return NULL;
}  

inline char 
occupied(int tilex, int tiley,int grid)
{
  return GRID_PTR(grid)->tilegrid[tilex][tiley].occupant != NULL;
}

inline void 
set_occupant(int tilex, int tiley,int grid, mob* new_occupant)
{
  SDL_mutexP(tile_mutex);
  GRID_PTR(grid)->tilegrid[tilex][tiley].occupant = new_occupant;
  SDL_mutexV(tile_mutex);
}

inline mob* 
get_occupant(int tilex, int tiley,int grid)
{
  return GRID_PTR(grid)->tilegrid[tilex][tiley].occupant;
}

inline void
reset_occupant(int tilex, int tiley,int grid)
{
  SDL_mutexP(tile_mutex);
  GRID_PTR(grid)->tilegrid[tilex][tiley].occupant = NULL;
  SDL_mutexV(tile_mutex);
}

int
add_tilegrid(tilelayer grid)
{
  object o;
  o.data = NULL;
  o.typeinfo = -1;
  int index_empty = sequence_position(tile_layers,o,NULL);
  if(index_empty == -1)
    {
      SDL_mutexP(tile_mutex);
      int index = sequence_append(&tile_layers,make_tilelayer_obj(grid));
      SDL_mutexV(tile_mutex);
      return index;
    }
  else
    {
      SDL_mutexP(tile_mutex);
      tile_layers.data[index_empty] = make_tilelayer_obj(grid);
      SDL_mutexV(tile_mutex);
      return index_empty;
    }
}

void
remove_grid_at(int index)
{
  SDL_mutexP(tile_mutex);
  free_obj(tile_layers.data[index]);
  tile_layers.data[index].data = NULL;
  tile_layers.data[index].typeinfo = -1;
  SDL_mutexV(tile_mutex);
}

inline void
set_maingrid_index(int ind)
{
  SDL_mutexP(tile_mutex);
  if(maingrid_index >= 0 && GRID_PTR(maingrid_index)->imagebuffer)
    free_grid_image_buffer(GRID_PTR(maingrid_index));
  maingrid_index = ind;
  remake_tilegrid(GRID_PTR(ind));
  SDL_mutexV(tile_mutex);
}
