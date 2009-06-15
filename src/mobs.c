/*
Copyright Remco Bras and Michael de Lang 2007,2008.
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

#include "mobs.h"
sequence mobs;
SDL_mutex* mob_mutex;
S_CONVERTORS(move_descriptor,MOVE_DESCRIPTOR);
S_CONVERTORS(mob,MOB);

void
mobs_init(void)
{
  mob_mutex = SDL_CreateMutex();
  mobs = sequence_init();
}

static inline void
lock_mobs()
{
  SDL_mutexP(mob_mutex);
}

static inline void
unlock_mobs()
{
  SDL_mutexV(mob_mutex);
}

mob
create_mob_using_sprite (unsigned x, unsigned y, unsigned grid, char *sprity)
{
  mob mobby;
  memset(&mobby,0,sizeof(mob));
  mobby.imgindex = push_image_on_stack (sprity);
  mobby.x = x * TILE_WIDTH;
  mobby.y = y * TILE_HEIGHT;
  mobby.grid = grid;
  mobby.xpixelalignment = mobby.ypixelalignment = 0;
  mobby.userdata = SCM_EOL;
  scm_gc_protect_object(mobby.userdata);
  mobby.move_descriptors = sequence_init();
  mobby.events = eventstack_init();
  return mobby;
}

int
find_empty_mob ()
{
  for (int i = 0; i < mobs.objcount; i++)
    {
      if (((mob*)mobs.data[i].data)->imgindex == -1)
	return i;
    }
  return -1;
}

static void
set_mob_occupants(mob* m)
{
  int tilex = (m->x+SPRITE_WIDTH/2)/TILE_WIDTH, tiley = (m->y+SPRITE_HEIGHT/2)/TILE_HEIGHT;
  set_occupant(tilex,tiley,m->grid,m);
}

static inline void
set_mob_tile_x(mob* m,int val)
{
  m->x = val*TILE_WIDTH;
}

static inline void
set_mob_tile_y(mob* m, int val)
{
  m->y = val*TILE_HEIGHT;
}

static inline void
set_mob_grid(mob* m, int val)
{
  m->grid = val;
}

int
push_mob_on_array (mob m)
{
  int indexempty = find_empty_mob ();
  if (indexempty != -1)
    {
      lock_mobs();
      *((mob*)mobs.data[indexempty].data) = m;
      unlock_mobs();
      set_mob_occupants(mobs.data[indexempty].data);
      return indexempty;
    }
  else
    {
      lock_mobs();
      int index = sequence_append(&mobs,make_mob_obj(m));
      unlock_mobs();
      set_mob_occupants(mobs.data[index].data);
      return index;
    }
}

void
remove_mob (int index)
{
  if (index < mobs.objcount)
    {
      mob m = *((mob*)mobs.data[index].data);
      int tilex = (m.x+SPRITE_WIDTH/2)/SPRITE_WIDTH,tiley = (m.y+SPRITE_HEIGHT/2)/SPRITE_HEIGHT;
      reset_occupant(tilex,tiley,maingrid_index);
      lock_mobs();
      release_image(((mob*)mobs.data[index].data)->imgindex);
      ((mob*)mobs.data[index].data)->imgindex = -1;
      unlock_mobs();
    }
}

static inline char
moving(mob m)
{
  return (m.xmoveamount && m.xmoverate) || (m.ymoveamount && m.ymoverate);
}

static void
animate_mob (mob * m)
{
  if (m->timetonextframe == -1)
    return;
  m->timetonextframe--;
  if (!m->timetonextframe)
    {
      if (m->resetonnext)
	{
	  m->frame = 0;
	  m->timetonextframe = m->initialtimetonextframe;
	  m->resetonnext = 0;
	  return;
	}
      m->frame++;
      if (m->frame == m->targetframe)
	{
	  if (m->animlooping)
	    {
	      m->resetonnext = 1;
	      m->timetonextframe = m->initialtimetonextframe;
	      return;
	    }
	  else
	    {
	      m->timetonextframe = -1;
	      return;
	    }
	}
      else
	{
	  m->timetonextframe = m->initialtimetonextframe;
	  return;
	}
    }
}

void
animate_mobs ()
{
  lock_mobs();
  for (int i = 0; i < mobs.objcount; i++)
    {
      animate_mob ((mob*)mobs.data[i].data);
    }
  unlock_mobs();
}

static void*
inner_add_mob_collision_event(void* indices)
{
  int index1 = ((int*)indices)[0], index2 = ((int*)indices)[1];
  eventstack_addevent(&global_usereventstack,
		      make_event(scm_from_locale_symbol("collision"),
				 scm_cons(scm_from_int(index1),
					  scm_from_int(index2))));  
  return NULL;
}

static void
add_mob_collision_event(mob* mob1, mob* mob2)
{
  int indexes[2] = {-1,-1};
  for(int i = 0; i < mobs.objcount ; i++)
    {
      if(mobs.data[i].data == mob1)
        indexes[0] = i;
      else if(mobs.data[i].data == mob2)
        indexes[1] = i;
    }
  scm_with_guile(inner_add_mob_collision_event,indexes);
}

struct tilechange_arg
{
  mob* m;
  int oldx,oldy,oldg,newx,newy,newg;
};

static void*
add_mob_tilechange_event(void* arg)
{
  struct tilechange_arg* args = arg;
  mob* m = args->m;
  int oldx = args->oldx, 
    oldy = args->oldy,
    oldg = args->oldg,
    newx = args->newx, 
    newy = args->newy,
    newg = args->newg;
  SCM type = scm_from_locale_symbol("tile-change");
  SCM data = scm_cons(scm_list_n(scm_from_int(oldx),
				 scm_from_int(oldy),
				 scm_from_int(oldg),
				 SCM_UNDEFINED),
		      scm_list_n(scm_from_int(newx),
				 scm_from_int(newy),
				 scm_from_int(newg),
				 SCM_UNDEFINED));
  eventstack_addevent(&(m->events),make_event(type,data));
  return NULL;
}

/*
This can only be called safely from move_mob, since otherwise move_mob or something similar may attempt to reference it at any point in time.
*/
static inline void
clear_mob_movement_queue(mob* m)
{
  sequence_free(m->move_descriptors);
  m->move_descriptors.objcount = 0;
  m->move_descriptors.data = NULL;
}

static void
mob_set_animation (mob * m, unsigned int animation, unsigned int startframe,
		   unsigned int targetframe, unsigned int framesperframe,
		   char looping)
{
  m->animation = animation;
  m->frame = startframe;
  m->targetframe = targetframe;
  m->timetonextframe = m->initialtimetonextframe = framesperframe;
  m->animlooping = looping;
}

static void
mob_stop_animation(mob* m)
{
  m->targetframe = m->frame;
  m->timetonextframe = -1;
  m->animlooping = 0;
}

static void
mob_set_movement (mob * m, int xam, double xrate, int yam, double yrate)
{
  m->xmoveamount = xam;
  m->xmoverate = xrate;
  m->ymoveamount = yam;
  m->ymoverate = yrate;
}

/*
  Struct declaration needed for the following few procedures.
*/
struct mob_motion_start_event_descriptor
{
  mob* m;
  int xtiles,ytiles,frames;
};

/*
  Workhorse procedure, to be run in guile mode.
  Arg should be a pointer to a properly filled-in
  struct mob_motion_start_event_descriptor.
*/
static void*
inner_add_mob_motion_start_event(void* arg)
{
  struct mob_motion_start_event_descriptor* stuff = (struct mob_motion_start_event_descriptor*)arg;
  eventstack_addevent(&(stuff->m->events),
		      make_event(scm_from_locale_symbol("motion-start"),
				 scm_list_n(scm_from_int(stuff->xtiles),
					    scm_from_int(stuff->ytiles),
					    scm_from_int(stuff->frames),
					    SCM_UNDEFINED)));
  return NULL;
}

/*
  Since events must be sent to the stacks when mobs start moving and stop
  moving, convenience procedures to handle this are added here.
*/
static void
add_mob_motion_start_event(mob* m, int xtiles, int ytiles, int frames)
{
  /*
    We can presume that this will only be called
    from the context of mob_move_all, that is, that 
    the mob mutex is locked.
  */
  struct mob_motion_start_event_descriptor stuff = {m,xtiles,ytiles,frames};
  scm_with_guile(inner_add_mob_motion_start_event,&stuff);
}

/*
  Workhorse procedure, to be run in guile mode.
  Adds a motion stop event and only needs the mob pointer.
  Hence, arg is a pointer to the mob this event needs to be
  added to.
*/
static void*
inner_add_mob_motion_stop_event(void* arg)
{
  eventstack_addevent(&((mob*)arg)->events,
		      make_event(scm_from_locale_symbol("motion-stop"),
				 SCM_EOL));
  return NULL;
}

static void
add_mob_motion_stop_event(mob* m)
{
  scm_with_guile(inner_add_mob_motion_stop_event,m);
}

/*
  Simple inline procedure that stops a mob.
  Since this stops the mob's movement by force,
  a stop event is automatically generated.
  Note that this event is generated even if the stop
  was spurious and the mob was not moving before.
*/
static inline void
mob_stop_movement(mob* m)
{
  m->xmoveamount = m->xmoverate = m->ymoveamount = m->ymoverate = 0;
  add_mob_motion_stop_event(m);
}

/*
  Set up movement parameters, nothing very complicated.
*/
static void
mob_move_all (mob * m, int xtiles, int ytiles, int frames)
{
  int xam = xtiles*TILE_WIDTH;
  int yam = ytiles*TILE_HEIGHT;
  double xrate = xam/frames;
  double yrate = yam/frames;
  mob_set_movement (m, xam, xrate, yam, yrate);
  add_mob_motion_start_event(m,xtiles,ytiles,frames);
}

/*
This procedure regulates all mob movement. The bits dealing with the amounts and rates are relatively straightforward. On top of those, 
it is the job of move_mob to deal with movement descriptors, i.e. once a mob is done moving and it has more movements queued up, it should
start on those. Furthermore, move_mob handles tile bounds checking for occupant purposes. This last bit is somewhat complicated.
*/
static void
move_mob (mob * m)
{
  int oldx = m->x, oldy = m->y;
  char was_moving, should_stop;
  if(!moving(*m) && m->move_descriptors.objcount)
    {
      move_descriptor md = *((move_descriptor*)m->move_descriptors.data[0].data);
      mob_move_all(m,md.tilex,md.tiley,md.frames);
      sequence_remove_at(&m->move_descriptors,0);
    }
  /*
    Store if the mob was initially moving.
    Used below to determine if it stopped.
  */
  was_moving = moving(*m);
  if (m->xmoveamount)
    {
      if (abs (m->xmoveamount) < abs (m->xmoverate))
	{
	  m->x += m->xmoveamount;
	  m->xmoveamount = m->xmoverate = 0;
	}
      else
	{
	  m->x += m->xmoverate;
	  m->xmoveamount -= m->xmoverate;
	}
    }
  if (m->ymoveamount)
    {
      if (abs (m->ymoveamount) < abs (m->ymoverate))
	{
	  m->y += m->ymoveamount;
	  m->ymoveamount = m->ymoverate = 0;
	}
      else
	{
	  m->y += m->ymoverate;
	  m->ymoveamount -= m->ymoverate;
	}
    }
  /*
    Store if stop event should be sent on this mob.
    Essentially, this is true if the mob was moving at the start of the frame
    and isn't at the end, whether for reasons of simply being done moving
    or for reasons of collisions.
    In case of collisions, we'd like this to not give any extra events, 
    so we fire the event only at the end of the procedure.
    So, first, the mob stopped naturally if it isn't moving now and was moving.
  */
  should_stop = !moving(*m) && was_moving;
  if(m->x != oldx || m->y != oldy)
    {
      /*
      Process occupants of tiles. Note that occupants are stored as pointers to the mob, i.e. as pointers to the
      actual mob data in the sequence mobs. Also note that, since GUILE cannot use those, the add_mob_collision_event does a double
      loop over the mobs sequence to find both mobs and use their indices for the event. These events are added globally,
      since they cannot really be said to be the exclusive domain of any of the two mobs.
      */
      int tilex_center = (m->x+SPRITE_WIDTH/2)/TILE_WIDTH, tiley_center = (m->y+SPRITE_HEIGHT/2)/TILE_HEIGHT;
      int tilex = m->x > oldx ? (m->x+SPRITE_WIDTH)/TILE_WIDTH : m->x < oldx ? m->x/TILE_WIDTH : tilex_center;
      int tiley = m->y > oldy ? (m->y+SPRITE_HEIGHT)/TILE_HEIGHT : m->y < oldy ? m->y/TILE_HEIGHT : tiley_center;
      int old_tilex = (oldx+SPRITE_WIDTH/2)/TILE_WIDTH, old_tiley = (oldy+SPRITE_HEIGHT/2)/TILE_HEIGHT;
      mob* occupant = NULL;
      /*Check grid bounds*/
      if(m->x < 0 || tilex >= MAIN_GRID->width || m->y < 0 || tiley >=MAIN_GRID->height)
	{
	  mob_stop_movement(m);
	  /*
	    We already stopped the mob, no event should be fired even
	    if it might otherwise be necessary to do so.
	  */
	  should_stop = 0;
	  /*Do some fixing of coordinates, to make sure the top-left corner of the sprite is at the limit of the set of allowed positions*/
	  m->x <  0 ? m->x = 0 : set_mob_tile_x(m,old_tilex);
	  m->y <  0 ? m->y = 0 : set_mob_tile_y(m,old_tiley);

	}
      else if(!(occupant = get_occupant(tilex,tiley,maingrid_index)) || occupant == m)
        {
          reset_occupant(old_tilex,old_tiley,maingrid_index);
	  /* Do tile block checking since there was no collision*/
	  if(old_tilex != tilex || old_tiley != tiley)
	    {
	      char blocking = MAIN_GRID->tilegrid[tilex][tiley].blocking;
	      char stop = 0;
	      /*Moving right,check left edge*/
	      if(tilex > old_tilex)
		{
		  stop = blocking & BLOCK_LEFT? 1: 0;
		}
	      /*Moving left*/
	      else
		stop = blocking & BLOCK_RIGHT? 1 : 0;
	      /*Moving down*/
	      if(tiley > old_tiley)
		stop = blocking & BLOCK_UP? 1 : 0;
	      /*Moving up*/
	      else
		stop = blocking & BLOCK_DOWN? 1 : 0;
	      /*This will be detected by edge checking before the center changes tiles*/
	      if(stop)
		{
		  mob_stop_movement(m);
		  /*
		    As usual, do not fire an event now.
		  */
		  should_stop = 0;
		  /*Coordinate fixing, to make sure the mob is on the OLD tile properly.*/
		  if(tilex != old_tilex)
		    set_mob_tile_x(m,old_tilex);
		  if(tiley != old_tiley)
		    set_mob_tile_y(m,old_tiley);
		}
	    }
          /*Add an event on the mob, so users can do whatever they want with this information
	   This requires some annoying shuffling of structs due to needing
	  guile mode locally.*/
          if(tilex_center != old_tilex || tiley_center != old_tiley)
	    {
	      struct tilechange_arg args = {m,old_tilex,old_tiley,m->grid,tilex,tiley,m->grid};
	      scm_with_guile(add_mob_tilechange_event,&args);
	    }
          set_mob_occupants(m);
        }
      else
        {
          mob_stop_movement(m);
	  /*
	    Do not fire an event now as the mob is being
	    stopped by force, which fires an event.
	  */
	  should_stop = 0;
	  /*Coordinate fixing, to make sure the mob is on the OLD tile properly.*/
	  if(tilex != old_tilex)
	    set_mob_tile_x(m,old_tilex);
	  if(tiley != old_tiley)
	    set_mob_tile_y(m,old_tiley);
          mob_stop_movement(occupant);
          /*
          Clear movement queues for occupant and m, so both mobs stop moving and stay still until ordered to move again by scripts.
          This is done so there is no weird behavior like mobs doing any weird, queued up movements while they should be locked in 
          a turn-based battle or something similar. The results of not doing this could be unpredictable and weird.
          */
          clear_mob_movement_queue(m);
          clear_mob_movement_queue(occupant);
          add_mob_collision_event(m,occupant);
        }
    }
  if(should_stop)
    add_mob_motion_stop_event(m);
}

void
move_mobs ()
{
  SDL_mutexP(mob_mutex);
  for (int i = 0; i < mobs.objcount; i++)
    {
      move_mob ((mob*)mobs.data[i].data);
    }
  SDL_mutexV(mob_mutex);
}

static void mob_add_movement(mob* m, int xtile, int ytile, int frames)
{
  if(!moving(*m))
    mob_move_all(m,xtile,ytile,frames);
  else
    {
      move_descriptor md;
      md.tilex = xtile;
      md.tiley = ytile;
      md.frames = frames;
      sequence_append(&(m->move_descriptors),make_move_descriptor_obj(md));
    }
}

/*Convenience functions for GUILE*/

void
mob_move_all_by_index(int ind, int xtiles, int ytiles, int frames)
{
  SDL_mutexP(mob_mutex);
  mob_move_all(mobs.data[ind].data,xtiles,ytiles,frames);
  SDL_mutexV(mob_mutex);
}

void
mob_add_movement_by_index(int ind, int xtiles, int ytiles, int frames)
{
  SDL_mutexP(mob_mutex);
  mob_add_movement(mobs.data[ind].data,xtiles,ytiles,frames);
  SDL_mutexV(mob_mutex);
}

void
mob_set_animation_by_index(int ind, unsigned int animation, unsigned int startframe, unsigned int targetframe, unsigned int framesperframe, char looping)
{
  SDL_mutexP(mob_mutex);
  mob_set_animation(mobs.data[ind].data,animation,startframe,targetframe,framesperframe,looping);
  SDL_mutexV(mob_mutex);
}

void
mob_stop_animation_by_index(int ind)
{
  SDL_mutexP(mob_mutex);
  mob_stop_animation(mobs.data[ind].data);
  SDL_mutexV(mob_mutex);
}

void
set_mob_frame_by_index(int ind, unsigned int animation, unsigned int framenum)
{
  SDL_mutexP(mob_mutex);
  ((mob*)mobs.data[ind].data)->animation = animation;
  ((mob*)mobs.data[ind].data)->frame = framenum;
  SDL_mutexV(mob_mutex);
}

void
set_mob_userdata_by_index(int ind, SCM newdata)
{
  SDL_mutexP(mob_mutex);
  scm_gc_unprotect_object (((mob *) mobs.data[ind].data)->
			   userdata);
  ((mob *) mobs.data[ind].data)->userdata = newdata;
  scm_gc_protect_object (((mob *) mobs.data[ind].data)->
			 userdata);
  SDL_mutexV(mob_mutex);
}

mob*
get_mob_by_index(int ind)
{
  return mobs.data[ind].data;
}

char
set_mob_position_by_index(int ind,int x, int y, int g)
{
  mob* m = get_mob_by_index(ind);
  int oldx = m->x , oldy = m->y , oldg = m->grid;
  if(!occupied(x,y,g))
    {
      struct tilechange_arg args = {m,oldx,oldy,oldg,x,y,g};
      lock_mobs();
      reset_occupant(oldx,oldy,oldg);
      m->x = x * TILE_WIDTH;
      m->y = y * TILE_HEIGHT;
      m->grid = g;
      set_occupant(x,y,g,m);
      scm_with_guile(add_mob_tilechange_event,&args);
      unlock_mobs();
      return 1;
    }
  else
      return 0;
}

void
stop_mob_movement_by_index(int ind)
{
  lock_mobs();
  mob_stop_movement(mobs.data[ind].data);
  clear_mob_movement_queue(mobs.data[ind].data);
  unlock_mobs();
}
