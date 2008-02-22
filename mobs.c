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
convertors(move_descriptor);
convertors(mob);

mob
create_mob_using_sprite (unsigned x, unsigned y, char *sprity)
{
  mob mobby;
  mobby.imgindex = push_image_on_stack (sprity);
  mobby.x = x * TILE_WIDTH;
  mobby.y = y * TILE_HEIGHT;
  mobby.xpixelalignment = mobby.ypixelalignment = 0;
  memset (((char *) &mobby) + sizeof (unsigned int) * 2 + sizeof (float) * 2 +
	  sizeof (SDL_Surface *), 0,
	  sizeof (mob) - sizeof (unsigned int) * 2 - sizeof (float) * 2 -
	  sizeof (SDL_Surface *));
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

void
set_mob_occupants(mob* m)
{
  int tilex = (m->x+SPRITE_WIDTH/2)/TILE_WIDTH, tiley = (m->y+SPRITE_HEIGHT/2)/TILE_HEIGHT;
  set_occupant(tilex,tiley,m);
}

int
push_mob_on_array (mob m)
{
  int indexempty = find_empty_mob ();
  if (indexempty != -1)
    {
      *((mob*)mobs.data[indexempty].data) = m;
      set_mob_occupants(mobs.data[indexempty].data);
      return indexempty;
    }
  else
    {
      int index = sequence_append(&mobs,make_mob_obj(m));
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
      reset_occupant(tilex,tiley);
      ((mob*)mobs.data[index].data)->imgindex = -1;
    }
}

inline char
moving(mob m)
{
  return (m.xmoveamount && m.xmoverate) || (m.ymoveamount && m.ymoverate);
}

void
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
  for (int i = 0; i < mobs.objcount; i++)
    {
      animate_mob ((mob*)mobs.data[i].data);
    }
}

inline void
mob_stop_movement(mob* m)
{
  m->xmoveamount = m->xmoverate = m->ymoveamount = m->ymoverate = 0;
}

void
add_mob_collision_event(mob* mob1, mob* mob2)
{
  int index1 = -1, index2 = -1;
  for(int i = 0; i < mobs.objcount ; i++)
    {
      if(mobs.data[i].data == mob1)
        index1 = i;
      else if(mobs.data[i].data == mob2)
        index2 = i;
    }
  eventstack_addevent(&global_usereventstack,make_event(scm_from_locale_symbol("collision"),scm_cons(scm_from_int(index1),scm_from_int(index2))));
}

void
add_mob_tilechange_event(mob* m, int oldx, int oldy, int newx, int newy)
{
  SCM type = scm_from_locale_symbol("tile-change"), data = scm_cons(scm_cons(scm_from_int(oldx),scm_from_int(oldy)),scm_cons(scm_from_int(newx),scm_from_int(newy)));
  eventstack_addevent(&(m->events),make_event(type,data));
}

/*
This procedure regulates all mob movement. The bits dealing with the amounts and rates are relatively straightforward. On top of those, 
it is the job of move_mob to deal with movement descriptors, i.e. once a mob is done moving and it has more movements queued up, it should
start on those. Furthermore, move_mob handles tile bounds checking for occupant purposes. This last bit is somewhat complicated.
*/
void
move_mob (mob * m)
{
  int oldx = m->x, oldy = m->y;
  if(!moving(*m) && m->move_descriptors.objcount)
    {
      move_descriptor md = *((move_descriptor*)m->move_descriptors.data[0].data);
      mob_move_all(m,md.tilex,md.tiley,md.frames);
      sequence_remove_at(&m->move_descriptors,0);
    }
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
  if(m->x != oldx || m->y != oldy)
    {
      /*
      Process occupants of tiles. Note that occupants are stored as pointers to the mob, i.e. as pointers to the
      actual mob data in the sequence mobs. Also note that, since GUILE cannot use those, the add_mob_collision_event does a double
      loop over the mobs sequence to find both mobs and use their indices for the event. These events are added globally,
      since they cannot really be said to be the exclusive domain of any of the two mobs.
      */
      int tilex = m->x > oldx ? (m->x+SPRITE_WIDTH)/TILE_WIDTH : m->x < oldx ? m->x/TILE_WIDTH : (m->x+SPRITE_WIDTH/2)/TILE_WIDTH;
      int tiley = m->y > oldy ? (m->y+SPRITE_HEIGHT)/TILE_HEIGHT : m->y < oldy ? m->y/TILE_HEIGHT : (m->y+SPRITE_HEIGHT/2)/TILE_HEIGHT;
      int old_tilex = (oldx+SPRITE_WIDTH/2)/TILE_WIDTH, old_tiley = (oldy+SPRITE_HEIGHT/2)/TILE_HEIGHT;
      mob* occupant = NULL;
      if(!(occupant = get_occupant(tilex,tiley)) || occupant == m)
        {
          reset_occupant(old_tilex,old_tiley);
          /*Add an event on the mob, so users can do whatever they want with this information*/
          if(tilex != old_tilex || tiley != old_tiley)
	    add_mob_tilechange_event(m,old_tilex,old_tiley,tilex,tiley);
          set_mob_occupants(m);
        }
      else
        {
          mob_stop_movement(m);
          mob_stop_movement(occupant);
          add_mob_collision_event(m,occupant);
        }
    }
}

void
move_mobs ()
{
  for (int i = 0; i < mobs.objcount; i++)
    {
      move_mob ((mob*)mobs.data[i].data);
    }
}

void
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

void
mob_stop_animation(mob* m)
{
  m->targetframe = m->frame;
  m->timetonextframe = -1;
  m->animlooping = 0;
}

void
mob_set_movement (mob * m, int xam, double xrate, int yam, double yrate)
{
  m->xmoveamount = xam;
  m->xmoverate = xrate;
  m->ymoveamount = yam;
  m->ymoverate = yrate;
}

/*This function acts as a preprocessor to move_mob, so move_mob doesn't have to worry
  about blocking tiles and the like. The downside of this is that, in theory, mobs moving
  over tiles while those tiles are being changed will exhibit some erroneous behavior.
  It is possible to marginalize this by queueing up short movements rather than long
  sweeping motions and taking care to not change tiles under the feet of any moving mobs.
  The one thing we should expect to change a lot is the set of positions of mobs, wherefore
  it is the duty of move_mob to perform checks for mob collision. 
*/
void
mob_move_all (mob * m, int xtiles, int ytiles, int frames)
{
  int xrate = xtiles * TILE_WIDTH / frames;
  int yrate = ytiles * TILE_HEIGHT / frames;
  int xam = 0;
  int yam = 0;
  int mobtilexold, mobtilex = (m->x + HALF_TILE_WIDTH) / TILE_WIDTH, mobtileyold, mobtiley = (m->y + HALF_TILE_HEIGHT) / TILE_HEIGHT, mobxnew = m->x, mobynew = m->y;
  char block = 0, oob_down,oob_up,oob_right,oob_left;
  for (int i = 1; i <= frames; i++)
    {
      mobxnew += xrate;
      mobynew += yrate;
      mobtilexold = mobtilex;
      mobtileyold = mobtiley;
      /*The following makes sure the right edge is checked, i.e. the edge most likely to move to a new tile*/
      mobtilex = xrate > 0? mobxnew/TILE_HEIGHT+1: xrate < 0? mobxnew/TILE_HEIGHT : (mobxnew+HALF_TILE_WIDTH)/TILE_WIDTH;
      mobtiley = yrate > 0? mobynew/TILE_HEIGHT+1: yrate < 0? mobynew/TILE_HEIGHT : (mobynew+HALF_TILE_HEIGHT)/TILE_HEIGHT;
      if((oob_left = (mobxnew < 0)) ||
         (oob_up =  (mobynew < 0)) ||
         (oob_right = (mobxnew + TILE_WIDTH >= main_grid.width * TILE_WIDTH)) ||
         (oob_down =  (mobynew + TILE_HEIGHT >= main_grid.height * TILE_HEIGHT)))
	{
          if(oob_left)
            {
	      xam += xrate - mobxnew;
            }
          else if(oob_right)
	    {
              xam += xrate - (mobxnew+TILE_WIDTH-main_grid.width*TILE_WIDTH)-1/*1-pixel safety margin #1*/;
            }
	  else if(oob_up)
	    {
	      yam += yrate - mobynew;
	    }
          else if(oob_down)
	    {
	      yam += yrate - (mobynew+TILE_HEIGHT-main_grid.height*TILE_HEIGHT)-1/*1-pixel safety margin #1*/;
	    }
	  if(!(oob_down || oob_up))
            {
	      yam += yrate;
            }
	  if(!(oob_left || oob_right))
            {
              xam += xrate;
            }
	  break;
        }
      if (mobtilex != mobtilexold || mobtiley != mobtileyold)
	{     
	  block = main_grid.tilegrid[mobtilex][mobtiley].blocking;
	  if ((mobtilex < mobtilexold && (block & BLOCK_RIGHT)) ||
	      (mobtilex > mobtilexold && (block & BLOCK_LEFT)) ||
	      (mobtiley < mobtileyold && (block & BLOCK_DOWN)) ||
	      (mobtiley > mobtileyold && (block & BLOCK_UP)))
            {
	      break;
            }
          else
            {
              xam += xrate;
              yam += yrate;
            }
	}
      else
	{
	  xam += xrate;
	  yam += yrate;
	}
    }
  mob_set_movement (m, xam, xrate, yam, yrate);
}

void mob_add_movement(mob* m, int xtile, int ytile, int frames)
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
