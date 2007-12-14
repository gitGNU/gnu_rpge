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

#include "mobs.h"
sequence mobs;

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
  mobby.userdata = SCM_UNDEFINED;
  return mobby;
}

object 
make_mob_obj(mob m)
{
  object o;
  o.typeinfo = TYPE_MOB;
  o.data = malloc(sizeof(mob));
  *((mob*)o.data) = m;
  return o;
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

int
push_mob_on_array (mob m)
{
  int indexempty = find_empty_mob ();
  if (indexempty != -1)
    {
      *((mob*)mobs.data[indexempty].data) = m;
      return indexempty;
    }
  else
    {
      return sequence_append(&mobs,make_mob_obj(m));
    }
}

void
remove_mob (int index)
{
  if (index < mobs.objcount)
    {
      ((mob*)mobs.data[index].data)->imgindex = -1;
    }
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

void
move_mob (mob * m)
{
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


//Warning, extremely inaccurate function.
void
mob_move_all (mob * m, int xtiles, int ytiles, int frames)
{
  int xrate = xtiles * TILE_WIDTH / frames;
  int yrate = ytiles * TILE_HEIGHT / frames;
  int xam = 0;
  int yam = 0;
  int mobtilexold, mobtilex = m->x / TILE_WIDTH, mobtileyold, mobtiley = m->y / TILE_HEIGHT;
  char block = 0;
  for (int i = 1; i <= frames; i++)
    {
      mobtilexold = mobtilex;
      mobtileyold = mobtiley;
      mobtilex = (m->x + (i * xrate)) / TILE_WIDTH;
      mobtiley = (m->y + (i * yrate)) / TILE_HEIGHT;
      //tile boundary reached, check
      if ((mobtilex != mobtilexold || mobtiley != mobtileyold) && !(mobtilex < 0 || mobtilex > main_grid.width || mobtiley < 0 || mobtiley > main_grid.height))
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
