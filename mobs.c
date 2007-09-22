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
mobstack mobs = { 0, 0 };

mob
create_mob_using_sprite (unsigned int x, unsigned int y, char *sprity)
{
  mob mobby;
  mobby.imgindex = push_image_on_stack (sprity);
  mobby.x = x;
  mobby.y = y;
  mobby.xpixelalignment = mobby.ypixelalignment = 0;
  memset (((char *) &mobby) + sizeof (unsigned int) * 2 + sizeof (float) * 2 +
	  sizeof (SDL_Surface *), 0,
	  sizeof (mob) - sizeof (unsigned int) * 2 - sizeof (float) * 2 -
	  sizeof (SDL_Surface *));
  return mobby;
}

int
find_empty_mob()
{
  for(int i = 0; i < mobs.size; i++)
     {
       if(mobs.mobs[i].imgindex == -1)
         return i;
     }
  return -1;
}

int
push_mob_on_array (mob m)
{
  int indexempty = find_empty_mob();
  if(indexempty != -1)
    {
      mobs.mobs[indexempty] = m;
      return indexempty;
    }
  else
    {
      mob *newmobs = malloc (sizeof (mob) * (mobs.size + 1));
      memcpy (newmobs, mobs.mobs, sizeof (mob) * mobs.size);
      newmobs[mobs.size] = m;
      free (mobs.mobs);
      mobs.mobs = newmobs;
      mobs.size++;
      return mobs.size - 1;
    }
}

char
mob_equal(mob m, mob n)
{
  return (m.x == n.x && m.y == n.y && m.xpixelalignment == n.xpixelalignment && m.ypixelalignment == n.ypixelalignment && m.imgindex == n.imgindex && m.animation == n.animation && m.frame == n.frame && m.targetframe == n.targetframe   && m.xmoverate == n.xmoverate && m.xmovetime == n.xmovetime && m.ymoverate == n.ymoverate && m.ymovetime == n.ymovetime);
}

int
find_mob (mob m)
{
  printf("Finding in size %d\n",mobs.size);
  for(int i = 0; i < mobs.size; i++)
    {
      if(mob_equal(mobs.mobs[i],m))
        return i;
    }
  return -1;
}

void
remove_mob (mob m)
{
  int index = find_mob(m);
  printf("%d\n",index);
  if(mobs.size == 0 || index == -1)
    return; 
  if(index != -1)
    {
      mobs.mobs[index].imgindex = -1;
    }
}

void
animate_mob(mob* m)
{
  if(m->timetonextframe == -1)
    return;
  m->timetonextframe--;
  if(!m->timetonextframe)
    {
      if(m->resetonnext)
        {
          m->frame = 0;
          m->timetonextframe = m->initialtimetonextframe;
          m->resetonnext = 0;
          return;
        }
      m->frame++;
      if(m->frame == m->targetframe)
        {
          if(m->animlooping)
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

    }

}

void 
animate_mobs()
{
  for(int i = 0; i < mobs.size; i++)
    {
      animate_mob(&mobs.mobs[i]);
    }
}


void
mob_set_animation(mob* m, unsigned int animation, unsigned int startframe, unsigned int targetframe, unsigned int framesperframe, char looping)
{
  m->animation = animation;
  m->frame = startframe;
  m->targetframe = targetframe;
  m->timetonextframe = m->initialtimetonextframe = framesperframe;
  m->animlooping = looping;
}