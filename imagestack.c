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

#include "imagestack.h"

sequence images = {0,0};

image
make_image (SDL_Surface* data, char* filename)
{
  image i;
  i.data = data;
  i.filename = filename;
  return i;
}

object
make_image_obj(image i)
{
  object o;
  o.typeinfo = TYPE_IMAGE;
  o.data = malloc(sizeof(image));
  *((image*)o.data)=i;
  return o;
}

int 
find_image(char* filename)
{
  for(int i = 0; i < images.objcount; i++)
    {
      if(filename)
        {
        if(((image*)images.data[i].data)->filename && !strcmp(((image*)images.data[i].data)->filename,filename))
            return i;
        }
      else
        {
          if(!((image*)images.data[i].data)->filename )
            return i;
        }
    }
  return -1;
}

int
find_empty()
{
  return find_image(NULL);
}

int 
push_image_on_stack(char* filename)
{
  int index  = find_image(filename), indexempty = find_empty();
  if(indexempty != -1)
    {
      images.data[indexempty] = make_image_obj(make_image(load_image(filename),filename));
      return indexempty;
    }
  else
    {
       return sequence_append(&images,make_image_obj(make_image(load_image(filename),filename)));
    }
}

void
remove_image (char* filename)
{
  int index = find_image(filename);
  if(index == -1 || !images.objcount || !images.data)
    return;
  //Should free the filename here, tends to cause crashes
  ((image*)images.data[index].data)->data = NULL;
  ((image*)images.data[index].data)->filename = NULL;
  images.data[index].data = NULL; 
}
