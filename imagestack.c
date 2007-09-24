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

imagestack images = {0,0};

image
make_image (SDL_Surface* data, char* filename)
{
  image i;
  i.data = data;
  i.filename = filename;
  return i;
}

int 
find_image(char* filename)
{
  for(int i = 0; i < images.size; i++)
    {
      if(filename)
        {
          if(images.images[i].filename && !strcmp(images.images[i].filename,filename))
            return i;
        }
      else
        {
          if(!images.images[i].filename )
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
  image* newimages;
  if(index != -1)
    return index;
  if(indexempty != -1)
    {
      images.images[indexempty] = make_image(load_image(filename),filename);
      return indexempty;
    }
  else
    {
      index = images.size;
      newimages = malloc(sizeof(image)*(images.size+1));
      memcpy(newimages,images.images,sizeof(image)*images.size);
      newimages[index] = make_image(load_image(filename),filename);
      free(images.images);
      images.images = newimages;
      images.size++;
      return index;
    }
}

void
remove_image (char* filename)
{
  int index = find_image(filename);
  if(index == -1 || !images.size || !images.images)
    return;
  //Should free the filename here, tends to cause crashes
  images.images[index].data = NULL; 
  images.images[index].filename = NULL;
}
