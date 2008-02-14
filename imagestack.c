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

#include "imagestack.h"

sequence images = {0,0};
convertors(image);

/*
The SDL_Surface* data is usually passed from load_image in video.c or a similar procedure, forcing it to be properly optimized for blitting on our output surface.
*/
image
make_image (SDL_Surface* data, char* filename)
{
  image i;
  i.data = data;
  i.filename = filename;
  return i;
}

/*
The explicit check for being passed NULL seems redundant, but needs to be covered to make find_empty easier to write. Other than that, this procedure gracefully handles holes in the images sequence, a necessary feature to keep indices used for images therein constant.
*/
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

/*
A convenience function to make loading images easier. It takes advantage of holes in the images sequence, filling them up. Indices returned from this procedure are guaranteed to be constant, barring corruption of the sequence itself.
*/
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
  /*Should free the filename here, tends to cause crashes, SHOULD also free the surface, need to look into why this doesn't do that.*/
  ((image*)images.data[index].data)->data = NULL;
  ((image*)images.data[index].data)->filename = NULL;
  images.data[index].data = NULL; 
}
