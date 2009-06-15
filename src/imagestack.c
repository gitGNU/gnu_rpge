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
SDL_mutex* image_mutex;
S_CONVERTORS(image,IMAGE);

#define ADD_REF(n) ((image*)images.data[n].data)->count++
#define DEC_REF(n) ((image*)images.data[n].data)->count--
#define GET_REF(n) ((image*)images.data[n].data)->count
#define GET_PERM(n) ((image*)images.data[n].data)->permanence

/*The usual initialization function, for... abstraction and convenience purposes*/
void
images_init(void)
{
  images = sequence_init();
  image_mutex = SDL_CreateMutex();
}

/*
The SDL_Surface* data is usually passed from load_image in video.c or a similar procedure, forcing it to be properly optimized for blitting on our output surface.
*/
image
make_image (SDL_Surface* data, char* filename)
{
  image i;
  i.data = data;
  i.filename = filename;
  i.count = i.permanence = 0;
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
Note: This function MUST be called with a string allocated by malloc or a variant thereof (or implicitly that way using scm_to_locale_string, strdup and so on). If this is not done, remove_image will crash when called to remove this image.
*/
int 
push_image_on_stack(char* filename)
{
  int index  = find_image(filename), indexempty = find_empty();
  char* path = get_path(image_paths,filename);
  if(!path)
    {
      fprintf(stderr,"RPGE: Cannot find image in . or image paths: %s\n",filename);
      return -1;
    }
  else if(index != -1)
    {
      SDL_mutexP(image_mutex);
      ADD_REF(index);
      SDL_mutexV(image_mutex);
      return index;
    }
  else if(indexempty != -1)
    {
      SDL_mutexP(image_mutex);
      free_obj(images.data[indexempty]);
      images.data[indexempty] = make_image_obj(make_image(load_image(path),filename));
      ADD_REF(indexempty);
      SDL_mutexV(image_mutex);
      if(path != filename)
	free(path);
      return indexempty;
    }
  else
    {
      SDL_mutexP(image_mutex);
      int returnval = sequence_append(&images,make_image_obj(make_image(load_image(path),filename)));
      ADD_REF(returnval);
      SDL_mutexV(image_mutex);
      if(path != filename)
	free(path);
      return returnval;
    }
}

static void
remove_image_at(int index)
{
  if(index == -1 || !images.objcount || !images.data)
    return;
  /*If we've reached this point, we're going to make changes, so lock the image sequence*/
  SDL_mutexP(image_mutex);
  SDL_FreeSurface(((image*)images.data[index].data)->data);
  free(((image*)images.data[index].data)->filename);
  ((image*)images.data[index].data)->data = NULL;
  ((image*)images.data[index].data)->filename = NULL;
  SDL_mutexV(image_mutex);
}

void
remove_image (char* filename)
{
  int index = find_image(filename);
  remove_image_at(index);
}

/*Intended for outside use, this 'releases' a reference to an image, cleaning it up if needed*/
void
release_image(int index)
{
  if(index == -1 || !images.objcount || !images.data || index >= images.objcount)
    return;
  /*Lock as usual*/
  SDL_mutexP(image_mutex);
  DEC_REF(index);
  if(!GET_REF(index) && !GET_PERM(index))
    {
      remove_image_at(index);
    }
  SDL_mutexV(image_mutex);
}

void
release_image_with_name(char* filename)
{
  int index = find_image(filename);
  release_image(index);
}

inline char*
get_image_name(int index)
{
  return ((image*)images.data[index].data)->filename;
}
