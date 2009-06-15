/*
Copyright Remco Bras 2007,2008
This file is part of RPGE

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

#include "text.h"

sequence fonts;
sequence texts;
S_CONVERTORS(font,FONT);
S_CONVERTORS(text,TEXT);
SDL_mutex *font_mutex,*text_mutex;

void
text_rendering_init()
{
  texts = sequence_init();
  fonts = sequence_init();
  font_mutex = SDL_CreateMutex();
  text_mutex = SDL_CreateMutex();
}

font
make_font(TTF_Font* fontptr, char* filename,unsigned int size)
{
  font returnfont;
  returnfont.font = fontptr;
  returnfont.filename = filename;
  returnfont.size = size;
  return returnfont;
}

char
object_font_filename_and_size_eq(object font1, object font2)
{
  return (!strcmp(((font*)font1.data)->filename,((font*)font2.data)->filename) &&
          ((font*)font1.data)->size == ((font*)font2.data)->size);
}

int
find_empty_font(void)
{
  for(int i = 0; i < fonts.objcount; i++)
    {
      if(!((font*)fonts.data[i].data)->size)
        return i;
    }
  return -1;
}

int
open_font(char* filename, int size)
{
  font font_to_add;
  int index;
  font_to_add.filename = filename;
  font_to_add.size = size;
  font_to_add.font = NULL;
  object o = make_font_obj(font_to_add);
  index = sequence_position(fonts,o,object_font_filename_and_size_eq);
  if(index != -1)
    {
      free(o.data);
      return index;
    }
  else
    {
      char* new_filename = get_path(font_paths,filename);
      font_to_add.font = TTF_OpenFont(new_filename,size);
      free(new_filename);
      if(!font_to_add.font)
        return -1;
      else
        {
          int empty_font = find_empty_font();
          if(empty_font != -1)
            {
              free(o.data);
	      SDL_mutexP(font_mutex);
              *((font*)fonts.data[empty_font].data) = font_to_add;
	      SDL_mutexV(font_mutex);
              return empty_font;
            }
          else
            {
              *((font*)o.data) = font_to_add;
	      SDL_mutexP(font_mutex);
              int returnval = sequence_append(&fonts, o);
	      SDL_mutexV(font_mutex);
	      return returnval;
            }
        }
    }
}

void
close_font(int index)
{
  if(index >= 0 && index < fonts.objcount)
    {
      SDL_mutexP(font_mutex);
      ((font*)fonts.data[index].data)->size = 0;
      SDL_mutexV(font_mutex);
    }
}

/*
Technically, splitting width-based would be roughly more complicated, unless monospaced fonts are used. However, if we wish to properly support text in windows, we should do something about that some other time.

String is copied by make_text, with the copy being quite reasonably destroyed. The actual 
tokens stored in the buffers sequence are separate, malloced,freeable buffers,
all of which should be destroyed by the caller once this text is no longer needed.

String itself is left to the caller.
*/

text 
make_text(unsigned int x, unsigned int y, char* string, int fontindex, SDL_Color color)
{
  text t;
  t.x = x;
  t.y = y;
  t.fontindex = fontindex;
  t.color = color;
  if(string)
    {
      string = strdup(string);
      char* saveptr = NULL;
      t.buffers = sequence_init();
      char* token = strtok_r(string,"\n",&saveptr);
      if(token)
	sequence_append(&t.buffers,make_string_obj(strdup(token)));
      else
        {
          return t;
        }
      while((token = strtok_r(NULL,"\n",&saveptr)))
        {
          sequence_append(&t.buffers,make_string_obj(strdup(token)));
        }
      free(string);
      return t;
    }
  else
    {
      t.buffers = sequence_init();
      return t;
    }
}

void
print_string_obj(object o)
{
  if(o.data)
    {
      puts(*((char**)o.data));
    }
}

void
print_text(text t)
{
  sequence_foreach(t.buffers,print_string_obj);
}

int
find_empty_text(void)
{
  for(int i = 0; i < texts.objcount; i++)
    {
      if(((text*)texts.data[i].data)->fontindex == -1)
        return i;
    }
  return -1;
}

int
add_text(text t)
{
  int indexempty = find_empty_text();
  if(indexempty != -1)
    {
      SDL_mutexP(text_mutex);
      *((text*)texts.data[indexempty].data) = t;
      SDL_mutexV(text_mutex);
      return indexempty;
    }
  else
    {
      SDL_mutexP(text_mutex);
      int returnval =  sequence_append(&texts,make_text_obj(t));
      SDL_mutexV(text_mutex);
      return returnval;
    }
}

void
remove_text(int index)
{
  SDL_mutexP(text_mutex);
  for(int i = 0; i < ((text*)texts.data[index].data)->buffers.objcount; i++)
    free(*((char**)((text*)texts.data[index].data)->buffers.data[i].data));
  sequence_free(((text*)texts.data[index].data)->buffers);
  ((text*)texts.data[index].data)->fontindex = -1;
  SDL_mutexV(text_mutex);
}

void
move_text(int index,unsigned int x,unsigned int y)
{
  text* t = texts.data[index].data;
  SDL_mutexP(text_mutex);
  t->x = x;
  t->y = y;
  SDL_mutexV(text_mutex);
}
