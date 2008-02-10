/*
Copyright Remco Bras 2007
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
convertors(font);
convertors(text);

font
make_font(TTF_Font* fontptr, char* filename)
{
  font returnfont;
  returnfont.font = fontptr;
  returnfont.filename = filename;
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
  object o = make_font_obj(font_to_add);
  index = sequence_position(fonts,o,object_font_filename_and_size_eq);
  if(index != -1)
    {
      free(o.data);
      return index;
    }
  else
    {
      font_to_add.font = TTF_OpenFont(filename,size);
      if(!font_to_add.font)
        return -1;
      else
        {
          int empty_font = find_empty_font();
          if(empty_font != -1)
            {
              free(o.data);
              *((font*)fonts.data[empty_font].data) = font_to_add;
              return empty_font;
            }
          else
            {
              *((font*)o.data) = font_to_add;
              return sequence_append(&fonts, o);
            }
        }
    }
}

void
close_font(int index)
{
  if(index >= 0 && index < fonts.objcount)
    ((font*)fonts.data[index].data)->size = 0;
}

/*
Technically, splitting width-based would be roughly more complicated, unless monospaced fonts are used. However, if we wish to properly support text in windows, we should do something about that some other time.

WARNING: Texts do not manage the memory of their actual buffers, they ONLY manage the pointers (as per the sequence demand that any and all data of any and all objects should be freeable it's safer to store char**s as opposed to the buffers themselves). The buffers should be managed by their users at all times.
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
      char* saveptr;
      t.buffers = sequence_init();
      char* str = strdup(string);
      char* token = strtok_r(str,"\n",&saveptr);
      if(token)
        sequence_append(&t.buffers,make_string_obj(token));
      else
        {
          return t;
        }
      while(token = strtok_r(NULL,"\n",&saveptr))
        {
          sequence_append(&t.buffers,make_string_obj(token));
        }
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
      *((text*)texts.data[indexempty].data) = t;
      return indexempty;
    }
  else
    return sequence_append(&texts,make_text_obj(t));
}

void
remove_text(int index)
{
  sequence_free(((text*)texts.data[index].data)->buffers);
  ((text*)texts.data[index].data)->fontindex = -1;
}

