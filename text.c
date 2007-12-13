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

font make_font(TTF_Font* fontptr, char* filename)
{
  font returnfont;
  returnfont.font = fontptr;
  returnfont.filename = filename;
  return returnfont;
}

char object_font_filename_eq(object font1, object font2)
{
  return !strcmp(((font*)font1.data)->filename,((font*)font2.data)->filename);
}

object make_font_obj(font f)
{
  object o;
  o.data = malloc (sizeof(font));
  *((font*)o.data)=f;
  o.typeinfo = TYPE_FONT;
  return o;
}

int open_font(char* filename, int size)
{
  font font_to_add;
  int index;
  font_to_add.filename = filename;
  object o = make_font_obj(font_to_add);
  index = sequence_position(fonts,o,object_font_filename_eq);
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
          *((font*)o.data) = font_to_add;
          return sequence_append(&fonts, o);
        }
    }
}
