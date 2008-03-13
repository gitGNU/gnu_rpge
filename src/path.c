/*
Copyright Remco Bras 2008
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

#include "path.h"
/*
  path.c:Implement path system
*/

sequence image_paths, scheme_paths;

/*
  Initialize necessary sequences.
*/
void 
paths_init(void)
{
  image_paths = scheme_paths = sequence_init();
}

/*
  Add path to the list of allowable directories in pathgroup.
*/
void 
add_path(sequence* pathgroup, char* path)
{
  sequence_append(pathgroup,make_string_obj(path));
}

/*
  Check if file can be opened, if so, return true.
*/
char
fexists(char* file)
{
  /*Presume that reading in text mode or reading in binary mode will not cause any differences to the existence of the file*/
  FILE* f = fopen(file,"rt");
  if(f)
    {
      fclose(f);
      return 1;
    }
  /*Otherwise, since f is a NULL FILE*, don't bother closing*/
  else
    return 0;
}

/*
  Return a string allocated with malloc that contains the filename of file, with the first allowable path appended.
*/
char* 
get_path(sequence pathgroup, char* filename)
{
  char* buffer,*path;
  int len,pathlen;
  if(fexists(filename))
    return filename;
  len = strlen(filename);
  for(int i = 0; i < pathgroup.objcount; i++)
    {
      path = get_obj_string(pathgroup.data[i]);
      if(path)
        {
          pathlen = strlen(path);
          buffer = xcalloc(pathlen+len+1,sizeof(char));
          strcpy(buffer,path);
          strcpy(buffer+pathlen,filename);
          if(fexists(buffer))
            return buffer;
          else
            free(buffer);
        }
    }
  return NULL;
}

/*
  Necessary to add a simple function to the directives, nothing special
*/
void
add_scheme_dir(char* dir)
{
  add_path(&scheme_paths,strdup(dir));
}

void
add_image_dir(char* dir)
{
  add_path(&image_paths,strdup(dir));
}
