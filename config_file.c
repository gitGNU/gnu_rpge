/*
Copyright Remco Bras 2007
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

/*
config_file.c: declare a function or two to load scheme files from a configuration file.
*/

#include "config_file.h"


/*Define a custom getline so we don't need gcc. The semantics of the glibc variety are slightly different, but this should do for all sane cases. This may need a replacement calloc on some systems, which should be easy enough to
throw in. Much unlike the fgets it is based on, this variety of getline removes any leftover newlines. Memory allocated by this procedure should be free()d by the caller.*/
char*
getline(FILE* stream)
{
   unsigned int size = 0;
   char* string = (char*) xcalloc(BLOCK_SIZE,sizeof(char)), *status = NULL;
   while(1)
     {
       status = fgets(string+size,BLOCK_SIZE,stream);
       if(!status)
         {
           if(!size)
             {
               free(string);
               return NULL;
             }
           else
             return string;
         }
         size += strlen(string+size);
         if(string[size-1] == '\n')
           {
             string[size-1] = '\0';
             return string;
           }
         string = xrealloc(string,size+BLOCK_SIZE);
     }
}

/*This function modifies its first argument, stripping comments (everything after the very first #, so multiline strings are unsupported) and trailing whitespace just
  before the comment.
*/
void
exclude_comments(char* str)
{
  while(*str)
    {
      if(*str == '#')
	{
	  *str = '\0';
	  break;
	}
      str++;
    }
  /*Proceed to strip all space and space-like characters from the end of the string, starting at just before the trailing null*/
  str--;
  while(*str)
    {
      if(*str == ' ' || *str == '\t' || *str == '\n' || *str == '\v')
	{
	  *str = '\0';
	  str--;
	}
      else
	break;
    }
}

/*Probably a misnomer, but this one executes all files named in a
file,one name per line, excluding those lines which happen to start
with '#'. Furthermore, it filters out comments and superfluous trailing spaces after filenames.*/
void
exec_config_file(char* filename)
{
  FILE* file = fopen(filename,"rt");
  if(!file)
    return;
  char* str;
  while((str = getline(file)))
    {
      if(str[0] != 0 && str[0] != '#')
	{
	  exclude_comments(str);
	  scm_c_safe_load(str);
	}
      free(str);
    }
}
