/*
Copyright Remco Bras 2007,2009
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
SDL_mutex* directive_lock;
S_CONVERTORS(directive_t,DIRECTIVE_T);
sequence directives = {0,0};

/*Define a custom getline so we don't need gcc. The semantics of the glibc variety are slightly different, but this should do for all sane cases. This may need a replacement calloc on some systems, which should be easy enough to
throw in. Much unlike the fgets it is based on, this variety of getline removes any leftover newlines. Memory allocated by this procedure should be free()d by the caller.*/
char*
getline2(FILE* stream)
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
  before the comment. Furthermore, this function tracks and returns the length of the reduced string. This is both necessary to avoid underflow and useful to avoid loading
  the file NULL.
*/
int
exclude_comments(char* str)
{
  int len = 0;
  while(*str)
    {
      if(*str == '#')
	{
	  *str = '\0';
	  break;
	}
      /*Basically an interwoven edition of strlen, to avoid looping many times over the same thing*/
      len++;
      str++;
    }
  /*
    Proceed to strip all space and space-like characters from the end of the string, starting just before the trailing \0.
   */
  str--;
  while(len && isspace(*str))
    {
      *str = '\0';
      str--;
      len--;
    }
  return len;
}

static void 
inner_register_directive(char* name, directive_t_callee func)
{
  directive_t d = {name,func};
  SDL_mutexP(directive_lock);
  sequence_append(&directives,make_directive_t_obj(d));
  SDL_mutexV(directive_lock);  
}

void
register_directive(char* name, void (*func)(char*))
{
  struct directive_t_func cfunc = {0,func};
  directive_t_callee callee;
  callee.cfunc = cfunc;
  inner_register_directive(name,callee);
}

void
register_scm_directive(char* name, SCM func)
{
  struct directive_t_scm scmfunc = {1,func};
  directive_t_callee callee;
  callee.scmfunc = scmfunc;
  inner_register_directive(name,callee);
}

/*
  Free the data allocated to directive d.
  Since C function pointers do not need freeing unless
  they have been dynamically allocated and dealing with the Guile GC
  is the job of guile_config_file.c, this only frees the name of the
  directive.
*/
static void 
free_directive(directive_t d)
{
  free(d.name);
}

/*
  Remove the ith directive.
  This does no error checking whatsoever.
*/
static inline void
remove_directive_at(int i)
{
  SDL_mutexP(directive_lock);
  free_directive(get_obj_directive_t(directives.data[i]));
  sequence_remove_at(&directives,i);
  SDL_mutexV(directive_lock);
}

/*
  Remove all directives named name.
*/
void
remove_directive(char* name)
{
  for(int i = 0; i < directives.objcount; i++)
    {
      char* str = get_obj_directive_t(directives.data[i]).name;
      if(!strcmp(str,name))
	{
	  remove_directive_at(i);
	  i--;
	}
    }
}

void
directives_init()
{
  directives = sequence_init();
  directive_lock = SDL_CreateMutex();
  register_directive(strdup("include"),exec_config_file);
  register_directive(strdup("scheme-dir"),add_scheme_dir);
  register_directive(strdup("image-dir"),add_image_dir);
  register_directive(strdup("font-dir"),add_font_dir);
}

void 
funcall_directive(directive_t directive, char* data)
{
  directive_t_callee callee = directive.func;
  if(callee.scmp)
    scm_apply_1(callee.scmfunc.lambda,scm_from_locale_string(data),SCM_EOL);
  else
    callee.cfunc.func(data);
}

/*
  This one does a data-directed dispatch over directive, calling the function found
  with data. The point here is to make a pluggable framework. (As a bonus,
  the chosen convention happens to allow shoving exec_config_file in as the 
  handler for "include")
*/
void
handle_colon_directive(char* directive, char* data)
{
  char* str;
  for(int i = 0; i < directives.objcount; i++)
    {
      str = ((directive_t*)directives.data[i].data)->name;
      if(!strcmp(str,directive))
	{
	  funcall_directive(get_obj_directive_t(directives.data[i]),data);
	}
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
    {
      fprintf(stderr,"RPGE: Cannot find configuration file %s\n",filename);
      return;
    }
  char* str,*colon;
  int len;
  while((str = getline2(file)))
    {
      if(str[0] != 0 && str[0] != '#')
	{
	  if((len = exclude_comments(str)))
	    {
	      colon = strchr(str,':');
	      if(colon)
		{
		  /*Split string at colon*/
		  *colon = '\0';
		  /*Send the data off for processing*/
		  handle_colon_directive(str,colon + 1);
		}
	      else
		{
		  char* path = get_path(scheme_paths,str);
		  if(path)
		    scm_c_safe_load(path);
                  else
                    fprintf(stderr,"RPGE: Cannot find Scheme source file in . or Scheme search paths: %s\n",str);
		  if(path != str)
		    free(path);
		}
	    }
	}
      free(str);
    }
}
