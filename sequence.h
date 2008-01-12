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

/*prototype sequence header*/
#ifndef SEQ_H
#define SEQ_H
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <SDL/SDL.h>
  
#define TYPE_IMAGE 0  
#define TYPE_MOB 1
#define TYPE_EVENT 2
#define TYPE_WINDOW 3
#define TYPE_INT 4
#define TYPE_UINT32 5
#define TYPE_ARGV 6
#define TYPE_FONT 7
#define TYPE_STRING 8
#define TYPE_TEXT 9
#define TYPE_DISPATCH_PAIR 10
  
typedef struct
{
  void *data;			
  unsigned long typeinfo;	
} object;

typedef struct
{
  unsigned long objcount;
  object *data;
} sequence;

sequence sequence_init(void);
int sequence_append (sequence *seq, object data);
int sequence_position (sequence seq, object data, char (* eqproc) (object, object));
object sequence_find (sequence seq, object data, char (* eqproc) (object, object));
void sequence_remove_at(sequence *seq, int index);
void sequence_remove (sequence *seq, object data, char (* eqproc) (object, object));
sequence sequence_map (sequence seq, object (*proc) (object));
void sequence_foreach (sequence seq, void (*proc) (object));
void sequence_free (sequence seq);
object sequence_reduce(sequence seq, object (* proc)(object, object));
object make_int_obj(int i);
int get_obj_int(object o);
object make_uint32_obj (Uint32 u);
Uint32 get_obj_uint32(object o);
object make_string_obj(char*);
char* get_obj_string(object);

#endif
