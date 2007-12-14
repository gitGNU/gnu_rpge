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
 
#include "sequence.h"

sequence 
sequence_init(void)
{
  sequence s;
  s.data = NULL;
  s.objcount = 0;
  return s;
}

int
sequence_append (sequence* seq, object data)
{
  object* result = realloc(seq->data,(seq->objcount+1)*sizeof(object));
  if(!result)
    return -1;
  else
    {
      seq->data = result;
      seq->data[seq->objcount] = data;
      seq->objcount++;
      return seq->objcount-1;
    }
}

char
obj_identity_eq(object o1, object o2)
{
  return o1.data == o2.data;
}

int
sequence_position (sequence seq, object data, char (* eqproc) (object, object))
{
  if(!eqproc)
    eqproc = obj_identity_eq;
  for (int i = 0; i < seq.objcount; i++)
    {
      if (eqproc(seq.data[i],data))
	{
	  return i;
	}
    }
  return -1;
}

object
sequence_find (sequence seq, object data, char (* eqproc) (object, object))
{
  if (seq.data)
    {
      int i = sequence_position (seq, data,eqproc);
      if (i != -1)
	return seq.data[i];
    }
}

void
sequence_remove (sequence *seq, object data, char (* eqproc) (object, object))
{
  int i = sequence_position (*seq, data, eqproc);
  if (i != -1)
    {
      object *newdata;
      free(seq->data[i].data);
      memcpy (seq->data + i, seq->data + i + 1,
	      sizeof (object) * (seq->objcount - i - 1));
      newdata = realloc(seq->data,sizeof(object)*(seq->objcount - 1));
      seq->data = newdata;
      seq->objcount--;
   }
}

sequence
sequence_map (sequence seq, object (*proc) (object))
{
  for(int i = 0; i < seq.objcount; i++)
    {
      seq.data[i] = proc(seq.data[i]);
    }
  return seq;
}

void
sequence_foreach (sequence seq, void (*proc) (object))
{
  for(int i=0; i< seq.objcount;i++)
    {
      proc(seq.data[i]);
    }
}

void
free_obj(object o)
{ 
  free(o.data);
}

void
sequence_free (sequence seq)
{
  sequence_foreach(seq,free_obj);
  free(seq.data);
}

object
sequence_reduce(sequence seq, object (* proc)(object, object))
{
  if(seq.objcount == 1)
    return seq.data[0];
  else
    {
      object result = proc(seq.data[0],seq.data[1]);
      for(int i = 2; i < seq.objcount; i++)
        {
          result = proc(result, seq.data[i]);
        }
      return result;
    }
}

void
sequence_remove_at (sequence* seq, int index)
{
  if(!seq->objcount  || index >= seq->objcount)
    return;
  else
    {
      object* newobjs;
      free(seq->data[index].data);
      memcpy(seq->data+index,seq->data+index+1,sizeof(object)*(seq->objcount-index-1));
      newobjs = realloc(seq->data,sizeof(object)*(seq->objcount-1));
      seq->data = newobjs;
      seq->objcount--;
    }
}

/*
Some general utility conversion funcs, which, in my not-so-humble opinion, belong here
*/

object
make_int_obj(int i)
{
  object o;
  o.typeinfo = TYPE_INT;
  o.data = malloc(sizeof(int));
  *((int*)o.data)=i;
  return o;
}

int
get_obj_int(object o)
{
  return *((int*)o.data);
}

object
make_uint32_obj (Uint32 u)
{
  object o;
  o.typeinfo = TYPE_UINT32;
  o.data = malloc(sizeof(Uint32));
  *((Uint32*)o.data)=u;
  return o;
}

Uint32
get_obj_uint32(object o)
{
  return *((Uint32*)o.data);
}
