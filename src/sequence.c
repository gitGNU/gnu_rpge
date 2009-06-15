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

#include "sequence.h"
S_CONVERTORS(int,INT);
S_CONVERTORS(Uint32,UINT32);
CONVERTORS(char*,string,STRING);

/*Return empty sequences and objects, for initialization and/or defaulting purposes*/
sequence
sequence_init (void)
{
  sequence s;
  s.data = NULL;
  s.objcount = 0;
  return s;
}

static object
null_obj()
{
  object o = {0,0};
  return o;
}

/*Reflects LISP append! in appending an object to a sequence in-place, rather than on a copy of the sequence. As a special quirk, it returns the index the object was placed at, for the purposes of returning such indices to GUILE.*/

int
sequence_append (sequence * seq, object data)
{
  object *result = xrealloc (seq->data, (seq->objcount + 1) * sizeof (object));
  if (!result)
    return -1;
  else
    {
      seq->data = result;
      seq->data[seq->objcount] = data;
      seq->objcount++;
      return seq->objcount - 1;
    }
}

/*
Both sequence_position and sequence_find take a predicate, which should take two objects and return char. The default predicate, which checks for holding the exact same pointer, is used in case eqproc is NULL. Note that these predicates are always called with the data in the sequence as the first argument and the data passed to _find or _position as the second argument. In case you wish to avoid superfluous memory usage by passing objects of specific types into these procedures, presume that those are the second argument to your predicate. This may make certain predicates somewhat hard to understand or quite non-generic.
*/
static char
obj_identity_eq (object o1, object o2)
{
  return o1.data == o2.data;
}

int
sequence_position (sequence seq, object data, char (*eqproc) (object, object))
{
  if (!eqproc)
    eqproc = obj_identity_eq;
  for (int i = 0; i < seq.objcount; i++)
    {
      if (eqproc (seq.data[i], data))
	{
	  return i;
	}
    }
  return -1;
}

object
sequence_find (sequence seq, object data, char (*eqproc) (object, object))
{
  if (seq.data)
    {
      int i = sequence_position (seq, data, eqproc);
      if (i != -1)
	return seq.data[i];
    }
  return null_obj();
}

void
sequence_remove (sequence * seq, object data, char (*eqproc) (object, object))
{
  int i = sequence_position (*seq, data, eqproc);
  if (i != -1)
    {
      object *newdata;
      free (seq->data[i].data);
      memcpy (seq->data + i, seq->data + i + 1,
	      sizeof (object) * (seq->objcount - i - 1));
      newdata = xrealloc (seq->data, sizeof (object) * (seq->objcount - 1));
      seq->data = newdata;
      seq->objcount--;
    }
}

/*
RPGE's versions of map and foreach act rather like the scheme equivalent, in the sense that map replaces the object in the sequence and foreach does not (and expects void from the procedure passed to it). However, this edition of map does not clean the old object up properly, wherefore it is the duty of the proc to take care of this. Since map_destructive is... intriguing in that it modifies the sequence in-place, a non-destructive variant is provided. Note that, for performance reasons, it's better to use the destructive variant if you're about to dump the old data anyway. In that case, do yourself a favor and have proc delete the old object as you will lose any reference to its data contained in seq once the destructive variant is done.
*/

sequence
sequence_map(sequence seq, object (*proc) (object))
{
  sequence new = sequence_init();
  for (int i = 0; i < seq.objcount; i++)
    {
      sequence_append(&new,proc (seq.data[i]));
    }
  return new;
}

sequence
sequence_map_destructive (sequence seq, object (*proc) (object))
{
  for (int i = 0; i < seq.objcount; i++)
    {
      seq.data[i] = proc (seq.data[i]);
    }
  return seq;
}

void
sequence_foreach (sequence seq, void (*proc) (object))
{
  for (int i = 0; i < seq.objcount; i++)
    {
      proc (seq.data[i]);
    }
}

void
free_obj (object o)
{
  free (o.data);
}

void
sequence_free (sequence seq)
{
  sequence_foreach (seq, free_obj);
  free (seq.data);
}

/*
This algorithm calls a procedure on the zeroth and first element of a sequence, then on the result of that and the second element and so on until the end of the sequence is reached. This is very useful for things like adding up sequences of vectors, finding maximum values in a list of integers and so on. sequence_reduce, unlike common LISP implementations of this algorithm, does not take an optional argument to be used as a starting object instead of the zeroth object.
*/
object
sequence_reduce (sequence seq, object (*proc) (object, object))
{
  if (seq.objcount == 1)
    return seq.data[0];
  else
    {
      object result = proc (seq.data[0], seq.data[1]);
      for (int i = 2; i < seq.objcount; i++)
	{
	  result = proc (result, seq.data[i]);
	}
      return result;
    }
}

void
sequence_remove_at (sequence * seq, int index)
{
  if (!seq->objcount || index >= seq->objcount)
    return;
  else
    {
      object *newobjs;
      free (seq->data[index].data);
      memcpy (seq->data + index, seq->data + index + 1,
	      sizeof (object) * (seq->objcount - index - 1));
      newobjs = xrealloc (seq->data, sizeof (object) * (seq->objcount - 1));
      seq->data = newobjs;
      seq->objcount--;
    }
}
