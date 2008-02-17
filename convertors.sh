#!/bin/sh
#Copyright 2008 Remco Bras
#Copyright Remco Bras 2007
#This file is part of RPGE.
#
#RPGE is free software; you can redistribute it and/or modify
#it under the terms of the GNU General Public License as published by
#the Free Software Foundation; either version 3 of the License, or
#(at your option) any later version.
#
#RPGE is distributed in the hope that it will be useful,
#but WITHOUT ANY WARRANTY; without even the implied warranty of
#MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#GNU General Public License for more details.
#
#You should have received a copy of the GNU General Public License
#along with this program.  If not, see <http://www.gnu.org/licenses/>
#
#
#convertors.sh: Implement the convertors(type) and convertor_headers(type) sed-based macros.
#

#Save the macro-using file, allowing the user to work with the macro in place, rather than the expansion.
cp $1{,.backup}

sed 's/convertors[:space:]*(\([[:alnum:]_]*\));/convertors(\1,\1);/g' $1 -i

sed 's/convertors[:space:]*(\([[:alnum:]_*]*\),\([[:alnum:]_]*\));/inline object\
                         make_\2_obj(\1 foo)\
                         {\
                           object o;\
                           o.data=xmalloc(sizeof(\1));\
                           o.typeinfo = TYPE_\U\2\E;\
                           *((\1*)o.data)=foo;\
                           return o;\
                         }\
                         inline \1\
                         get_obj_\2 (object o)\
                         {\
                           return *((\1*)o.data);\
                         }/g' $1 -i
                         
sed 's/convertor_headers(\(.*\));/object make_\1_obj(\1 foo);\
                                  \1 get_obj_\1 (object o);/g' $1 -i