;Copyright  2009 Remco Bras
;
;This file is part of the RPGE.
;
;RPGE is free software; you can redistribute it and/or modify
;it under the terms of the GNU General Public License as published by
;the Free Software Foundation; either version 3 of the License, or
;(at your option) any later version.
;
;RPGE is distributed in the hope that it will be useful,
;but WITHOUT ANY WARRANTY; without even the implied warranty of
;MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;GNU General Public License for more details.
;
;You should have received a copy of the GNU General Public License
;    along with this program.  If not, see <http://www.gnu.org/licenses/>
;

;default-bindings.scm: Define the default global event bindings.
;This file is primarily used by other libraries and should not be
;modified for the purpose of creating an application using rpge.

(bind-event 'key-down (lambda (e) 
			(let ((b (get-binding (cdr e))))
			  (if (not (null? b)) (exec b)))))