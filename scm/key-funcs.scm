;Copyright  2008 Remco Bras
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
 
(define get-bindings (make-table-closure))

(define (bind-key key proc)
  (add-to-table! (get-bindings) key proc))

(define (add-binding key proc)
  (let ((current-b (get-binding key)))
    (if (null? current-b)
	(bind-key key proc)
	(begin (remove-binding key)
	       (bind-key key (interleave current-b proc))))))

(define (get-binding key)
  (get-from-table (get-bindings) key))

(define (remove-binding key)
  (remove-from-table! (get-bindings) key))

