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

;key-funcs-new.scm: This file defines a new set of key binding functions.
;The main idea of these is to separate bindings into an event-time pair,
;providing functions to be called when keys are pressed or released, and a
;function to be called while the key is being held.

;Function to return the table containing key bindings.
;Each key is a symbol, each value is a list of hash tables.
(define key-bindings-table (closure-gen (make-hash-table)))

;Return the list all keys are bound to once the first attempt
;to bind anything to them is made.
(define (initial-binding-list)
  (list (make-hash-table)
	(make-hash-table)
	(make-hash-table)))

;Helper procedure to initialize a binding to the initial value
;returned by the procedure above.
(define (initialize-binding! key)
  (hash-set! (key-bindings-table) key (initial-binding-list)))

;Convenience selectors for the lists used as bindings in 
;(key-bindings-table).
(define on-press-table car)
(define on-release-table cadr)
(define while-pressed-table caddr)

;Helper procedure to define a procedure that
;manipulates bindings.
;The argument sub-table-getter is called with a binding list as 
;returned (initially) from initial-binding-list, and should
;return a hash table the key should be bound in.
(define (binding-procedure sub-table-getter)
  (alambda (key procedure)
	   (let ((current-binding (hash-ref (key-bindings-table) key)))
	     (if current-binding
		 (let ((sub-table (sub-table-getter current-binding))
		       (sym (gensym)))
		   (hash-set! sub-table sym procedure)
		   sym)
		 (begin
		   (initialize-binding! key)
		   (self key procedure))))))

;User-friendlier procedures that access the three hash tables individually.
(define bind-keypress! (binding-procedure on-press-table))
(define bind-keyrelease! (binding-procedure on-release-table))
(define bind-while-pressed! (binding-procedure while-pressed-table))