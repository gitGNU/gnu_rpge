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

;Procedure generator that generates procedures to get bindings for
;keys.
;Interleaves the available bindings.
(define (binding-getter-procedure sub-table-getter)
  (lambda (key)
    (let ((current-binding (hash-ref (key-bindings-table) key)))
      (if current-binding
	  (let ((sub-table (sub-table-getter current-binding)))
	    (hash-fold (lambda (key value accum)
			 (interleave accum value))
		       (lambda () 'DONE)
		       sub-table))
	  (lambda () 'DONE)))))

;User-friendlier procedures that get each of the normally defined bindings
;from the 3 tables used for every key.
(define key-press-binding (binding-getter-procedure on-press-table))
(define key-release-binding (binding-getter-procedure on-release-table))
(define key-held-binding (binding-getter-procedure while-pressed-table))


;Another hash table containing the status of keys.
(define key-status-map (closure-gen (make-hash-table)))

;Flagging procedure to mark a key as held.
(define (mark-key-held! key)
  (hash-set! (key-status-map) key #t))

;Flagging procedure to undo the mark made above.
(define (mark-key-released! key)
  (hash-remove! (key-status-map) key))

;Selector procedure.
(define (is-key-held key)
  (hash-ref (key-status-map) key))

;Main procedure to execute all bindings to be executed while keys are held.
;Bindings are run sequentially, so this procedure is fairly slow with many
;consistently-running bindings.
(define (execute-key-held-procedures)
  (hash-for-each (lambda (k v)
		   (if v
		       ((key-held-binding k))))
		 (key-status-map)))

(define (make-key-held-thread)
  (thread-job execute-key-held-procedures (cons 0 250000)))