;Copyright 2008 Remco Bras
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

(define tracked-mob-table (make-table-closure))

(define (listener-proc mob)
  (let ((ind (open-mob-events mob #t)))
    (lambda ()
      (do ((ev (get-mob-event mob ind) (get-mob-event mob ind)))
	  ((not (tracked-mob? m)) (remove-from-table! (tracked-mob-table) mob))
	(execute-mob-binding mob ev)
	(execute-global-mob-binding mob ev)))))

(define (tracked-mob? m)
  (equal? (get-from-table (tracked-mob-table) m) #f))

(define (add-thread-listener! mob)
  (make-thread (listener-proc mob)))

(define (add-tracked-mob! mob)
  (add-to-table! (tracked-mob-table) mob #f)
  (add-thread-listener! mob))

(define (remove-tracked-mob! mob)
  (set-in-table! (tracked-mob-table) mob #t))

(define (init-mob-bindings mob)
  (add-to-table! (get-mob-data mob) 'bindings (init-table)))
  
(define (get-mob-bindings mob)
  (get-from-table (get-mob-data mob) 'bindings))
  
(define (get-mob-binding-table mob event)
  (get-from-table (get-mob-bindings mob) event))

(define (get-mob-binding mob event)
  (let ((tab (get-mob-binding-table mob event)))
    (if (null? tab)
	'()
	(reduce interleave
		(lambda anything 'DONE)
		(hash-map->list
		 (lambda (k v) v)
		 tab)))))

(define (set-mob-binding! mob event proc)
  (let ((tab (get-mob-binding-table mob event)))
    (if (null? tab)
	(let ((newtab (make-hash-table))
	      (sym (gensym)))
	  (add-to-table! (get-mob-bindings mob) event newtab)
	  (hash-set! newtab sym proc)
	  sym)
	(let ((sym (gensym)))
	  (hash-clear! tab)
	  (hash-set! tab sym proc)
	  sym))))

(define (add-mob-binding! mob event proc)
  (let ((sym (gensym))
	(tab (get-mob-binding-table mob event)))
    (if (null? tab)
	(set-mob-binding! mob event proc)
	(begin 
	  (hash-set! tab sym proc)
	  sym))))
            
(define (execute-mob-binding mob event)
  (let ((proc (get-mob-binding mob (car event))))
    (if (null? proc) '()
        (exec proc event))))

(define global-mob-bindings (make-table-closure))

(define (get-global-mob-binding-table event)
  (get-from-table (global-mob-bindings) event))

(define (get-global-mob-binding event)
  (let ((tab (get-global-mob-binding-table event)))
    (if (null? tab)
	'()
	(reduce interleave 
		(lambda anything 'DONE) 
		(hash-map->list
		 (lambda (k v) v)
		 tab)))))

(define (set-global-mob-binding! event proc)
  (let ((tab (get-global-mob-binding-table event)))
    (if (null? tab)
	(let ((newtab (make-hash-table))
	      (sym (gensym)))
	  (add-to-table! (global-mob-bindings) event newtab)
	  (hash-set! newtab sym proc)
	  sym)
	(let ((sym (gensym)))
	  (hash-clear! tab)
	  (hash-set! tab sym proc)
	  sym))))
	
(define (add-global-mob-binding! event proc)
  (let ((sym (gensym))
	(tab (get-global-mob-binding-table event)))
    (if (null? tab)
	(set-global-mob-binding! event proc)
	(begin 
	  (hash-set! tab sym proc)
	  sym))))

(define (remove-global-mob-binding! event sym)
  (let ((tab (get-global-mob-binding-table event)))
    (if (not (null? tab))
	(hash-remove! tab sym))))

(define (execute-global-mob-binding mob event)
  (let ((b (get-global-mob-binding (car event))))
    (if (not (null? b)) (exec b mob event))))