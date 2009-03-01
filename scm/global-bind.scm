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
;

(define bindings (closure-gen (make-hash-table)))

(define index (open-global-events #t))

(define (branch event)
  (let ((subtab (hashq-ref (bindings) (car event))))
    (if subtab (hash-for-each (lambda (k v) (v event)) subtab))))

(define (bind-event event proc)
  (let ((subtab (hashq-ref (bindings) event)))
    (if (not subtab)
	(begin (set! subtab (make-hash-table))
	       (hashq-set! (bindings) event subtab)))
    (let ((sym (gensym)))
      (hashq-set! subtab sym proc)
      sym)))

(define (remove-binding event binding)
  (let ((subtab (hashq-ref (bindings) event)))
    (if subtab
	(hashq-remove! subtab binding))))

(define (check-for-events)
  (let ((event (get-global-event index)))
    (cond  
    ((not (null? (car event))) (branch event))))
  (check-for-events))
