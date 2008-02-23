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

(define (mob-tracking-init)
  (add-to-table! (get-global-data) `tracked-mobs (init-table)))

(define (tracked-mob-table)
  (get-from-table (get-global-data) 'tracked-mobs))

(define (add-tracked-mob! mob)
  (add-to-table! (tracked-mob-table) mob (open-mob-events mob)))
 
(define (init-mob-bindings mob)
  (add-to-table! (get-mob-data mob) 'bindings (init-table)))
  
(define (get-mob-bindings mob)
  (get-from-table (get-mob-data mob) 'bindings))
  
(define (bind-mob-event mob event proc)
  (let ((stored (get-from-table (get-mob-bindings mob) event)))
    (cond ((null? stored) (add-to-table! (get-mob-bindings mob) event proc))
          (else
            (set-in-table! (get-mob-bindings mob) event proc)))))
            
(define (get-mob-binding mob event)
  (get-from-table (get-mob-bindings mob) event))
            
(define (execute-mob-binding mob event)
  (let ((proc (get-mob-binding mob (car event))))
    (display proc)
    (newline)
    (if (null? proc) '()
        (proc event))))