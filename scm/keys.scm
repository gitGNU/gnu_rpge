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

(define index (open-global-events))

(define (branch event)
  (cond ((eq? (car event) 'key-down)
	 (let ((binding (get-binding (cdr event))))
           (if (not (null? binding))
               (binding))))))

(define (check-for-events)
  (display "Checking")
  (newline)
  (let ((event (get-global-event index)))
    (cond  
    ((not (null? (car event))) (branch event))))
  (check-for-events))

;signal that the next load may be executed
(unlock-mutex load-mutex)
(check-for-events)