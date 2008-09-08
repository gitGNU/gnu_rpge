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

(define (check-mob-events)
  (for-each (lambda (pair)
          (let ((mob (car pair)) (index (cdr pair)))
            (let ((event (get-mob-event mob index)))
              (cond ((null? (car event))  '())
                    (else
                      (execute-mob-binding mob event)
		      (execute-global-mob-binding mob event))))))	    
       (cdr (tracked-mob-table)))
  (usleep 250000)
  (check-mob-events))

;unlock the load mutex so the next load can be executed.
(unlock-mutex load-mutex) 
(check-mob-events)