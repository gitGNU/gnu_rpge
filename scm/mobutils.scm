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

(define (get-mob-bootstrap-proc)
  (get-from-table (get-global-data) 'mob-bootstrap-proc))

(define (set-mob-bootstrap-proc! proc)
  (if (not (null? (get-mob-bootstrap-proc)))
      (set-in-table! (get-global-data) 'mob-bootstrap-proc proc)
      (add-to-table! (get-global-data) 'mob-bootstrap-proc proc)))

(define (add-mob-bootstrap-proc! p)
  (let ((current-p (get-mob-bootstrap-proc)))
    (if (null? current-p)
	(set-mob-bootstrap-proc! p)
	(set-mob-bootstrap-proc! (interleave current-p p)))))

(define (make-mob x y grid sprite)
  (let ((mobby (create-mob x y grid sprite)))
    (if (not (null? (get-mob-bootstrap-proc)))
	((get-mob-bootstrap-proc) mobby))
    mobby))