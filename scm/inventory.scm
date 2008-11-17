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

(define (initialize-mob-inventory m)
  (add-to-table! (get-mob-data  m) 'inventory '()))

(define (give-item m i)
  (set-in-table! (get-mob-data m) 'inventory (cons i (get-from-table (get-mob-data m) 'inventory))))

(define (remove-item m i)
  (set-in-table! (get-mob-data m) 'inventory (remove! (let ((done #f))
							(lambda (x)
							  (if (not done)
							      (if (equal? x i)
								  (begin
								    (set! done #t)
								    #t))
							      #f)))
						      (get-from-table (get-mob-data m) 'inventory))))