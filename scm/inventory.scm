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

(define (get-mob-inventory m)
  (get-from-table (get-mob-data m) 'inventory))

(define (set-mob-inventory! m v)
  (if (null? (get-mob-inventory m))
      (initialize-mob-inventory m))
  (set-in-table! (get-mob-data m) 'inventory v))

(define (give-item m i)
  (set-mob-inventory! m (cons i (get-mob-inventory m))))

(define (remove-item m i)
  (set-mob-inventory! m (remove! (let ((done #f))
				   (lambda (x)
				     (if (not done)
					 (if (equal? x i)
					     (begin
					       (set! done #t)
					       #t))
					 #f)))
				 (get-mob-inventory m))))

(define (prim-get-money mob)
  (get-from-table (get-mob-data mob) 'gold))

(define (get-money mob)
  (let ((res (prim-get-money mob)))
    (if (null? res)
	0
	res)))

(define (set-money! mob val)
  (if (null? (prim-get-money mob))
      (add-to-table! (get-mob-data mob) 'gold val)
      (set-in-table! (get-mob-data mob) 'gold val)))

(define (sufficient-money? mob price)
  (>= (get-money mob) price))

(define (money-twiddler proc)
  (lambda (mob val)
    (set-money! mob (proc (get-money mob) val))))

(define add-money (money-twiddler +))
(define deduct-money (money-twiddler -))
