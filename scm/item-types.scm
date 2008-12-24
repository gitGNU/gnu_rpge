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

;item-types.scm: Defines the default item types.

;Just a stab at making decent weapons.
;Currently, these have a price, power and type.
;Weapon types are unused right now, price is for shop purposes and power is used
;in this thing's own equip handlers. 
(define (make-weapon name price power type)
  (let ((i (make-bare-equipment name price)))
    (add-allowed-slot i 'weapon)
    ((item-setter 'power) i power)
    ((item-setter 'weapon-type) i type)
    (set-equip-handler! i (slot-conditional-proc 'weapon 
						 (lambda (m s) 
						   (set-mob-attack! m (+ (get-mob-attack m) power)))))
    (set-unequip-handler! i (slot-conditional-proc 'weapon 
						   (lambda (m s) 
						     (set-mob-attack! m (- (get-mob-attack m) power)))))
    i))

(define (make-sword name price power)
  (make-weapon name price power 'sword))

;Define a new potion, which is an item whose
;activation handler adds the amounts in stat-effects
;(an alist of stat/diff pairs) to the mob stats.
;These items have absolutely zero allowed slots.
(define (make-potion name . stat-effects)
  (make-item `(activation-handler . ,(lambda (mob item) 
				      (for-each (lambda (p)
						  (set-stat mob
							    (car p)
							    (+ (cdr p)
							       (let ((s (get-stat mob (car p))))
								 (if (null? s)
								     0
								     s)))))
						stat-effects)))))