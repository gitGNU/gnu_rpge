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

(define (initialize-mob-equipment m)
  (add-to-table! (get-mob-data m)  'equipment (init-table)))

(define (get-mob-equipment mob)
  (get-from-table (get-mob-data m) 'equipment))

(define (equipped-item mob slot)
  (get-from-table (get-mob-equipment mob) slot))

(define equip-handler (item-getter 'equip-handler))
(define unequip-handler (item-getter 'unequip-handler))
(define set-equip-handler! (item-setter 'equip-handler))
(define set-unequip-handler! (item-setter 'unequip-handler))

(define allowed-slots (item-getter 'allowed-slots))

(define set-allowed-slots! (item-setter 'allowed-slots))

(define (slot-twiddler p)
  (alambda (i s . args)
    (aif (allowed-slots i)
	 (apply p `(,it ,s ,@args))
	 (begin (set-allowed-slots! i (make-hash-table))
		(apply self `(,i ,s ,@args))))))

(define (slot-conditional-proc slot proc)
  (lambda (mob slotty)
    (if (eq? slotty slot)
	(proc mob slotty))))

(define slot-allowed? (slot-twiddler hashq-ref))

(define add-allowed-slot (slot-twiddler (lambda (slots slot . args)
					  (hashq-set! slots slot #t))))

(define remove-allowed-slot (slot-twiddler hashq-remove!))

(define (equip-garded-proc proc)
  (alambda (mob . args)
    (if (null? (get-mob-equipment mob))
	 (begin
	   (initialize-mob-equipment mob)
	   (apply self (cons mob args)))
	 (apply proc (cons mob args)))))

(define equip 
  (equip-garded-proc 
   (lambda (mob slot item) 
     (if (slot-allowed? item slot)
	 (let ((i (equipped-item mob slot)))
	   (if (not (null? i))
	       (unequip mob slot))
	   (add-to-table! (get-mob-equipment mob) slot item)
	   ;Call the equip handler with the mob AND the slot.
	   ;The slot argument is added for polymorphic items,
	   ;say swords that have a shield form as well.
	   ;Obviously, it makes no sense for the thing to 
	   ;add attack if attached to a non-weapon slot.
	   ((equip-handler item) mob slot))))))

(define unequip 
  (equip-garded-proc 
   (lambda (mob slot)
     (let ((i (equipped-item mob slot)))
       (remove-from-table! (get-mob-equipment mob) slot)
       ;For the same reason as above, the unequip handler is
       ;called with both the mob and the slot.
       ((unequip-handler i) mob slot)))))

(define (make-bare-equipment price)
  (let ((i (make-item `(slots . ,(make-hash-table)))))
    (set-price! i price)
    i))
