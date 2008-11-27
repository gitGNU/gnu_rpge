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

(define (slot-allowed? i s)
  (let ((slots ((item-getter 'allowed-slots) i)))
    (hashq-ref slots s)))

(define (equip mob slot item)
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
	((equip-handler item) mob slot))))

(define (unequip mob slot)
  (let ((i (equipped-item mob slot)))
    (remove-from-table! (get-mob-equipment mob) slot)
    ;For the same reason as above, the unequip handler is
    ;called with both the mob and the slot.
    ((unequip-handler i) mob slot)))