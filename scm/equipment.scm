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

(define (equip mob slot item)
  (let ((i (equipped-item mob slot)))
    (if (null? i)
	(add-to-table! (get-mob-equipment mob) slot item)
	(set-in-table! (get-mob-equipment mob) slot item))))