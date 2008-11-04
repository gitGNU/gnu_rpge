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

(define (make-npc x y z	sprite message)
  (let ((mob (make-mob x y z sprite)))
    (let ((binding (bind-event 'collision
			       (lambda (e)
				 (if (or (= (cadr e) mob) (= (cddr e) mob))
				     (add-new-dialog! 'NPC 0 0  'bordered-standard-dialog (list message)))
				 'DONE))))
      (add-mob-destruction-proc mob (lambda () (remove-binding 'collision binding))))))