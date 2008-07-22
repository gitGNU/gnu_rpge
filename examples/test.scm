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
(use-modules (ice-9 threads))
(set-global-data (init-table))
(set-mob-bootstrap-proc! (lambda (X) 
			   (set-mob-data X (init-table)) 
			   (stats-init X)
			   (init-mob-bindings X)))
(define grid  (init-tilegrid 50 40))
(set-all-tiles grid (create-tile "tile2.png" (make-rect 0 0 16 16) block-none))
(define m (make-mob 0 0 grid "sprite_letter.png"))
(define n (make-mob 5 0 grid "sprite_letter.png"))
;Camera locking is now handled externally, so these are back to their old simplicity.
;Tell mob_event_test.scm to get a move on and track this mob.
(add-tracked-mob! m)
(bind-mob-event m 'tile-change (lambda (event)(set-camera-x (caddr event)) (set-camera-y (cdddr event))))
(add-binding 'd (lambda() (add-mob-movement m 1 0 16)))
(add-binding 'a (lambda() (add-mob-movement m -1 0 16)))
(add-binding 's (lambda() (add-mob-movement m 0 1 16)))
(add-binding 'w (lambda() (add-mob-movement m 0 -1 16)))
;Binding for dialog system, yay
(add-binding 'q next-message)
;Menu fun, this really needs to be abstracted and put in the standard library...... which needs rewriting.
(add-binding 'e (lambda () 
	       (if (null? (get-dialog-queue))
		   '()
		   (let ((d (get-dialog (get-current-dialog-id))))
		     ;Check if we're dealing with a menu
		     (cond ((null? d) '())
			   ((eq? (car (get-string-list d)) 'menu) ((get-menu-action (list-ref (get-menu-choices (get-string-list d)) (get-index (get-string-list d)))))))))))
(set-tile grid 5 5 (create-tile "tile1.png" (make-rect 0 0 16 16) block-all-undirectional))
(set-main-grid grid)
(add-global-mob-binding 'tile-change (lambda (mob event) (display event)(newline)))
(make-thread safe-load "keys.scm")
(make-thread safe-load "mob_event_test.scm")
(run-repl)