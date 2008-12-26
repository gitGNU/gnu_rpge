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
(define grid  (register-grid 'foo 50 40))
(named-grid:set-all-tiles! grid (create-tile "tile2.png" (make-rect 0 0 16 16) block-none))
(define m (make-mob 0 0 (named-grid grid) "sprite_letter.png"))
(define n (make-mob 5 0 (named-grid grid) "sprite_letter.png"))
;Tell mob_event_test.scm to get a move on and track this mob.
(add-tracked-mob! m)
(bind-mob-event m 'tile-change (lambda (event)(set-camera-x (caddr event)) (set-camera-y (cadddr event))))
(define (binding-generator m x y)
  (lambda () (if (null? (get-current-dialog))
		 (add-mob-movement m x y 16))))
;Camera locking is now handled externally, so these are back to their old simplicity.
(add-binding 'd (binding-generator m 1 0))
(add-binding 'a (binding-generator m -1 0))
(add-binding 's (binding-generator m 0 1))
(add-binding 'w (binding-generator m 0 -1))
;Bindings for dialog system, both from dialog.scm
(add-binding 'q (lambda () (if (not (null? (get-current-dialog))) (next-message))))
(add-binding 'e (lambda () (if (not (null? (get-current-dialog))) (decide))))
(named-grid:set-tile! grid 5 5 (create-tile "tile1.png" (make-rect 0 0 16 16) block-all-undirectional))
(set-main-grid (named-grid grid))
(make-thread safe-load "keys.scm")
(run-repl)