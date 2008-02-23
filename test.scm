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
 
(set-global-data (init-table))
(init-tilegrid 50 40)
(set-all-tiles (create-tile "test_tile2.png" (make-rect 0 0 16 16) block-none))
(set-mob-bootstrap-proc! (lambda (X) 
			   (set-mob-data X (init-table)) 
			   (stats-init X)
                           (init-mob-bindings X)
			   (display (get-mob-data X))
			   (newline)))
(define m (make-mob 0 0 "test_sprite.png"))
(add-to-table! (get-global-data) 'bindings (init-table))
(procedural-stats-init)
(mob-tracking-init)
;Camera locking is now handled externally, so these are back to their old simplicity.
;Tell mob_event_test.scm to get a move on and track this mob.
(add-tracked-mob! m)
(display (get-global-data))
(newline)
(bind-mob-event m 'tile-change (lambda (event)(set-camera-x (caddr event)) (set-camera-y (cdddr event))))
(bind-key 'd (lambda() (add-mob-movement m 1 0 16)))
(bind-key 'a (lambda() (add-mob-movement m -1 0 16)))
(bind-key 's (lambda() (add-mob-movement m 0 1 16)))
(bind-key 'w (lambda() (add-mob-movement m 0 -1 16)))
(set-tile 5 5 (create-tile "test.png" (make-rect 0 0 16 16) block-all-undirectional))
(define n (make-mob 5 0 "test_sprite.png"))