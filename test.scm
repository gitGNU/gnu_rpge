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
			   (display (get-mob-data X))
			   (newline)))
(display (get-global-data))
(newline)
(define m (make-mob 0 0 "test_sprite.png"))
(add-to-table! (get-global-data) 'bindings (init-table))
(procedural-stats-init)
(bind-key 'd (lambda() (add-mob-movement m 1 0 16)(set-camera-x (1+ (get-camera-x)))))
(bind-key 'a (lambda() (add-mob-movement m -1 0 16)(set-camera-x (- (get-camera-x) 1))))
(bind-key 's (lambda() (add-mob-movement m 0 1 16)(set-camera-y (1+ (get-camera-y)))))
(bind-key 'w (lambda() (add-mob-movement m 0 -1 16)(set-camera-y (- (get-camera-y) 1))))
(set-tile 5 5 (create-tile "test.png" (make-rect 0 0 16 16) block-all-undirectional))
(define n (make-mob 5 0 "test_sprite.png"))