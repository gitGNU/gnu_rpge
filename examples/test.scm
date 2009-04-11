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

;General initialization, open the threads module,
;initialize the global userdata and set 
;our bootstrap procedure to allow statistics and
;mob bindings to work.
;Most of this should probably be handled automatically
;by the required libraries.
(use-modules (ice-9 threads))
(set-global-data (init-table))
(add-mob-bootstrap-proc! (lambda (X) 
			   (set-mob-data X (init-table)) 
			   (stats-init X)
			   (init-mob-bindings X)))
;Load the test map.
(map-load "test.map")
;We still need to do some things with this mob that haven't been
;ported to the map language yet, grab it.
;At this point, the mob has already been automatically tracked, and our above bootstrap
;procedure has taken care of the initialization needed for that.
(define m (map-named-mob (get-named-map 'test) 'main))
;Definition to simplify the below bindings.
(define (binding-generator m x y)
  (lambda () (if (null? (get-current-dialog))
		 (add-mob-movement m x y 16))))
;Bind keys to movement.
(add-binding 'd (binding-generator m 1 0))
(add-binding 'a (binding-generator m -1 0))
(add-binding 's (binding-generator m 0 1))
(add-binding 'w (binding-generator m 0 -1))
;Default bindings for the dialog system, allowing some simple movement in dialogs and
;menus.
(add-binding 'q (lambda () (if (not (null? (get-current-dialog))) (next-message))))
(add-binding 'e (lambda () (if (not (null? (get-current-dialog))) (decide))))
(set-main-grid (named-grid 'test))
(make-thread safe-load "global-bind-loop.scm")
(run-repl)