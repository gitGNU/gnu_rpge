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
			   (init-mob-bindings X)
			   (init-mob-velocity! X)))

;Load the test map.
(map-load "test.map")

;We still need to do some things with this mob that haven't been
;ported to the map language yet, grab it.
;At this point, the mob has already been automatically tracked, and our above bootstrap
;procedure has taken care of the initialization needed for that.
(define m (map-named-mob (get-named-map 'test) 'main))

(defmacro when (test . body)
  `(if ,test
       (begin ,@body)))

;Definition to simplify the below bindings.
(define (press-binding-generator m x y)
  (lambda () (when (null? (get-current-dialog))
		   (if (not (= x 0))
		       (set-mob-velocity-x! m x))
		   (if (not (= y 0))
		       (set-mob-velocity-y! m y)))))

(define (release-binding-generator m x y)
  (lambda () (when (null? (get-current-dialog))
		   (if (not (= x 0))
		       (set-mob-velocity-x! m 0))
		   (if (not (= y 0))
		       (set-mob-velocity-y! m 0)))))

(define (bind-movement-key key mob x y)
  (bind-keypress! key (press-binding-generator m x y))
  (bind-keyrelease! key (release-binding-generator m x y)))

;Bind keys to movement.
(bind-movement-key 'a m -1 0)
(bind-movement-key 's m 0 1)
(bind-movement-key 'd m 1 0)
(bind-movement-key 'w m 0 -1)

;Default bindings for the dialog system, allowing some simple movement in dialogs and
;menus.
(bind-keypress! 'q (lambda () (if (not (null? (get-current-dialog))) (next-message))))
(bind-keypress! 'e (lambda () (if (not (null? (get-current-dialog))) (decide))))
(make-thread safe-load "global-bind-loop.scm")
(run-repl)