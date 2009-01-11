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

;warp.scm: My attempt to define warping spots, which move a mob from one grid, named
;by a certain symbol, to another, if they move to a certain spot on the grid.

;Resolve a position whose grid component is a grid name.
(define (denamify-position p)
  (list (car p) (cadr p) (if (symbol? (caddr p)) (named-grid (caddr p)) (caddr p))))

;Define a warp from pos1 to pos2.
;Both the source position and the destination position may have
;a symbol for a grid component, in which case the grid is resolved.
(define (make-warp pos1 pos2)
  (add-global-mob-binding! 'tile-change
			  (lambda (m ev)
			    (let* ((data (cdr ev))
				   (newpos (cdr data)))
			      (cond ((equal? (denamify-position pos1) newpos)
				     (stop-mob-movement m)
				     (set-mob-position m (denamify-position pos2))))))))