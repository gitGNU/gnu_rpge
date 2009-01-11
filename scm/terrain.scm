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

;terrain.scm: Define the basics of warps, named grids, preloading and so on.

(define *grid-names* (make-hash-table))

(define (register-grid name width height)
  (hashq-set! *grid-names* name (init-tilegrid width height))
  name)

(define (named-grid name)
  (hashq-ref *grid-names* name))

;Procedure generator that takes a procedure and returns a new one, 
;which takes a grid name, resolves it and applies p to
; (grid . args), where grid is the resolved grid.
(define (named-grid-proc p)
  (lambda (name . args)
    (let ((g (named-grid name)))
      (apply p (cons g args)))))
  
(define named-grid:set-tile! (named-grid-proc set-tile))
(define named-grid:set-all-tiles! (named-grid-proc set-all-tiles))
(define named-grid:remove-grid! (named-grid-proc remove-tilegrid))