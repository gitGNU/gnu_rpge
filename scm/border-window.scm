;Copyright  2008 Remco Bras
;
;This file is part of RPGE.
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

(define (make-bordered-window width height x y spritename cliprect)
  (let* ((startx (car cliprect))
	 (starty (cadr cliprect))
	 (border-width (caddr cliprect))
	 (border-height (cadddr cliprect))
	 (mid-width (- width (* border-width 2)))
	 (mid-height (- height (* border-height 2))))
    ;Needs optimization and generalization
    (do ((i 0 (1+ i))
	 (acc '()))
	((= i 3) (reverse! acc))
      (do ((j 0 (1+ j)))
	  ((= j 3) '())
	  (set! acc
		(cons (create-window (if (= i 1) mid-width border-width)
				     (if (= j 1) mid-height border-height)
				     (+ x (if (>= i 1) border-width 0)
					(if (= i 2) mid-width 0))
				     (+ y (if (>= j 1) border-height 0)
					(if (= j 2) mid-height 0))
				     spritename
				     (make-rect (+ startx (* i border-width))
						(+ starty (* j border-height))
						border-width
						border-height))
		      acc))))))
				     
(define (remove-bordered-window bw)
  (for-each remove-window bw))