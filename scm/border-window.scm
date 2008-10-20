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

(use-modules (srfi srfi-1))

(define (make-bordered-window width height x y spritename cliprect)
  (let* ((border-width (caddr cliprect))
	 (border-height (cadddr cliprect))
	 (startx (- (car cliprect) border-width))
	 (starty (- (cadr cliprect) border-height))
	 (mid-width (- width (* border-width 2)))
	 (mid-height (- height (* border-height 2))))
    ;Needs optimization and generalization
    (collect-append 
     ((i 0 (1+ i)))
     (= i 3)
     (collect ((j 0 (1+ j)))
	      (= j 3)
	      (create-window (if (= i 1) mid-width border-width)
			     (if (= j 1) mid-height border-height)
			     (+ x (if (>= i 1) border-width 0)
				(if (= i 2) mid-width 0))
			     (+ y (if (>= j 1) border-height 0)
				(if (= j 2) mid-height 0))
			     spritename
			     (make-rect (+ startx (* i border-width))
					(+ starty (* j border-height))
					border-width
					border-height))))))
				     
(define (remove-bordered-window bw)
  (for-each remove-window bw))

(define (get-window-dimensions w)
  (if (pair? w)
      (cons (fold + 0 (map (lambda (win) (car (get-window-dimensions win))) (hop cdddr w)))
	    (fold + 0 (map (lambda (win) (cdr (get-window-dimensions win))) (take w 3))))
      (primitive-get-window-dimensions w)))

(define (get-window-coordinates w)
  (primitive-get-window-coordinates (if (pair? w) (car w) w)))

(define (move-window w new-spot)
  (if (pair? w)
      (let* ((origin (primitive-get-window-coordinates (car w)))
	     (diff (cons (- (car new-spot) (car origin))
			 (- (cdr new-spot) (cdr origin)))))
	(for-each (lambda (x) (let ((origin (primitive-get-window-coordinates x)))
				(primitive-move-window x (cons (+ (car origin) (car diff))
							       (+ (cdr origin) (cdr diff))))))
	     w))
      (primitive-move-window w new-spot)))

(define (resize-window w new-size)
  (if (pair? w)
      (let* ((old-size (get-window-dimensions w))
	     (diff (cons (- (car new-size) (car old-size)) (- (cdr new-size) (cdr old-size)))))
	(for-each (lambda (win) (let ((old (get-window-dimensions win)))
			     (resize-window win (cons (+ (car old) (car diff))
						      (cdr old)))))
		  (take (cdddr w) 3)) ;Take the middle column, i.e. members 4-6
	(for-each (lambda (win) (let ((old (get-window-coordinates win)))
				  (move-window win (cons (+ (car old) (car diff))
							 (cdr old)))))
		  (take (cdddr (cdddr w)) 3)) ;Final column, doesn't REALLY need the take, but just in case someone screws with our windows.
	(for-each (lambda (win) (let ((old (get-window-dimensions win)))
			     (resize-window win (cons (car old)
						      (+ (cdr old) (cdr diff))))))
		  (hop (safe-repeater cdr 3)
		       (cdr w)))
	(for-each (lambda (win) (let ((old (get-window-coordinates win)))
				  (move-window win (cons (car old)
							 (+ (cdr old) (cdr diff))))))
		  (hop (safe-repeater cdr 3)
		       (cddr w))))
      (primitive-resize-window w new-size)))
