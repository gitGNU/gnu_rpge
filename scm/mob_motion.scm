;Copyright 2009 Remco Bras
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

;;Initialize the velocity of the mob to (0,0).
(define (init-mob-velocity! m)
  (add-to-table! (get-mob-data m) 'velocity (cons 0 0)))

;;Getter for velocity, trivial.
(define (get-mob-velocity m)
  (get-from-table (get-mob-data m) 'velocity))

(define (wrap-velocity-setter proc)
  (lambda (m v)
    (let ((not-moving (and (= (car (get-mob-velocity m)) 0) (= (cdr (get-mob-velocity m)) 0))))
      (proc m v)
      (if not-moving
	  (queue-mob-motion! m)))))

;;Trivial setter.
(define set-mob-velocity! (wrap-velocity-setter (lambda (m v)
						  (set-in-table! (get-mob-data m) 'velocity v))))

;;Utility setters.
(define set-mob-velocity-x! (wrap-velocity-setter (lambda (m v)
						    (set-car! (get-mob-velocity m) v))))

(define set-mob-velocity-y! (wrap-velocity-setter (lambda (m v)
						    (set-cdr! (get-mob-velocity m) v))))

;;Queue the default motion for a mob, given its velocity.
;;Velocity is given as number-of-tiles-per-16-frames.
;;Returns #t if a motion was queued.
(define (queue-mob-motion! m)
  (let ((v (get-mob-velocity m)))
    (cond ((and (= (car v) 0) (= (cdr v) 0)) #f)
	  (else (add-mob-movement m (car v) (cdr v) 16) #t))))

;;The weirdly-named 'motion continuator', i.e. the procedure that,
;;when a mob is stopped, simply queues the next motion.
;;More properly, it simply curries queue-mob-motion!.
(define (mob-motion-continuator m)
  (lambda (event)
    (queue-mob-motion! m)))


