;Copyright  2009 Remco Bras
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

;time.scm: Define a couple of time-related utilities based
;on the simplest of guile's timing routines.
;These are meant for simple tasks like timing jobs in threads and
;hence don't care one bit about SRFI-19's heavy-duty timezone-related stuff.

;time+, add two times (as given by (gettimeofday)), and 
;return the result.
(define (time+ t1 t2)
  ;Essentially, initialize the original result, and handily start looping immediately to 
  ;get rid of spare seconds in the cdr field.
  (do ((res (cons (+ (car t1) (car t2))
		  (+ (cdr t1) (cdr t2)))
	    (cons (+ (car res) 1) 
		  (- (cdr res) 1000000))))
      ((< (cdr res) 1000000) res)))

;time-, subtract two times.
;It is assumed that both time values are reasonable, that is, (- t1 t2) will not
;contain a cdr that is bigger than 1 million.
(define (time- t1 t2)
  ;Do the same as in time+, just taking new stuff from the car.
  ;This does not check for strange results with negative cars.
  (do ((res (cons (- (car t1) (car t2))
		  (- (cdr t1) (cdr t2)))
	    (cons (- (car res) 1)
		  (+ (cdr res) 1000000))))
      ((> (cdr res) 0) res)))

(define (time-negative-or-zero? t)
  ;Well-formed time differences are only negative if
  ;either the car is negative, that is, they indicate a negative
  ;number of seconds, since time- will properly carry until
  ;the number of us is positive.
  (or (< (car t) 0)
      (and (= (car t) 0) (= (cdr t) 0))))

(define (sleep-time t)
  (usleep (+ (* (car t) 1000000) (cdr t))))

;Create a thread to run proc at most once every time,
;that is, make a good-faith attempt to execute proc starting
;every time, but if proc happens to take longer, just go on immediately.
;Also, the waiting procedures used may suck absolutely.
(define (thread-job proc time)
  (make-thread (alambda ()
			(let ((next (time+ (gettimeofday) time)))
			  (proc)
			  (if (time-negative-or-zero? (time- next (gettimeofday)))
			      (self)
			      (begin
				(sleep-time (time- next (gettimeofday)))
				(self)))))))