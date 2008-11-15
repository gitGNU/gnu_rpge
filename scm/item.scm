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

(define (make-message-acceptor items bindings)
  (lambda (message . args)
    (let ((p (hashq-ref bindings message)))
      (if (null? p) 
	  'MESSAGE-NOT-UNDERSTOOD
	  (apply p items args)))))

(define (alist->hash-table l)
  (let ((res (make-hash-table)))
    (for-each (lambda (pair) (hashq-set! res (car pair) (cdr pair))) l)
    res))

;This is so overly generic it's kind of ridiculous to name this an item...
(define (make-item . bindings)
  (make-message-acceptor (alist->hash-table bindings) 
			 (alist->hash-table `((get . ,(lambda (bindings . args) (apply hashq-ref (cons bindings args))))
					      (set . ,(lambda(bindings .args) (apply hashq-set! (cons bindings args))))))))

