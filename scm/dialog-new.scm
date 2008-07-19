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

;Essentially a closure over (dialog-id . dialog)
(define dialog-state (closure-gen (cons '() '())))
(define dialog-types (make-table-closure))

(define (make-dialog x y type data)
  (let* ((sizes (get-dimensions type data)) 
	 (font (get-font type data)) 
	 (sprite-data (get-sprite type data)) 
	 (window (create-window (car sizes) (cdr sizes) x y (car sprite-data) (cadr sprite-data) (caddr sprite-data))))
    (list type window font ((get-process-proc type) data))))

(define get-type car)
(define get-window cadr)
(define get-font caddr)
(define get-data cadddr)
    
(define (set-current-dialog! dialogid dialog)
  (set-car! (dialog-state) dialogid)
  (set-cdr! (dialog-state) dialog))

(define (get-current-dialog) (cdr (dialog-state)))

(define dialog-queue 
  (begin (define queue '())
	 (define (add-dialog d)
	   (set! queue (append! queue (list d))))
	 (define (get-next-dialog!)
	   (if (null? queue) '()
	       (let ((d (car queue)))
		 (set! queue (cdr queue))
		 d)))
	 (lambda (message . args)
	   (cond ((eq? message 'get) queue)
		 ((eq? message 'add) (apply add-dialog args))
		 ((eq? message 'get-next!) (get-next-dialog!))))))

(define (enqueue-dialog! dialogid d)
  (dialog-queue 'add (cons dialogid d)))

(define (add-new-dialog! dialogid x y type data)
  (let ((d (make-dialog x y type data)))
    (if (null? (get-current-dialog)) (set-current-dialog! dialogid d) (enqueue-dialog! dialogid d))))

(define (next-message)
  (let ((current-d (get-current-dialog)))
    (if ((get-next-proc (get-type current-d)) current-d) 
	(let ((next-dialog (dialog-queue 'get-next!)))
	  (set-current-dialog! (car next-dialog) (cdr next-dialog))))))

(define dialog-config-get
  ;This bit here really needs a (init-tables next-procs ...)
  (let ((next-procs (init-table)) 
	(process-procs (init-table))
	(font-procs (init-table))
	(dimension-procs (init-table))
	(sprite-procs (init-table)))
    (lambda (message key)
      ;This also needs work
      (cond ((eq? message 'next-proc)      (get-from-table next-procs key))
	    ((eq? message 'process-proc)   (get-from-table process-procs key))
	    ((eq? message 'font-proc)      (get-from-table font-procs key))
	    ((eq? message 'dimension-proc) (get-from-table dimension-procs key))
	    ((eq? message 'sprite-proc)    (get-from-table dimension-procs key))))))

(define (get-next-proc type)
  (dialog-config-get 'next-proc type))

(define (get-font type data)
  ((dialog-config-get 'font-proc type) data))

(define (get-dimensions type data)
  ((dialog-config-get 'dimension-proc type) data))

(define (get-sprite type data)
  ((dialog-config-get 'sprite-proc type) data))