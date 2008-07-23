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
    (list type window font ((get-process-proc type) type data font sprite-data window))))

(define get-dialog-type car)
(define get-dialog-window cadr)
(define get-dialog-font caddr)
(define get-dialog-data cadddr)
(define (set-dialog-data! dialog val)
  (set-car! (cdddr dialog) val))
    
(define (set-current-dialog! dialogid dialog)
  (set-car! (dialog-state) dialogid)
  (set-cdr! (dialog-state) dialog))

(define (get-current-dialog) (cdr (dialog-state)))

(define dialog-queue 
  (begin (define queue '())
	 (define (add-dialog d)
	   (set! queue (append! queue (list d))))
	 (define (get-next-dialog!)
	   (if (null? queue) (cons '() '())
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

(define (destroy-dialog! d)
  (close-font (get-dialog-font d))
  (remove-window (get-dialog-window d)))

(define (next-message)
  (let ((current-d (get-current-dialog)))
    (if ((get-next-proc (get-dialog-type current-d)) current-d) 
	(begin
	  (destroy-dialog! current-d)
	  (let ((next-dialog (dialog-queue 'get-next!)))
	    (set-current-dialog! (car next-dialog) (cdr next-dialog)))))))

(define (create-config-proc tables)
  ;Create an assoc list of the bound tables and convert it to a table
  (let ((bindings (make-table-from-assoc-list (map (lambda (x) (cons x (init-table))) tables))))
    ;This should be replaced by a similar-to-the-above 'bind lambdas to symbols' scheme.
    (lambda (command table . args)
      (cond ((eq? command 'get)
	     (get-from-table (get-from-table bindings table) (car args)))
	    ((eq? command 'add!)
	     (add-to-table! (get-from-table bindings table) (car args) (cadr args)))
	    ((eq? command 'set!)
	     (set-in-table! (get-from-table bindings table) (car args) (cadr args)))))))


(define dialog-config
  (create-config-proc '(next-proc font-proc dimension-proc sprite-proc process-proc)))

(define (dialog-config-get table key)
  (dialog-config 'get table key))

(define (dialog-config-add! table key value)
  (dialog-config 'add! table key value))

(define (dialog-config-set! table key value)
  (dialog-config 'set! table key value))

(define (get-next-proc type)
  (dialog-config-get 'next-proc type))

(define (get-process-proc type)
  (dialog-config-get 'process-proc type))

(define (get-font type data)
  ((dialog-config-get 'font-proc type) data))

(define (get-dimensions type data)
  ((dialog-config-get 'dimension-proc type) data))

(define (get-sprite type data)
  ((dialog-config-get 'sprite-proc type) data))

(define (add-dialog-type! type next-proc process-proc font-proc dimension-proc sprite-proc)
  (dialog-config-add! 'next-proc type next-proc)
  (dialog-config-add! 'process-proc type process-proc)
  (dialog-config-add! 'font-proc type font-proc)
  (dialog-config-add! 'dimension-proc type dimension-proc)
  (dialog-config-add! 'sprite-proc type sprite-proc))