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

(define dialog-config (make-table-closure))
(define dialogs (make-table-closure))



(define (dialogs-init initial-width initial-height initial-font sprite-name sprite-width sprite-height)
  (add-to-table! (dialog-config) 'dimensions (cons initial-width initial-height))
  (add-to-table! (dialog-config) 'font initial-font)
  (add-to-table! (dialog-config) 'sprite sprite-name)
  (add-to-table! (dialog-config) 'sprite-width sprite-width)
  (add-to-table! (dialog-config) 'sprite-height sprite-height))

(define (get-dialog id)
  (get-from-table (dialogs) id))

(define (make-dialog dialogid x y stringlist)
  (let ((sizes (get-from-table (dialog-config) 'dimensions)) (font (get-from-table (dialog-config) 'font)))
    (let ((w (create-window (car sizes) (cdr sizes) x y (get-from-table (dialog-config) 'sprite) (get-from-table (dialog-config) 'sprite-width) (get-from-table (dialog-config) 'sprite-width))))
      (let ((dialog (list w -1 font stringlist)))
	(add-to-table! (dialogs) dialogid dialog)
	(dialogs-next dialogid)
	dialog))))

(define (dialogs-next dialogid)
  (let ((dialog (get-dialog dialogid)))
    (cond
     ((null? dialog) '())
     (else 
      (let ((text (get-next-text-string dialog)))
	(cond ((null? text) (destroy-dialog! dialogid) 
	       (show-next-dialog!))
	      (else
	       (if (> (get-dialog-text dialog) -1)
		   (destroy-text (get-dialog-text dialog)))
	       (set-dialog-text! dialog (make-text (get-dialog-text-x dialog) (get-dialog-text-y dialog) text (get-dialog-font dialog) 255 255 255)))))))))

(define get-dialog-text cadr)

(define get-dialog-window car)

(define get-dialog-font caddr)

(define (get-next-text-string dialog)
  (let ((list (cadddr dialog)))
    (if (null? list) '() 
	(let ((t (car list)))
	  (cond ((eq? t 'menu) 
		 (let ((ind (cadr list)))
		   (set-car! (cdr list) (+ ind 1))
		   (if (= (cadr list) (length (cddr list)))
		       (set-car! (cdr list) 0))
		   (car (list-ref (cddr list) (cadr list)))))
		(else
		 (set-car! (cdddr dialog) (cdr list))
		 t))))))

(define (get-dialog-text-x dialog)
  (let ((w (get-dialog-window dialog)))
    (let ((sizes (get-window-dimensions w))(position (get-window-coordinates w)))
      (+ (/ (car sizes) 10) (car position)))))

(define (get-dialog-text-y dialog)
  (let ((w (get-dialog-window dialog)))
    (let ((sizes (get-window-dimensions w)) (position (get-window-coordinates w)))
      (+ (/ (cdr sizes) 10) (cdr position)))))

(define (set-dialog-text! dialog val)
  (set-car! (cdr dialog) val))

(define (destroy-dialog! id)
  (let ((dialog (get-dialog id)))
    (remove-window (get-dialog-window dialog))
    (destroy-text (get-dialog-text dialog))
    (remove-from-table! (dialogs) id)))

(define (set-current-dialog! id)
  (dialog-queue 'set (list id)))

(define (queue-dialog id x y stringlist)
  (if (null? (dialog-queue 'get)) (begin (make-dialog id x y stringlist) (set-current-dialog! id))
      (append! (dialog-queue 'get) (list (list id x y stringlist)))))

;The below definition of dialog-queue is equivalent to the one used, but should be left commented out until mkobject is accepted into the library proper.
;(define dialog-queue (mkobject ((queue '())) (get (lambda whatever queue) 
	;				      set (lambda (self val) (set! queue val)) 
		;			      update (lambda (self) (cond ((null? (self 'get)) '())
			;						  ((null? (cdr (self 'get))) (self 'set '()))
				;					  (else
					;				   (let ((next (cadr (self 'get))))
						;			     (self 'set (cons (car next) (cddr (self 'get))))
							;		     next)))))))

(define dialog-queue
  (begin
    (define queue '())
    (define (set-queue! val)
      (set! queue val))
    (define (update-queue!)
      (cond ((null? queue) '())
	    ((null? (cdr queue)) (set-queue! '()))
	    (else
	     (let ((next (cadr queue)))
	       (set-queue! (cons (car next) (cddr queue)))
	       next))))
    (lambda (message . args)
      (cond ((eq? message 'get) queue)
	    ((eq? message 'set) (set-queue! (car args)))
	    ((eq? message 'update) (update-queue!))))))

(define (get-dialog-queue)
  (dialog-queue 'get))

(define (get-current-dialog-id)
  (car (get-dialog-queue)))

(define get-string-list cadddr)
(define get-menu-choices cddr)
(define get-menu-action cdr)
(define get-index cadr)

(define (make-menu id x y choices)
  (make-dialog id x y (append (list 'menu -1) choices)))

(define (queue-menu id x y choices)
  (queue-dialog id x y (append (list 'menu -1) choices)))

(define (destroy-current-dialog)
  (destroy-dialog! (get-current-dialog-id))
  (show-next-dialog!))

(define (show-next-dialog!)
  (let ((next (dialog-queue 'update)))
    (if (null? next) '() (apply make-dialog next))))