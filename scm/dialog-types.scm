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

(define dialog-defaults (make-table-closure))

(define get-default-dialog-font       (table-getter (dialog-defaults ) 'font))
(define get-default-dialog-dimensions (table-getter (dialog-defaults ) 'dimensions))
(define get-default-dialog-sprite     (table-getter (dialog-defaults ) 'sprite))

(define set-default-dialog-font       (table-setter (dialog-defaults ) 'font))
(define set-default-dialog-dimensions (table-setter (dialog-defaults ) 'dimensions))
(define set-default-dialog-sprite     (table-setter (dialog-defaults ) 'sprite))

(add-dialog-type! 'standard-dialog 
		  (lambda (dialog)
		    (let ((data (get-dialog-data dialog)))
		      ;Contract: data is a list of a text index and any number of to-be-rendered messages.
		      (destroy-text (car data))
		      (if (not (null? (cdr data)))
			  (let* ((window (get-dialog-window dialog)) 
				 (font (get-dialog-font dialog)) 
				 (window-sizes (get-window-dimensions window))
				 (window-coordinates (get-window-coordinates window))
				 (text-x (+ (car window-coordinates) (/ (car window-sizes) 10)))
				 (text-y (+ (cdr window-coordinates) (/ (cdr window-sizes) 10))))
			    (set-dialog-data! dialog (cons (make-text text-x text-y (cadr data) font 255 255 255) (cddr data)))
			    #f)
			  #t)))
		  (lambda (type data font sprite-data window)
		    ;Data here is a list of the actual strings the dialog will contain
		    (let* ((sizes (get-window-dimensions window))
			   (coords (get-window-coordinates window))
			   (text-x (+ (car coords) (/ (car sizes) 10)))
			   (text-y (+ (cdr coords) (/ (cdr sizes) 10))))
		      (cons (make-text text-x text-y (car data) font 255 255 255) (cdr data))))
		  get-default-dialog-font
		  get-default-dialog-dimensions
		  get-default-dialog-sprite)

(add-dialog-type! 'cycling-menu 
		  ;Contract: Data is (text-index index-in-list . list-of-usable-stuff)
		  (lambda (dialog)
		    (let* ((data (get-dialog-data dialog)) 
			   (index (cadr data))
			   (text (car data))
			   (stringlist (cddr data))
			   (stringlen (length stringlist)))
		      (cond
		       ((= stringlen 1) #f)
		       (else
			(let* ((coords (get-text-coordinates text)) (newindex (remainder (+ index 1) stringlen)))
			  (destroy-text text)
			  (set-dialog-data! dialog (cons (make-text (car coords) (cdr coords) (list-ref stringlist newindex) (get-dialog-font dialog) 255 255 255) 
							 (cons newindex stringlist)))
			  #f)))))
		  (lambda (type data font sprite-data window)
		    (let* ((sizes (get-window-dimensions window))
			   (coords (get-window-coordinates window))
			   (text-x (+ (car coords) (/ (car sizes) 10)))
			   (text-y (+ (cdr coords) (/ (cdr sizes) 10))))
		      (cons (make-text text-x text-y (car data) font 255 255 255)
			    (cons 0 data))))
		  get-default-dialog-font
		  get-default-dialog-dimensions
		  get-default-dialog-sprite)
			
(add-dialog-type! 'vertical-menu
		  ;Data is (cursor-index . ((string . text-index) ..))
		  (lambda (dialog)
		    (let* ((font (get-dialog-font dialog))
			   (data (get-dialog-data dialog))
			   (cursor-index (car data))
			   (texts-list (cdr data))
			   (texts-count (length texts-list))
			   (current-entry (list-ref texts-list cursor-index))			   
			   (next-index (remainder (+ cursor-index 1) texts-count))
			   (next-entry (list-ref texts-list next-index))
			   (current-coords (get-text-coordinates (cdr current-entry)))
			   (next-coords (get-text-coordinates (cdr next-entry))))
		      (destroy-text (cdr current-entry))
		      (destroy-text (cdr next-entry))
		      (set-cdr! current-entry (make-text (car current-coords) (cdr current-coords) (car current-entry) font 255 255 255))
		      (set-cdr! next-entry (make-text (car next-coords)    (cdr next-coords)    (car next-entry)    font 0   255 255))
		      (set-car! data next-index)
		      #f))
		  (lambda (type data font sprite-data window)
		    (let* ((sizes (get-window-dimensions window))
			   (coords (get-window-coordinates window))
			   (text-x (+ (car coords) (/ (car sizes) 10)))
			   (text-dy (/ (cdr sizes) 10))
			   (text-y (+ (cdr coords) text-dy)))
		      (let ((first #t))
			(cons 0 (map (lambda (s) (cons s 
					       (let ((y text-y))
						 (set! text-y (+ text-y text-dy))
						 (apply make-text text-x y s font (if first (begin (set! first #f) '(0 255 255)) '(255 255 255)))))) data)))))
		  get-default-dialog-font
		  get-default-dialog-dimensions
		  get-default-dialog-sprite)
