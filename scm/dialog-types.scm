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
		  (lambda (data)
		    (open-font "FreeMono.ttf" 32))
		  (lambda (data)
		    (cons 300 200))
		  (lambda (data)
		    (list "tile1.png" 16 16)))

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
		  (lambda (data)
		    (open-font "FreeMono.ttf" 32))
		  (lambda (data)
		    (cons 300 200))
		  (lambda (data)
		    (list "tile1.png" 16 16)))
			
		      
		      