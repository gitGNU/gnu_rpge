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
		    (let ((data (get-data dialog)))
		      ;Contract: data is a list of a text index and any number of to-be-rendered messages.
		      (remove-text (car data))
		      (if (cdr data)
			  (let* ((window (get-window dialog)) 
				 (font (get-font dialog)) 
				 (window-sizes (get-window-dimensions window))
				 (window-coordinates (get-window-coordinates window))
				 (text-x (+ (car window-coordinates) (* 0.1 (car window-sizes))))
				 (text-y (+ (cdr window-coordinates) (* 0.1 (cdr window-sizes)))))						  
			    (set-data! dialog (cons (make-text text-x text-y (cadr data) font 255 255 255) (cddr data))))
			  #t)))
		  (lambda (type data font sprite-data window)
		    ;Data here is a list of the actual strings the dialog will contain
		    (let* ((sizes (get-window-dimensions window))
			   (coords (get-window-coordinates window))
			   (text-x (+ (car coords) (* 0.1 (car sizes))))
			   (text-y (+ (cdr coords) (* 0.1 (cdr sizes)))))
		      (cons (make-text text-x text-y (car data) font 255 255 255) (cdr data))))
		  (lambda (data)
		    (open-font "FreeMono.ttf" 32))
		  (lambda (data)
		    (cons 300 200))
		  (lambda (data)
		    (list "tile1.png" 16 16)))