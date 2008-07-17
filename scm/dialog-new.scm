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

(define dialog-types (make-table-closure))

(define (make-dialog-new x y type data)
  (let* ((sizes (get-dimensions type data)) 
	 (font (get-font type data)) 
	 (sprite-data (get-sprite type data)) 
	 (window (create-window (car sizes) (cdr sizes) x y (car sprite-data) (cadr sprite-data) (caddr sprite-data))))
    (list window font ((get-process-proc type) data))))
    
(define (set-current-dialog-new! dialogid dialog)
  (add-to-table! (dialogs) dialogid dialog)
  (next-message dialogid)
  dialog)

(define (add-new-dialog-new! dialogid x y type data)
  (let ((d (make-dialog-new x y type data)))
    (if (null? (get-current-dialog-new)) (set-current-dialog-new! dialogid d) (enqueue-dialog-new! dialogid d))))

