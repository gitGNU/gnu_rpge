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
;

;Path.scm: Define a couple utilities to deal with load paths.

;;Return a new path list.
(define (path-list-init)
  '())

;;Return a new path list that is the same as the second argument, with the first argument added.
(define path-list-add cons)

;;Return a path list that contains all elements of liz, except string.
(define (path-list-remove string liz)
  (remove (lambda (s)
	    (equal? s string))
	  liz))

;;Return if file name exists, that is, if we can open a file named name.
;;This does not imply, by necessity, that the file does not exist if this returns false,
;;as files may not be accessible for other reasons.
(define (file-exists? name)
  (catch #t
	 (lambda ()
	   (let ((port (open-input-file name)))
	     (close port)
	     #t))
	 (lambda args
	   #f)))

;;Open a file named path, given that it may be in any of the directories specified in liz or the current working directory.
(define (open-input-file-with-path-list path liz)
  (cond ((file-exists? path) (open-input-file path))
	((null? liz) '())
	((file-exists? (string-append (car liz) path)) (open-input-file (string-append (car liz) path)))
	(else (open-input-file-with-path-list path (cdr liz)))))
