(define (path-list-init)
  '())

(define path-list-add cons)

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

(define (open-input-file-with-path-list path liz)
  (cond ((null? liz) '())
	((file-exists? (string-append (car liz) path)) (open-input-file (string-append (car liz) path)))
	(else (open-input-file-with-path-list path (cdr liz)))))
