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
	 (window ((get-window-proc type) (car sizes) (cdr sizes) x y (car sprite-data) (cadr sprite-data))))
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
  (if (null? (get-current-dialog)) (set-current-dialog! dialogid (make-dialog x y type data)) (enqueue-dialog! dialogid (list x y type data))))

(define (destroy-dialog! d)
  (close-font (get-dialog-font d))
  ((get-window-destruction-proc (get-dialog-type d)) (get-dialog-window d)))

(define (switch-to-next-dialog)
  (destroy-dialog! (get-current-dialog))
  (let ((new (dialog-queue 'get-next!)))
    (set-current-dialog! (car new) (if (null? (cdr new)) (cdr new) (apply make-dialog (cdr new))))))

(define (next-message)
  (let ((current-d (get-current-dialog)))
    (if ((get-next-proc (get-dialog-type current-d)) current-d) 
	(switch-to-next-dialog))))

(define (create-config-proc )
  ;This should be replaced by a similar-to-the-above 'bind lambdas to symbols' scheme.
  (let ((bindings (init-table)))
    (lambda (command table . args)
      (let ((t (get-from-table bindings table)))
	(if (null? t)
	    (begin (set! t (init-table)) (add-to-table! bindings table t)))
	(cond ((eq? command 'get)
	       (get-from-table t (car args)))
	      ((eq? command 'add!)
	       (add-to-table! t (car args) (cadr args)))
	      ((eq? command 'set!)
	       (set-in-table! t (car args) (cadr args))))))))


(define dialog-config
  (create-config-proc))

(define (dialog-config-get table key)
  (dialog-config 'get table key))

(define (dialog-config-add! table key value)
  (dialog-config 'add! table key value))

(define (dialog-config-set! table key value)
  (dialog-config 'set! table key value))

(define (config-getter table)
  (lambda (type)
    (dialog-config-get table type)))

(define dialog-defaults (make-table-closure))

(define get-default-dialog-font       (table-getter (dialog-defaults ) 'font))
(define get-default-dialog-dimension  (table-getter (dialog-defaults ) 'dimensions))
(define get-default-dialog-sprite     (table-getter (dialog-defaults ) 'sprite))

(define set-default-dialog-font       (table-setter (dialog-defaults ) 'font))
(define set-default-dialog-dimension  (table-setter (dialog-defaults ) 'dimensions))
(define set-default-dialog-sprite     (table-setter (dialog-defaults ) 'sprite))

;A simple macro to simplify our definitions below.
(defmacro generate-getter (name default) 
  `(define ,(string->symbol (string-append "get-" (symbol->string name)))  
           (cond-interleave (config-getter ',name) (lambda ,(gensym) ,default))))

(defmacro default-getter (sym)
  `(generate-getter ,(string->symbol (string-append (symbol->string sym) "-proc")) 
		    ,(string->symbol (string-append "get-default-dialog-" (symbol->string sym)))))

(generate-getter next-proc      (lambda whatever #f))
(generate-getter process-proc   (lambda anything '()))
(generate-getter choice-proc    (lambda whatever (list #f)))
(default-getter font)
(default-getter dimension)
(default-getter sprite)
(generate-getter window-proc             create-window)
(generate-getter window-destruction-proc remove-window)

(define (get-font type data)
  ((get-font-proc type) data))

(define (get-dimensions type data)
  ((get-dimension-proc type) data))

(define (get-sprite type data)
  ((get-sprite-proc type) data))

(define (add-dialog-type! type  . specifiers)  
  (do ((l specifiers (cdr l)))
      ((null? l) 'DONE)
    (dialog-config-add! (caar l) type (cadar l))))


(define (decide)
  (let* ((d (get-current-dialog)) (c-proc (get-choice-proc (get-dialog-type d))))
    (if (not (null? c-proc)) 
	(let ((res (c-proc d)))
	  (if (car res)
	      (begin (destroy-dialog! d)
		     (switch-to-next-dialog)))
	  (cdr res)))))

;Ok, it's ugly, but hey, it simplifies syntax and takes care of adding the bordered stuff.
(defmacro add-bordered-and-standard-dialog-type! (name . args)
  `(begin
     (add-dialog-type! ',name ,@(map (lambda (pair) (cons 'list (cons (list 'quote (car pair)) (cdr pair)))) args))
     (add-dialog-type! ',(string->symbol (string-append "bordered-" (symbol->string name))) ,@(append (list (list 'list '(quote window-proc) 'make-bordered-window)
													  (list 'list '(quote window-destruction-proc) 'remove-bordered-window))
												     (map (lambda (pair) (cons 'list (cons (list 'quote (car pair)) (cdr pair))))
 args)))))
     
