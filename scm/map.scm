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

;map.scm: Define the map 'language' and its warped form of eval.
;Of course, said language is merely a set of combinations that, me wanting to avoid
;clutter in the global namespace, happen to have special meanings to them.
;Other than that, they have absolutely no meaning at all.

(define maps (make-hash-table))

(define map-paths (path-list-init))

(define (add-map-path! path)
  (set! map-paths (path-list-add path map-paths)))

(define (remove-map-path! path)
  (set! map-paths (path-list-remove path map-paths)))

;;Hook the above into config files.
(register-directive! "map-dir" add-map-path!)

(define (get-named-map name)
  (hashq-ref maps name))

(define (map-incref name)
  (aif (get-named-map name)
       (set-car! it (+ (car it) 1))))

(define (map-decref name)
  (aif (get-named-map name)
       (if (= (car it) 1)
	   (remove-map! name)
	   (set-car! it (- (car it) 1)))))

(define map-mobs cadr)
(define map-warps caddr)
(define map-preloads cadddr)
(define map-destruction-procedure (lambda (m) (car (cddddr m))))

(define (add-map! name mobs warps preloadeds refcount destruction-procedure)
  (hashq-set! maps name (list refcount mobs warps preloadeds destruction-procedure)))

(define (add-empty-map! name)
  (add-map! name (make-hash-table) (make-hash-table) '() 1 (lambda anything 'DONE)))

(define (destroy-map! name)
  (let* ((named-map (get-named-map name))
	 (map-mobs (map-mobs named-map))
	 (map-warps (map-warps named-map))
	 (preloadeds (map-preloads named-map)))
    (for-each map-decref preloadeds)
    (hash-for-each (lambda (k v) (remove-mob v) map-mobs))
    (for-each remove-warp! map-warps)
    (named-grid:remove-grid! name)))

(define (remove-map! name)
  (aif (get-map name)
       (begin
	 (destroy-map! it)
	 ((map-destruction-procedure it) it)))
  (hashq-remove! name))

(define get-map-procedure '())
(define set-map-procedure! '())

(let ((map-procs (make-hash-table)))
  (set! get-map-procedure
	(lambda (op)
	  (hash-ref map-procs op)))
  (set! set-map-procedure!
	(lambda (op proc)
	  (hash-set! map-procs op proc))))

(define (add-map-mob! name sym x y sprite)
  (let ((m (make-mob x y (named-grid name) sprite)))
    (hash-set! (map-mobs (get-named-map name)) sym m)))

(define (map-named-mob map name)
  (hash-ref (map-mobs map) name))

;This is supposed to be used in the context of map-load,
;where name defines the 'context' of the map expression,
;the car of the expression determines the procedure to call
;and the cdr of the expression provides arguments to the procedure.
(define (map-eval name expr)
  (if (not (pair? expr))
      ;Non-sexprs make no real sense in this 'language', so ignore them
      'DONE
      (apply (get-map-procedure (car expr)) (cons name (cdr expr)))))

;Return string, up to the last occurrence of char
(define (string-upto-last string char)
  (string-take string (string-rindex string char)))

(define (map-load filename)
  (let ((name (string->symbol (string-upto-last filename #\.))))
    (define (eval-expr port)
      (let ((expr (read port)))
	(if (eof-object? expr)
	    'DONE
	    (begin (map-eval name expr)
		   (eval-expr port)))))
    (add-empty-map! name)
    (call-with-path-list-input-file map-paths filename eval-expr)))

;;Make loading maps a little easier.
(register-directive! "map" map-load)
			       
(set-map-procedure! 'initialize-grid register-grid)

(let ((process-tilespec (lambda (tilespec)
			  (apply create-tile
				 `(,(cadr tilespec)
				   ,(caddr tilespec)
				   ,(primitive-eval (cadddr tilespec)))))))
  (set-map-procedure! 'all-tiles (lambda (name tilespec)
				   (named-grid:set-all-tiles! name 
							      (process-tilespec tilespec))))
  (set-map-procedure! 'tile (lambda (name x y tilespec)
			      (named-grid:set-tile! name x y (process-tilespec tilespec)))))
			    
(set-map-procedure! 'begin (lambda args 
			     (for-each primitive-eval (cdr args))))

(set-map-procedure! 'mob (lambda (name name-or-x y sprite . overflow)
			   (if (symbol? name-or-x)
			       (add-map-mob! name name-or-x y sprite (car overflow))
			       (add-map-mob! name (gensym) name-or-x y sprite))))

(set-map-procedure! 'mob-bind (lambda (name id ev proc)
				(let ((p (primitive-eval proc))
				      (m (map-named-mob (get-named-map name) id)))
				  (if (not (tracked-mob? m))
				      (add-tracked-mob! m))
				  (add-mob-binding! m ev p))))

(set-map-procedure! 'show-grid (lambda (name)
				 (set-main-grid (named-grid name))))
