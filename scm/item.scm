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

(define (make-message-acceptor items bindings)
  (lambda (message . args)
    (aif (hashq-ref bindings message)
	 (apply it items args)
	 'MESSAGE-NOT-UNDERSTOOD)))

(define (alist->hash-table l)
  (let ((res (make-hash-table)))
    (for-each (lambda (pair) (hashq-set! res (car pair) (cdr pair))) l)
    res))

;This is so overly generic it's kind of ridiculous to name this an item...
(define (make-item . bindings)
  (make-message-acceptor (alist->hash-table bindings) 
			 (alist->hash-table `((get . ,(lambda (bindings . args) (apply hashq-ref (cons bindings args))))
					      (set . ,(lambda (bindings . args) (apply hashq-set! (cons bindings args))))))))

;Generic utility procedure
(define (item-getter key)
  (lambda (item)
    (item 'get key)))

(define (item-setter key)
  (lambda (item value)
    (item 'set key value)))

;Gets the handler invoked upon item activation.
(define activation-handler (item-getter 'activation-handler))

;Called with a mob since items are always the property of one mob or another.
;Of course, items may call any scheme code they wish, so they may create a menu or other
;such fun thing to select something with. For example, they could temporarily pause the
;game and start moving overlays around, or at least, once such things are implemented.
;Activation handlers are called with the item itself as a second argument,
;so potions and such things may be generalized rather than needing private handlers.
;Of course, creating a bajillion closures can work just fine (heck, items themselves are closures),
;but doing so might just not be the most comfortable way to code.
(define (activate mob item)
  ((activation-handler item) mob item))
  
(define get-price (item-getter 'price))
(define set-price! (item-setter 'price))