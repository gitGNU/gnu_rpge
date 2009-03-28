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

;Battle.scm: Define the basics of a nethack-style battle system.

;Default damage function.
;One can of course decide to use a different one.
;The contract for damage functions is that all calls made to them
;shall be given the same arguments as those listed here, namely
;two mobs in attacker-first order.
;They shall return a number.
(define (get-damage attacker defender)
  ;Damage == (attack-armor)*(random factor in [0.75-1])
  (* (let ((raw (- (get-mob-attack attacker) (get-mob-armor defender))))
       (if (> raw 0)
	   raw
	   0))
     (+ 0.75 (* (random:uniform) 0.25))))

;Create a battle handler, with the given damage procedure curried in.
;These simply take care of the mechanics of one side of a battle.
(define (battle-executor damage-procedure)
  (lambda (attacker defender)
    (let ((damage (damage-procedure attacker defender)))
      (decrease-mob-life! defender damage))))

;Create a twin battle handler, which handles simultaneous attacks.
;This can take any battle handler, as long as it follows the same conventions 
;as those made by battle-executor.
(define (twin-battle-executor battle-proc)
  (lambda (a b)
    (battle-proc a b)
    (battle-proc b a)))

;Create a referee procedure. 
;These handle the total execution of a battle, including the death of one of the
;participants. 
;The actual battle logic is contained in twin-handler and may be swapped out.
(define (referee-proc twin-handler)
  (lambda (a b)
    (twin-handler a b)
    ;Actually take care of dead mobs.
    ;A lot of logic is still hardcoded in this.
    ;Mob death is universal, as is handling it,
    ;which merely leaves award-experience! as something to swap out.
    ;If this must be a really self-contained lib, swap out the death procs as well.
    (map (lambda (a b)
	   (if (mob-dead? a)
	       (begin
		 (award-experience! b a)
		 (handle-death! a))))
	 (list a b)
	 (list b a))))
    
  