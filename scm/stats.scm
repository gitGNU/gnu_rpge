;Copyright 2008 Remco Bras
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

(define (stats-init mob)
  (add-to-table! (get-mob-data mob) 'stats (init-table)))

(define (mob-stats-table mob)
  (get-from-table (get-mob-data mob) 'stats))

(define (get-stat mob stat)
  (let ((stat-proc (get-stat-proc stat)))
    (if (null? stat-proc)
	(get-from-table (mob-stats-table mob) stat)
	(stat-proc mob))))

(define (set-stat mob stat value)
  (let ((stat-value (get-stat mob stat)))
    (if (null? stat-value) (add-to-table! (mob-stats-table mob) stat value)
	(set-in-table! (mob-stats-table mob) stat value))))

(define get-procstats-table (make-table-closure))

(define (add-procedural-stat-proc! stat proc)
  (add-to-table! (get-procstats-table) stat proc))

(define (get-stat-proc stat)
  (get-from-table (get-procstats-table) stat))

(define (remove-procedural-stat-proc! stat)
  (remove-from-table! (get-procstats-table) stat))

(define (stat-setter stat)
  (lambda (mob value)
    (set-stat mob stat value)))

(define (stat-getter stat)
  (lambda (mob)
    (get-stat mob stat)))

(define set-mob-attack! (stat-setter 'attack))
(define get-mob-attack (stat-getter 'attack))

(define set-mob-life! (stat-setter 'life))
(define get-mob-life (stat-getter 'life))

(define get-mob-armor (stat-getter 'armor))
(define set-mob-armor! (stat-setter 'armor))

(define get-mob-experience (stat-getter 'experience))
(define set-mob-experience! (stat-setter 'experience))

(define (decrease-mob-life! mob v)
  (set-mob-life! mob (- (get-mob-life mob) v)))

(define get-mob-experience-value (stat-getter 'experience-value))
(define set-mob-experience-value! (stat-setter 'experience-value))

(define (mob-dead? m)
  (<= (get-mob-life m) 0))

(define (award-experience! victor loser)
  (set-mob-experience! victor 
		       (+ (get-mob-experience victor)
			  (get-mob-experience-value loser))))