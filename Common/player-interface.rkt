#lang racket

;; a player interface that the referee can use to service players 
;; ---------------------------------------------------------------------------------------------------

(provide
 #; {type Player}
 player/c

 (all-from-out Qwirkle/Common/placement))

(require Qwirkle/Common/placement)
(require Qwirkle/Common/game-state)
(require Qwirkle/Common/tiles)

(define player/c
  (class/c
   [name      (->m string?)]
   [setup     (->m state? (listof tile?) void?)]
   [take-turn (->m state? action?)]
   [new-tiles (->m (listof tile?) void?)]
   [win       (->m boolean? void?)]))