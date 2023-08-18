#lang racket

;; a player interface that the referee can use to service players 
;; ---------------------------------------------------------------------------------------------------

(provide
 #; {type Player}
 player%/c
 player/c

 ;; the only states a player may consume 
 info-state/c

 (all-from-out Qwirkle/Common/placement))

(require Qwirkle/Common/placement)
(require Qwirkle/Common/game-state)
(require Qwirkle/Common/tiles)

(define info-state/c
  (and/c state? (flat-named-contract "PubKnowledge" (λ (s) (natural? (state-tiles s))))))

(define player%/c
  (class/c
   [name      (->m string?)]
   [setup     (->m info-state/c (listof tile?) void?)]
   [take-turn (->m info-state/c action*?)]
   [new-tiles (->m (listof tile?) void?)]
   [win       (->m boolean? void?)]))

(define player/c [instanceof/c player%/c])
