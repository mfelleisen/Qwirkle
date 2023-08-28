#lang racket

;; a player interface that the referee can use to service players 
;; ---------------------------------------------------------------------------------------------------

(provide
 #; {type Player}
 player%/c
 player/c

 ;; the only states a player may consume 
 pk?

  #; {Any -> Any : a string that satisfies the length and char cosntraint}
 MAX-PLAYER-NAME PLAYER-NAME
 player-name? 

 (all-from-out Qwirkle/Common/placement))

;; ---------------------------------------------------------------------------------------------------
(require Qwirkle/Common/placement)
(require Qwirkle/Common/game-state)
(require Qwirkle/Common/tiles)

(module+ test
  (require (submod Qwirkle/Common/game-state examples))
  (require rackunit))

;; ---------------------------------------------------------------------------------------------------
(define pk?
  (and/c state? (flat-named-contract "PubKnowledge" (λ (s) (natural? (state-tiles s))))))

(module+ test
  (check-false (pk? +starter-state)))

;; ---------------------------------------------------------------------------------------------------
(define player%/c
  (class/c
   [name      (->m string?)]
   [setup     (->m pk? (listof tile?) void?)]
   [take-turn (->m pk? action*?)]
   [new-tiles (->m (listof tile?) void?)]
   [win       (->m boolean? void?)]))

(define player/c [instanceof/c player%/c])

;; ---------------------------------------------------------------------------------------------------
#; {Any -> (U False Any)}
(define (player-name? x)
  (and (string? x) (and (<= (string-length x) MAX-PLAYER-NAME) (regexp-match PLAYER-NAME-PX x))))

(define MAX-PLAYER-NAME 20)
(define PLAYER-NAME "^[a-zA-Z0-9]+$")
(define PLAYER-NAME-PX (pregexp PLAYER-NAME))

(module+ test
  (check-true (cons? (player-name? "aB0")))
  (check-false (player-name? ""))
  (check-false (player-name? "ouch ouch"))
  (check-false (player-name? "123456789avcdefgAVCDEEDDDEE")))