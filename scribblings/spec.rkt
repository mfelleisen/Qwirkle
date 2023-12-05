(module spec racket/base
   [provide (all-defined-out)]

   [define PER-TURN-s 6]

   [define FINISH-BONUS-7 6]
   [define Q-BONUS-7 6]

   [define FINISH-BONUS-8 4]
   [define Q-BONUS-8 8]

   [define FINISH-BONUS [make-parameter FINISH-BONUS-7]]
   [define Q-BONUS      [make-parameter Q-BONUS-7]]
   [define MIN-PLAYERS 2]
   [define MAX-PLAYERS 4]

   [define WAIT-FOR-SIGNUP 3]  ;; chance to send a name 
   [define SERVER-WAIT 20]     ;; seconds per round 

   (define MAX-PLAYER-NAME 20)
   (define PLAYER-NAME "^[a-zA-Z0-9]+$")
  
   (define [OBS] "--show") 

   (define Tmp/ "Tmp/")
   (define PNG ".png")

   (require (only-in racket/math natural?))
   (define TILE# 1000000)
   (define (tile#? x) (and (natural? x) (<= x TILE#)))	
  
   (define (jsexpr->tiles# x)
     (if (tile#? x)
         x
	 (begin (eprintf "jsexpr->tile# : value does not satisfy tiles#?: ~a\n" x) #f)))

   )
