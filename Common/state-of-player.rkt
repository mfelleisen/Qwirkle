#lang racket

;; a data representation of the state of a Q player
;; ---------------------------------------------------------------------------------------------------

(provide 
 #; {type [SoPlayer Y] = [sop Natural [Listof Tile] Y]}
 ;; where Y is typically an external player or just a symbolic name
 sop?
 sop-score
 
 (contract-out
  [sop (-> natural? (listof tile?) any/c sop?)])

 #; {[Y] [SoPlayer Y] Natural -> [SoPlayer Y]}
 sop-score++

 #; {[Y] {SoPlayer Y} [Listiof Tile] [Listiof Tile] -> {SoPlayer Y}}
 hand-to

 #; {[SoPlayer Y] -> [SoPlayer (U Symbol String)]}
 sop-special

 #; {Player [Listof Tile] -> Boolean}
 player-owns-tiles
 
 #; {[X Y] [SoPlayer X] Y [Y ->Image] {{ [Y] [SoPlayer Y] -> Image }} -> Image}
 render-sop*

 #; {[Y] [SoPlayer Y] -> Image}
 render-sop)

;; ---------------------------------------------------------------------------------------------------
(require Qwirkle/Common/tiles)
(require (prefix-in 2: 2htdp/image))
(require (for-syntax syntax/parse))

(module+ test
  (require (submod ".."))
  (require (submod Qwirkle/Common/tiles examples))
  (require rackunit))

;; ---------------------------------------------------------------------------------------------------
(struct sop [score tiles player] #:prefab)

(define-match-expander sop/m
  (λ (stx)
    (syntax-parse stx
      [(sop/m score tiles payload) #'(sop score tiles payload)])))

#; {[SoPlayer Y] -> {SoPlayer Y}}
(define (sop-score++ first n)
  (match-define [sop/m score tiles payload] first)
  (sop (+ score n) tiles payload))

#; {[Y] {SoPlayer Y} [Listiof Tile] [Listiof Tile] -> {SoPlayer Y}}
(define (hand-to p new-tile* old-tile*)
  (match-define [sop/m score tiles payload] p)
  (sop score (append new-tile* (remove* old-tile* tiles)) payload))

#; {[SoPlayer Y] -> [SoPlayer Y]}
(define (sop-special first)
  (match-define [sop/m score tiles payload] first)
  (define name
    (cond
      [(or (symbol? payload) (string? payload)) payload]
      [else (object-name payload)]))
  (sop score tiles name))

#; {Player [Listof Tile] -> Boolean}
(define (player-owns-tiles player placed-tiles)
  (define tiles-owned (sop-tiles player))
  (for/and ([placed placed-tiles])
    (begin0
      (cons? (member placed tiles-owned))
      (set! tiles-owned (remove placed tiles-owned)))))

(module+ test
  (define the-starter-player (apply sop 0 [list starter-tile* 'player1]))
  (check-true (player-owns-tiles the-starter-player starter-tile*) "it owns lshaped")
  (check-true (player-owns-tiles (sop 0 (cons +starter-tile tiles1) 'p) tiles1) "owns, 1"))

#; {[X Y] [SoPlayer X] Y [Y ->Image] -> Image}
(define (render-sop* one l-sop [render-one render-sop])
  (define sop-images (cons (render-sop one) (map render-one l-sop)))
  (for/fold ([r (first sop-images)]) ([s (rest sop-images)])
    (2:above/align 'left r vblank s)))

#; {[Y] {SoPlayer Y} -> Image}
(define (render-sop 1sop)
  (match-define [sop/m score tiles player] 1sop)
  (define tiles-image (map render-tile tiles))
  (define score-image (2:text (number->string score) 20 'black))
  (define player-image (2:text (~a player) 20 'black))
  (apply 2:beside/align 'top player-image hblank score-image hblank tiles-image))

(define hblank (2:rectangle 10 1 'solid 'white))
(define vblank (2:rectangle 1 10 'solid 'white))

;; ---------------------------------------------------------------------------------------------------
(module+ json
  (provide
   #; {type JPlayer = Natural || { SCORE : Natural, TILES : [Listof JTile]}}
   TILES
   SCORE
   players->jsexpr
   1player->jsexpr
   jsexpr->1player))

(module+ json
  (require (submod Qwirkle/Common/tiles json))
  (require Qwirkle/Lib/parse-json)
  (require json))

(module+ json

  (define SCORE 'score)
  (define TILES 'tile*)

  (define (players->jsexpr p*) (map 1player->jsexpr p*))

  #; {(U SoPlayer Natural) -> JPlayer}
  (define (1player->jsexpr 1player)
    (match 1player [(sop/m score tiles _) (hasheq SCORE score TILES (map tile->jsexpr tiles))]))

  (define (jsexpr->1player j #:name (name "a name"))
    (def/jsexpr-> 1player
      #:object {[SCORE (? natural? s)] [TILES tiles (list t ...)]}
      (sop s t name))
    (jsexpr->1player j)))
