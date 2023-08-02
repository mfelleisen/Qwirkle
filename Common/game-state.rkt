#lang racket

;; a data representation of the referee's knowledge and the player's knowledge about the game

(provide
 #; {type [GameState X Y] = [state Map [Listof X] Y]}
 ;;      where [Listof X] specifies the order of turns that players take from here on out 
 #; {type [RefState Y]    = [GameState [SoPlayer Y]] [Listof Tile]}
 ;;      what the referee knows about the game 
 #; {type PubKnowledge    = [GameState (cons [SoPlayer (U String Symbol)] Natural) Natural]}
 ;;      what a player may know about the game: its own state (named) and the score of others
 state?
 state-map
 state-players
 state-tiles

 #; {type [SoPlayer Y]    = [sop Natural [Listof Tile] Y]}
 ;;      where Y is typically an external player or just a symbolic name 
 sop-tiles
 sop-score 

 (contract-out
  [create-ref-state
   (->* (map? [listof [list/c [listof tile?] any/c]]) (#:tiles0 (listof tile?)) state?)]
  
  [ref-state-to-info-state
   (-> state? state?)]
  
  [legal
   #; {(U PubKnowledge [RefState Y]) [Listof Placement] -> (U Map? False)}
   (-> state? (listof placement?) (or/c #false map?))]
  
  [score
   ;; legal confirmed, new map evaluated with placements that produced it 
   ;; referee must add bonus for finish
   ;; SHOULD THIS BE JUST A PART OF `complete-turn`?? NO, because the ref adds the 'finish bonus'
   ;; the ref must consult the state and determine whether the active player has placed all tiles
   ;; --> introduce score+ function that determines by itself whether this is true??? 
   (->* (map? (listof placement?)) (#:finishing natural?) natural?)]
  
  [complete-turn
   ;; a possibly new map, points, the placed tiles, the tiles handed out yield a new game state
   #; {complete-turn s gmap delta old-tiles new-tiles}
   ;; PROTICOL assume that for some placements
   #; (legal s placements)
   ;; yields gmap
   #; (score gmap placements)
   ;; yields delta between `(state-map s)` and `gmap`
   (-> state? map? natural? (listof tile?) state?)]

  [apply-action
   (-> state? placement? state?)]

  [render-ref-state (-> state? 2:image?)]))

(module+ examples
  (provide
   ; +starter-tile
   ; +starter-coor
   starter-players
   ref-starter-state
   info-starter-state

   ref-starter-state-handout
   info-starter-state-handout

   +ref-starter-state
   info-+ref-starter-state
   
   handouts
   starter-players-handout

   +ref-atop-state
   
   special-state
   info-special-state
   bad-state
   info-bad-state))

(module+ json
  (provide
   #; {type JState     = { MAP : JMap, PLAYERS : [Listof JPlayer], TILES : (U Natural [Listof Tile]) }}
   #; {type JPlayer    = Natural || { SCORE : Natural, TILES : [Listof JTile]}}
   
   MAP PLAYERS SCORE TILES ; COORDINATE ATILE

   (contract-out
    [state->jsexpr (-> state? jsexpr?)]
    [jsexpr->state (-> jsexpr? (or/c state? #false))])))
  

;; ---------------------------------------------------------------------------------------------------
(require Qwirkle/Common/coordinates)
(require Qwirkle/Common/map)
(require Qwirkle/Common/placement)
(require Qwirkle/Common/tiles)
(require (prefix-in 2: 2htdp/image))
(require SwDev/Lib/list)

(module+ examples
  (require (submod Qwirkle/Common/map examples))
  (require (submod Qwirkle/Common/placement examples))
  (require (submod Qwirkle/Common/tiles examples)))

(module+ json
  (require (submod Qwirkle/Common/map json))
  (require (submod Qwirkle/Common/placement json))
  (require (submod Qwirkle/Common/tiles json))
  (require Qwirkle/Lib/parse-json)
  (require json))

(module+ test
  (require (submod ".."))
  (require (submod ".." examples))
  (require (submod ".." json))
  (require (submod Qwirkle/Common/coordinates examples))
  (require (submod Qwirkle/Common/placement examples))
  (require (submod Qwirkle/Common/map examples))
  (require (submod Qwirkle/Common/tiles examples))
  (require rackunit))

;; ---------------------------------------------------------------------------------------------------
(struct state [map players tiles] #:prefab)
(struct sop [score tiles player] #:prefab)

#; {SoPlayer -> SoPlayer}
(define (sop-special first)
  (match-define [sop score tiles payload] first)
  (cond
    [(or (symbol? payload) (string? payload)) first]
    [else (object-name payload)]))

;; ---------------------------------------------------------------------------------------------------
#; {[Y] Map [Listof [List [Listof Tile] Y]] -> [RefState Y]}
(define (create-ref-state gmap payload #:tiles0 (tiles0 '[]))
  (state gmap (map (λ (p) (apply sop 0 p)) payload) tiles0))

#; {[Y] [RefState Y] -> PubKnowledge}
(define (ref-state-to-info-state rgs)
  (match-define [state gmap (cons first players) tiles] rgs)
  (state gmap (cons (sop-special first) (map sop-score players)) (length tiles)))

(module+ examples
  (define handouts (make-list 6 #s(tile diamond green)))
  (define starter-players [list [list starter-tile* 'player1] [list qwirkle-tile* 'player2]])
  (define ref-starter-state (create-ref-state starter-map starter-players #:tiles0 handouts))
  (define info-starter-state (ref-state-to-info-state ref-starter-state))
  ;; what `ref-starter-state` evolves into after the tiles have been handed out 
  (define starter-players-handout [list [list qwirkle-tile* 'player2] [list handouts 'player1]])
  (define ref-starter-state-handout (create-ref-state starter-map starter-players-handout))
  (define info-starter-state-handout (ref-state-to-info-state ref-starter-state-handout)))

;; ---------------------------------------------------------------------------------------------------
#; {[Y] [RefStatet Y] Map Natural [Listof Tile] [Listof Tile] -> [RefState Y]}

(module+ test
  (check-equal? 
   (complete-turn ref-starter-state starter-map 0 starter-tile*)
   ref-starter-state-handout))

(define (complete-turn s new-map delta-score old-tile*)
  (match-define [state _ (cons first-player others) tiles] s)
  (define-values [handouts new-tiles] (replace-tiles old-tile* tiles))
  (define first-player++  (swap-tiles-and-points first-player delta-score old-tile* handouts))
  (define new-player-order (list-rotate+ (cons first-player++ others)))
  (state new-map new-player-order new-tiles))

#; {[Listof Tiles] [Listof Tiles] -> (values [Listof Tiles] [Listof Tiles])}
;; produce the list of tiles to be handed to the player and the remainder 
(define (replace-tiles placed-tiles tiles)
  (define n (length placed-tiles))
  (define k (length tiles))
  (if (< n k)
      (values (take tiles n) (drop tiles n))
      (values tiles '[])))

#; {Player N [Listof Tile] [Listof Tile] -> Player}
(define (swap-tiles-and-points player delta old-tile* new-tile*)
  (match-define [sop score tiles payload] player)
  (sop (+ score delta) (append new-tile* (remove* old-tile* tiles)) payload))

;; ---------------------------------------------------------------------------------------------------
(module+ examples ;; states and successor states 
  (define +ref-starter-state (create-ref-state starter-map (list (list (list +starter-tile) 'p12))))
  (define info-+ref-starter-state (ref-state-to-info-state +ref-starter-state))
  (define +ref-atop-state (create-ref-state map0 (list (list (list #s(tile circle red)) 'p12))))

  (define special-tiles (map placement-tile special-placements))
  (define special-state (create-ref-state special-map (list (list special-tiles 'ps))))
  (define info-special-state (ref-state-to-info-state special-state))

  (define bad-map   (legal special-state special-placements))
  (define bad-state (create-ref-state bad-map (list '((#s(tile square orange)) ps))))
  (define info-bad-state (ref-state-to-info-state bad-state)))

;; ---------------------------------------------------------------------------------------------------
;; apply a single placement to a PubKnowledge state w/o updating the score .. should it? 

(module+ test
  (define info-state-after-first-special-placement
    #s(state
       #hash((#s(coordinate -1 1) . #s(tile diamond green))
             (#s(coordinate -3 0) . #s(tile star red))
             (#s(coordinate -2 0) . #s(tile 8star red))
             (#s(coordinate -4 1) . #s(tile clover green))
             (#s(coordinate 0 0) . #s(tile circle red))
             (#s(coordinate 0 1) . #s(tile circle green))
             (#s(coordinate -4 0) . #s(tile clover red))
             (#s(coordinate -1 0) . #s(tile diamond red)))
       (#s(sop 0 (#s(tile star green)) ps))
       0))
  
  (check-equal? (apply-action info-special-state (first special-placements))
                info-state-after-first-special-placement "place first special on info-pk"))

#; {PubKnowledge Placement -> PubKnowledge}
(define (apply-action pk p)
  (match-define [state gmap (cons first others) pk-tiles]  pk)
  (define co (placement-coordinate p))
  (define ti (placement-tile p))
  (define new-map (add-tile gmap co ti))
  (match-define [sop score player-tiles payload] first)
  (define first++ (sop score (remove ti player-tiles) payload))
  (state new-map (cons first++ others) pk-tiles))

;; ---------------------------------------------------------------------------------------------------
;; legality of placements

#; {[Y] [RefState Y] Placement* -> Option<Map>}
;; are the placements legal according to the rules of Q? If so, produce the new map; otherwise #false
(define (legal gstate placements)
  (define placed-tiles (map placement-tile placements))
  (define coordinate*  (map placement-coordinate placements))
  (and
   (current-player-owns-tiles (first (state-players gstate)) placed-tiles)
   (or (same-row coordinate*) (same-column coordinate*))
   (all-adjacent-and-fits? (state-map gstate) placements)))

#; {Map Placemennt* -> Option<Map>}
;; create the map that the placements specify and check at each step that a tile can be placed & fits
(module+ test
  (check-equal? (all-adjacent-and-fits? map1 plmt1) map2)
  (check-true (map? (all-adjacent-and-fits? starter-map +starter-plmt)) "aa 1")
  (define plmnt-2-away (list (placement #s(coordinate +2 0) #s(tile circle red))))
  (check-false (all-adjacent-and-fits? starter-map plmnt-2-away) "aa 2"))
(define (all-adjacent-and-fits? gmap0 placements)
  (let/ec return 
    (for/fold ([gmap gmap0]) ([p placements])
      (define co (placement-coordinate p))
      (define ti (placement-tile p))
      (unless (and (adjacent? gmap co) (candidate? (fits gmap co ti)))
        (return #false))
      (add-tile gmap co ti))))

#; {Player [Listof Tile] -> Boolean}
(module+ test
  (define the-starter-player (apply sop 0 (first starter-players)))
  (check-true (current-player-owns-tiles the-starter-player starter-tile*) "it owns lshaped")
  (check-true (current-player-owns-tiles (sop 0 (cons +starter-tile tiles1) 'p) tiles1) "owns, 1"))
(define (current-player-owns-tiles player placed-tiles)
  (define tiles-owned (sop-tiles player))
  (for/and ([placed placed-tiles])
    (begin0
      (cons? (member placed tiles-owned))
      (set! tiles-owned (remove placed tiles-owned)))))

(module+ test ;; legal integration tests 
  (check-false (legal +ref-atop-state place-atop-starter) "b/c can't place tile atop another")
  (check-false (legal ref-starter-state lshaped-placement*) "b/c p* is lshaped")

  (check-true (map? (legal special-state special-placements)))
  (check-false (legal bad-state bad-spec-plmnt))

  #; {Map [Listof Placement] Option<Map> String -> Void}
  (define (check-legal gmap pp expected msg)
    (define tiles* (map placement-tile pp))
    (define gstate0 (create-ref-state gmap `[[,(cons +starter-tile tiles*) ,(~a 'player msg)]]))
    (check-equal? (legal gstate0 pp) expected msg))

  ;; run all scenarios
  (for ([m0 (list map0 map1 map2 map3 map4 map5 map6 map7 map8 map9 map10)]
        [m+ (list map1 map2 map3 map4 map5 map6 map7 map8 map9 map10 map11)]
        [pp (list plmt0 plmt1 plmt2 plmt3 plmt4 plmt5 plmt6 plmt7 plmt8 plmt9 plmt10)]
        [ii (in-naturals)])
    (check-legal m0 pp m+ (~a "step " ii))))

;; ---------------------------------------------------------------------------------------------------
;; scoring a placement 

#; {Map Placements -> Natural}
(define (score gmap placements #:finishing (finishing-bonus 0))
  (define coord (map placement-coordinate placements))
  (define line  (create-line gmap coord))
  (+ (length placements)                         ;; task 1 
     finishing-bonus                             ;; task 2 
     (score-same-line-segments gmap line coord)  ;; task 3 
     (score-orthoginal-lines gmap line coord)))  ;; task 4 

#; {Map Line [Listof Coordinate] -> Natural}
;; lengths for all segments of the placement line that contain a new placement, plus bonus 
(module+ test
  (define line2 (create-line map2 coord1))
  (check-equal? (score-same-line-segments map2 line2 coord1) 2))
(define (score-same-line-segments gmap line coords)
  (define segment* (if (row? line) (all-row-segments gmap line) (all-column-segments gmap line)))
  (let sum-of-overlaps ([segment* segment*])
    (cond
      [(empty? segment*) 0]
      [else (+ (bonus-for-full-row (contains-1-placement (first segment*) coords))
               (sum-of-overlaps (rest segment*)))])))

#;{Segment [Listof Coordinate] -> Natural}
;; lengt of segment if it contain one placement; 0 otherwise 
(define (contains-1-placement 1segment coords)
  (define count
    (for/first ([s 1segment] #:when (member s coords))
      (length 1segment)))
  (if (false? count) 0 count))

#; {Map [Listof Coord] -> Natural}
;; lengths for all lines orthogonal to the placement line that contain one new placement, plus bonus 
(define (score-orthoginal-lines gmap line coord)
  (define score (if (row? line) walk-column-orthogonally walk-row-orthogonally))
  (for/sum ([co coord])
    (bonus-for-full-row (score gmap co))))

#; {Natural -> Natural}
(define (bonus-for-full-row count)
  (if (= count 6) 12 count))

(module+ test ;; scoring tests 
  (define score1  10)
  (define score2   5)
  (define score3   8)
  (define score4   9)
  (define score5   8)
  (define score6   5)
  (define score7   5)
  (define score8  12)
  (define score9  10)
  (define score10 21)
  (define score11 11)
  
  (for ([m+ (list map1 map2 map3 map4 map5 map6 map7 map8 map9 map10 map11)]
        [pp (list plmt0 plmt1 plmt2 plmt3 plmt4 plmt5 plmt6 plmt7 plmt8 plmt9 plmt10)]
        [sc (list score1 score2 score3 score4 score5 score6 score7 score8 score9 score10 score11)]
        [ii (in-naturals)])
    (check-equal? (score m+ pp) sc (~a "scoring map " (+ ii 1))))

  (check-equal? (score map10 plmt9) score10 "Q bonus missing")
  (check-equal? (score (legal special-state special-placements) special-placements) 10 "2 segments"))

;; ---------------------------------------------------------------------------------------------------
;; render a referee state 

#; {[Y] [RefState Y] -> Image}
(define (render-ref-state gs)
  (match-define [state gmap (cons first [list sop ...]) _] gs)
  (define gmap-image (render-map gmap))
  (define sop-images (render-sop* first sop render-sop))
  (2:beside/align 'top gmap-image hblank sop-images))

#; {InforState -> Image}
(define (render-info-state is)
  (match-define [state gmap (cons first [list score ...]) _] is)
  (define gmap-image (render-map gmap))
  (define score-imgs (render-sop* first score (λ (s) (2:text (~a s) 20 'black))))
  (2:beside/align 'top gmap-image hblank score-imgs))

#; {[Y] SoPlayer Y [Y ->Image] -> Image}
(define (render-sop* one l-sop render-one)
  (define sop-images (cons (render-sop one) (map render-one l-sop)))
  (for/fold ([r (first sop-images)]) ([s (rest sop-images)])
    (2:above/align 'left r vblank s)))

#; {[Y] {SoPlayer Y} -> Image}
(define (render-sop 1sop)
  (match-define [sop score tiles player] 1sop)
  (define tiles-image (map render-tile tiles))
  (define score-image (2:text (number->string score) 20 'black))
  (define player-image (2:text (~a player) 20 'black))
  (apply 2:beside/align 'top player-image hblank score-image hblank tiles-image))

(define hblank (2:rectangle 10 1 'solid 'white))
(define vblank (2:rectangle 1 10 'solid 'white))
(module+ test
  'infor-starter-state
  (render-info-state info-starter-state)
  'ref-starte-state
  (render-ref-state ref-starter-state))

;; ---------------------------------------------------------------------------------------------------
(module+ json
  (define MAP 'map)
  (define PLAYERS 'players)
  (define SCORE 'score)
  (define TILES 'tile*)

  #; {PubKnowledge -> JState}
  (define (state->jsexpr rb)
    (match-define [state gmap players tiles] rb)
    (define jtiles (if (natural? tiles) tiles (map tile->jsexpr tiles)))
    (hasheq MAP (map->jsexpr gmap) PLAYERS (players->jsexpr players) TILES jtiles))

  #; {[Listof (U SoPlayer Natural)] -> [Listof JPLayer]}
  (define/contract (players->jsexpr players)
    (-> (cons/c sop? (listof natural?)) (listof jsexpr?))
    (map 1player->jsexpr players))

  #; {(U SoPlayer Natural) -> JPlayer}
  (define (1player->jsexpr 1player)
    (match 1player
      [(sop score tiles _) (hasheq SCORE score TILES (map tile->jsexpr tiles))]
      [(? natural?) 1player]))

  #; {JSexpr -> OPtion<Coordinate>}
  (def/jsexpr-> state
    #:object {[MAP map (? hash? gmap)]
              [PLAYERS players (cons first (list n ...))]
              [TILES (and t (or (? natural?) (list (? tile?) ...)))]}
    (cond
      [(natural? t) (state gmap (cons first n) t)]
      [else         (state gmap (cons first n) (jsexpr->tiles t))]))

  (def/jsexpr-> players
    #:array [(cons (app jsexpr->1player (? sop? first)) (list (? natural? n) ...)) (cons first n)])

  (def/jsexpr-> 1player
    #:object {[SCORE (? natural? s)] [TILES tiles (list t ...)]}
    (sop s t 'player1))

  (def/jsexpr-> tiles #:array [(list (app jsexpr->tile (? tile? t)) ...) t]) 
  
  ;; for testing only 
  (provide jsexpr->1player jsexpr->players jsexpr->tiles))
  
(module+ test
  (check-equal? (jsexpr->state (state->jsexpr info-starter-state)) info-starter-state))
