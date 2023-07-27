#lang racket

;; a data representation of the referee's knowledge and the player's knowledge about the game

(provide
 #; {type [RefState Y]}
 state?

 #; {type Placement = [Listof (placement Coordinate Tile)]}
 placement? 

 (contract-out
  (placement (-> coordinate? tile? placement?))

  [create-ref-state (-> map? [listof [list/c [listof tile?] any/c]] state?)]
  [render-ref-state (-> state? 2:image?)]
  [legal?           (-> state? (listof placement?) (or/c #false map?))]
  [complete-turn
   ;; a possibly new map, points, the placed tiles, the tiles handed out yield a new game state
   #; {complete-turn s gmap delta old-tiles new-tiles}
   ;; PROTICOL assume that for some coord* 
   #; (legal? s (map placement cord* old-tiles))
   ;; yields gmap
   #; (score s gmap)
   ;; yields delta 
   (-> state? map? natural? (listof tile?) (listof tile?) state?)]))

(module+ examples
  (provide
   +starter-tile
   +starter-coor
   +starter-plmt
   +ref-starte-state
   
   starter-players
   ref-starter-state
   handouts
   starter-players-handout
   ref-starter-state-handout
   
   info-starter-state
   lshaped-placement*))

;; ---------------------------------------------------------------------------------------------------
(require Qwirkle/Common/coordinates)
(require Qwirkle/Common/map)
(require Qwirkle/Common/tiles)
(require (prefix-in 2: 2htdp/image))
(require SwDev/Lib/list)

(module+ examples
  (require (submod Qwirkle/Common/map examples))
  (require (submod Qwirkle/Common/coordinates examples))
  (require (submod Qwirkle/Common/tiles examples)))

(module+ test
  (require (submod ".."))
  (require (submod ".." examples))
  (require (submod Qwirkle/Common/coordinates examples))
  (require (submod Qwirkle/Common/map examples))
  (require (submod Qwirkle/Common/tiles examples))
  (require rackunit))

;; ---------------------------------------------------------------------------------------------------
(struct state [map players] #:prefab)
(struct sop [score tiles player] #:prefab)

#; {type [GameState X] = [rgs Map [Listof X]]}
;;      where [Listof X] specifies the order of 
#; {type [RefState Y]  = [GameState [SoPlayer Y]]}
#; {type InfoState     = [GameState Natural]}
#; {type [SoPlayer Y]  = [sop Natural [Listof Tile] Y]}
;;      where Y is typically an external player or just a symbol 

;; ---------------------------------------------------------------------------------------------------
#; {[Y] Map [Listof [List [Listof Tile] Y]] -> [RefState Y]}
(define (create-ref-state gmap payload)
  (state gmap (map (λ (p) (apply sop 0 p)) payload)))

#; {[Y] [RefState Y] -> InfoState}
(define (ref-state-to-info-state rgs)
  (match-define [state gmap players] rgs)
  (state gmap (map sop-score players)))

(module+ examples
  (define starter-players [list [list starter-tile* 'player1] [list qwirkle-tile* 'player2]])
  (define ref-starter-state (create-ref-state starter-map starter-players))
  (define info-starter-state (ref-state-to-info-state ref-starter-state))
  
  (define handouts (make-list 6 #s(tile diamind green)))
  (define starter-players-handout [list [list qwirkle-tile* 'player2] [list handouts 'player1]])
  (define ref-starter-state-handout (create-ref-state starter-map starter-players-handout)))

;; ---------------------------------------------------------------------------------------------------
#; {State Map Natural [Listof Tile] [Listof Tile] -> State}

(module+ test
  (check-equal? 
   (complete-turn ref-starter-state starter-map 0 starter-tile* handouts)
   ref-starter-state-handout))

(define (complete-turn s new-map delta-score old-tile* new-tiles)
  (match-define [state _ (cons first-player others)] s)
  (define first-player++  (swap-tiles-and-points first-player delta-score old-tile* new-tiles))
  (define new-player-order (list-rotate+ (cons first-player++ others)))
  (state new-map new-player-order))

#; {Player N [Listof Tile] [Listof Tile] -> Player}
(define (swap-tiles-and-points player delta old-tile* new-tile*)
  (match-define [sop score tiles payload] player)
  (sop (+ score delta) (append new-tile* (remove* old-tile* tiles)) payload))

;; ---------------------------------------------------------------------------------------------------
#; {type Placement* = [Listof Placement]}
;; placements in the order in which the tiles are put down 
#; {type Placement  = [placement Coordinate Tile]}
(struct placement [coordinate tile] #:prefab)

(module+ examples
  (define lshaped-placement* 
    (for/list ([t starter-tile*] [co lshaped-coordinates]) (placement co t)))
  (define +starter-plmt (list (placement +starter-coor +starter-tile)))
  (define +ref-starte-state (state starter-map (list (sop 0 (list +starter-tile) 'player13)))))

#; {[RefState Y] Placement* -> Option<Map>}
;; are the placements legal according to the rules of Q? If so, produce the new map; otherwise #false

(module+ test
  (check-false (legal? ref-starter-state lshaped-placement*) "b/c p* is lshaped")

  #; {[Any Map String -> Void] Map [Listof Tile] [Listoor Coordinate] Option<Map> String -> Void}
  (define (check-legal check gmap tiles* coord* expected msg)
    (define plmnt0 (map placement coord* tiles*))
    ;; for socring there should be no 'all tiles down' bonus 
    (define player0 (sop 0 (cons +starter-tile tiles*) (~a 'player msg)))
    (define gstate0 (state gmap (list player0)))
    (check (legal? gstate0 plmnt0) expected msg))

  ;; run all scenarios 
  (for ([m0 (list map0 map1 map2 map3 map4 map5 map6 map7 map8 map9 map10)]
        [m+ (list map1 map2 map3 map4 map5 map6 map7 map8 map9 map10 map11)]
        [tt (list tiles0 tiles1 tiles2 tiles3 tiles4 tiles5 tiles6 tiles7 tiles8 tiles9 tiles10)]
        [cc (list coord0 coord1 coord2 coord3 coord4 coord5 coord6 coord7 coord8 coord9 coord10)]
        [ii (in-naturals)])
    (check-legal check-equal? m0 tt cc m+ (~a "step " ii))))

(define (legal? gstate placements)
  (and
   (current-player-owns-tiles (first (state-players gstate)) (map placement-tile placements))
   (or (same-rows placements) (same-columns placements))
   (all-adjacent-and-fits? (state-map gstate) placements)))

#; {Map Placemennt* -> Option<Map>}
;; create the map that the placements specify and check at each step that a tile can be placed & fits

(module+ test
  (define plmnt1 (map placement coord1 tiles1))
  (check-equal? (all-adjacent-and-fits? map1 plmnt1) map2)
  (check-true (map? (all-adjacent-and-fits? starter-map +starter-plmt)))

  (define plmnt-2-away (list (placement #s(coordinate +2 0) #s(tile circle red))))
  (check-false (all-adjacent-and-fits? starter-map plmnt-2-away)))

(define (all-adjacent-and-fits? gmap0 placements)
  (let/ec return 
    (for/fold ([gmap gmap0]) ([p placements])
      (match-define [placement co ti] p)
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

#; {[Placement -> M] -> Placement* -> Boolean}
(define [(same selector) placements]
  (apply = (map selector placements)))

(define same-rows (same (compose coordinate-row placement-coordinate)))
(define same-columns (same (compose coordinate-column placement-coordinate)))

(module+ test
  (check-true (same-rows plmnt1)))

;; ---------------------------------------------------------------------------------------------------
;; scoring a placement 

(module+ test 
  (define score1 10)
  (define score2 7)
  (define score3 9)
  (define score4 10)
  (define score5 9)
  (define score6 6)
  (define score7 6)
  (define score8 12)
  (define score9 10)
  (define score10 21)
  (define score11 12))

;; ---------------------------------------------------------------------------------------------------
;; render a referee state 

#; {[Y] [RefState Y] -> Image}
(define (render-ref-state gs)
  (match-define [state gmap [list sop ...]] gs)
  (define gmap-image (render-map gmap))
  (define sop-images (render-sop* sop render-sop))
  (2:beside/align 'top gmap-image hblank sop-images))

#; {InforState -> Image}
(define (render-info-state is)
  (match-define [state gmap [list score ...]] is)
  (define gmap-image (render-map gmap))
  (define score-imgs (render-sop* score (λ (s) (2:text (~a s) 20 'black))))
  (2:beside/align 'top gmap-image hblank score-imgs))

#; {[Y] Y [Y ->Image] -> Image}
(define (render-sop* l-sop render-sop)
  (define sop-images (map render-sop l-sop))
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
