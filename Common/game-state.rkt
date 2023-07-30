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
  
  [ref-state-to-info-state (-> state? state?)]
  
  [legal?
   #; {(U PubKnowledge [RefState Y]) [Listof Placement] -> (U Map? False)}
   (-> state? (listof placement?) (or/c #false map?))]
  
  [score
   ;; legal confirmed, new map evaluated with placements that produced it 
   ;; referee must add bonus for finish
   ;; SHOULD THIS BE JUST A PART OF `complete-turn`?? NO, because the ref adds the 'finish bonus'
   (-> map? (listof placement?) natural?)]
  
  [complete-turn
   ;; a possibly new map, points, the placed tiles, the tiles handed out yield a new game state
   #; {complete-turn s gmap delta old-tiles new-tiles}
   ;; PROTICOL assume that for some coord* 
   #; (legal? s (map placement cord* old-tiles))
   ;; yields gmap
   #; (score s gmap)
   ;; yields delta 
   (-> state? map? natural? (listof tile?) (listof tile?) state?)]

  [render-ref-state (-> state? 2:image?)]))

(module+ examples
  (provide
   +starter-tile
   +starter-coor
   +starter-plmt
   +ref-starter-state
   
   starter-players
   ref-starter-state
   +ref-atop-state
   handouts
   starter-players-handout
   ref-starter-state-handout
   
   info-starter-state
   place-atop-starter
   lshaped-placement*))

(module+ json
  (provide
   MAP PLAYERS SCORE TILES COORDINATE ATILE

   (contract-out
    [placements->jsexpr (-> (listof placement?) (listof jsexpr?))]
    [jsexpr->placements (-> (listof jsexpr?) (or/c (listof placement?) #false))]
    [state->jsexpr (-> state? jsexpr?)]
    [jsexpr->state (-> jsexpr? (or/c state? #false))])))
  

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

(module+ json
  (require (submod Qwirkle/Common/map json))
  (require (submod Qwirkle/Common/coordinates json))
  (require (submod Qwirkle/Common/tiles json))
  (require json))

(module+ test
  (require (submod ".."))
  (require (submod ".." examples))
  (require (submod ".." json))
  (require (submod Qwirkle/Common/coordinates examples))
  (require (submod Qwirkle/Common/map examples))
  (require (submod Qwirkle/Common/tiles examples))
  (require rackunit))

;; ---------------------------------------------------------------------------------------------------
(struct state [map players] #:prefab)
(struct sop [score tiles player] #:prefab)

#; {type [GameState X] = [rgs Map [Listof X]]}
;;      where [Listof X] specifies the order of turns that players take from here on out 
#; {type [RefState Y]  = [GameState [SoPlayer Y]]}
;;      what the referee knows about the game 
#; {type PubKnowledge  = [GameState (cons [SoPlayer (U String Symbol)] Natural)]}
;;      what a player may know about the game: its own state (named) and the score of others
#; {type [SoPlayer Y]  = [sop Natural [Listof Tile] Y]}
;;      where Y is typically an external player or just a symbolic name 

#; {SoPlayer -> SoPlayer}
(define (sop-special first)
  (match-define [sop score tiles payload] first)
  (cond
    [(or (symbol? payload) (string? payload)) first]
    [else (object-name payload)]))

;; ---------------------------------------------------------------------------------------------------
#; {[Y] Map [Listof [List [Listof Tile] Y]] -> [RefState Y]}
(define (create-ref-state gmap payload)
  (state gmap (map (λ (p) (apply sop 0 p)) payload)))

#; {[Y] [RefState Y] -> PubKnowledge}
(define (ref-state-to-info-state rgs)
  (match-define [state gmap (cons first players)] rgs)
  (state gmap (cons (sop-special first) (map sop-score players))))

(module+ examples
  (define starter-players [list [list starter-tile* 'player1] [list qwirkle-tile* 'player2]])
  (define ref-starter-state (create-ref-state starter-map starter-players))
  (define info-starter-state (ref-state-to-info-state ref-starter-state))
  
  (define handouts (make-list 6 #s(tile diamind green)))
  (define starter-players-handout [list [list qwirkle-tile* 'player2] [list handouts 'player1]])
  (define ref-starter-state-handout (create-ref-state starter-map starter-players-handout)))

;; ---------------------------------------------------------------------------------------------------
#; {[RefStatet Y] Map Natural [Listof Tile] [Listof Tile] -> [RefState Y]}

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
  (define place-atop-starter (list (placement origin #s(tile circle red))))
  (define lshaped-placement* (map placement lshaped-coordinates starter-tile*))
  (define +starter-plmt (list (placement +starter-coor +starter-tile)))
  (define +ref-starter-state (state starter-map (list (sop 0 (list +starter-tile) 'player13))))
  (define +ref-atop-state (state map0 (list (sop 0 (list #s(tile circle red)) 'player13)))))

#; {[RefState Y] Placement* -> Option<Map>}
;; are the placements legal according to the rules of Q? If so, produce the new map; otherwise #false

(module+ test
  (check-false (legal? +ref-atop-state place-atop-starter) "b/c can't place tile atop another")
  (check-false (legal? ref-starter-state lshaped-placement*) "b/c p* is lshaped")

  #; {[Any Map String -> Void] Map [Listof Tile] [Listoor Coordinate] Option<Map> String -> Void}
  (define (check-legal check gmap tiles* coord* expected msg)
    (define plmnt0 (map placement coord* tiles*))
    ;; for socring there should be no 'all tiles down' bonus 
    (define gstate0 (create-ref-state gmap `[[,(cons +starter-tile tiles*) ,(~a 'player msg)]]))
    (check (legal? gstate0 plmnt0) expected msg))

  ;; run all scenarios
  (for ([m0 (list map0 map1 map2 map3 map4 map5 map6 map7 map8 map9 map10)]
        [m+ (list map1 map2 map3 map4 map5 map6 map7 map8 map9 map10 map11)]
        [tt (list tiles0 tiles1 tiles2 tiles3 tiles4 tiles5 tiles6 tiles7 tiles8 tiles9 tiles10)]
        [cc (list coord0 coord1 coord2 coord3 coord4 coord5 coord6 coord7 coord8 coord9 coord10)]
        [ii (in-naturals)])
    (check-legal check-equal? m0 tt cc m+ (~a "step " ii))))

(define (legal? gstate placements)
  (define placed-tiles (map placement-tile placements))
  (define coordinate*  (map placement-coordinate placements))
  (and
   (current-player-owns-tiles (first (state-players gstate)) placed-tiles)
   (or (same-row coordinate*) (same-column coordinate*))
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

;; ---------------------------------------------------------------------------------------------------
;; scoring a placement 

#; {Map Placements -> Natural}
(define (score gmap placements)
  (define coord (map placement-coordinate placements))
  (define line  (create-line gmap coord))
  (+ (length placements)
     (score-same-line-segments gmap line coord)
     (score-orthoginal-lines gmap line coord)))

#; {Map Line [Listof Coordinate] -> Natural}
;; lengths for all segments of the placement line that contain a new placement
(module+ test
  (define line2 (create-line map2 coord1))
  (check-equal? (score-same-line-segments map2 line2 coord1) 2))
(define (score-same-line-segments gmap line coords)
  (define segment* (if (row? line) (all-row-segments gmap line) (all-column-segments gmap line)))
  (let sum-of-overlaps ([segment* segment*])
    (cond
      [(empty? segment*) 0]
      [else (+ (contains-1-placement (first segment*) coords) (sum-of-overlaps (rest segment*)))])))

#;{Segment [Listof Coordinate] -> Natural}
(define (contains-1-placement 1segment coords)
  (define count
    (for/first ([s 1segment] #:when (member s coords))
      (length 1segment)))
  (bonus-for-full-row (if (false? count) 0 count)))

#; {Map [Listof Coord] -> Natural}
;; lengths for all lines orthogonal to the placement line that contain one new placement 
(define (score-orthoginal-lines gmap line coord)
  (define score (if (row? line) walk-column-orthogonally walk-row-orthogonally))
  (for/sum ([co coord])
    (bonus-for-full-row (score gmap co))))

#; {Natural -> Natural}
(define (bonus-for-full-row count)
  (if (= count 6) 12 count))

(module+ test
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

  #; {[Any Map String -> Void] Map [Listof Tile] [Listoor Coordinate] Option<Map> String -> Void}
  (define (check-score gmap coord* tiles* expected msg)
    (define plmnt0 (map placement coord* tiles*))
    ;; for socring there should be no 'all tiles down' bonus 
    (check-equal? (score gmap plmnt0) expected msg))

  ;; run all scenarios
  (for ([m+ (list map1 map2 map3 map4 map5 map6 map7 map8 map9 map10 map11)]
        [tt (list tiles0 tiles1 tiles2 tiles3 tiles4 tiles5 tiles6 tiles7 tiles8 tiles9 tiles10)]
        [cc (list coord0 coord1 coord2 coord3 coord4 coord5 coord6 coord7 coord8 coord9 coord10)]
        [sc (list score1 score2 score3 score4 score5 score6 score7 score8 score9 score10 score11)]
        [ii (in-naturals)])
    (check-score m+ cc tt sc (~a "scoring map " (+ ii 1))))

  (check-equal? (score map10 (map placement coord9 tiles9)) score10 "Q bonus missing"))

;; ---------------------------------------------------------------------------------------------------
;; render a referee state 

#; {[Y] [RefState Y] -> Image}
(define (render-ref-state gs)
  (match-define [state gmap (cons first [list sop ...])] gs)
  (define gmap-image (render-map gmap))
  (define sop-images (render-sop* first sop render-sop))
  (2:beside/align 'top gmap-image hblank sop-images))

#; {InforState -> Image}
(define (render-info-state is)
  (match-define [state gmap (cons first [list score ...])] is)
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
  (define ATILE '1tile)
  (define COORDINATE 'coordinate)

  #; {type JState     = { MAP : JMap, PLAYERS : [Listof JPlayer] }}
  #; {type JPlayer    = Natural || { SCORE : Natural, TILES : [Listof JTile]}}
  #; {type Placemenst = [Listof 1Placement]}
  #; {type 1Placement = { TILE : JTile, COORDINATE : JCoordinate }}

  #; {PubKnowledge -> JState}
  (define (state->jsexpr rb)
    (match-define [state gmap players] rb)
    (hasheq MAP (map->jsexpr gmap) PLAYERS (players->jsexpr rb  players)))

  #; {Any [Listof (U SoPlayer Natural)] -> [Listof JPLayer]}
  (define (players->jsexpr gstate players)
    (match players
      [(list (? sop?) (? natural? n) ...) (map 1player->jsexpr players)]
      [_ (error 'state->jsexpr "PubKnowledge expected, given ~v" gstate)]))

  #; {(U SoPlayer Natural) -> JPlayer}
  (define (1player->jsexpr 1player)
    (match 1player
      ([sop score tiles _] (hasheq SCORE score TILES (map tile->jsexpr tiles)))
      ([? natural?] 1player)))

  #; {Placements -> [Listof JPlacement]}
  (define (placements->jsexpr p*)
    (map 1placement->jsexpr p*))
  
  #; {1Placement -> 1Placement}
  (define (1placement->jsexpr p)
    (match-define [placement co ti] p)
    (hasheq ATILE (tile->jsexpr ti) COORDINATE (coordinate->jsexpr co)))
  
  #; {JSexpr -> OPtion<Coordinate>}
  (define (jsexpr->state j)
    (match j
      [(hash-table
        [(? (curry eq? MAP)) (app jsexpr->map (? hash? gmap))]
        [(? (curry eq? PLAYERS)) (app jsexpr->players (cons first (list n ...)))])
       (state gmap (cons first n))]
      [_ (eprintf "state object does not match schema\n  ~a\n" (jsexpr->string j #:indent 2))
         #false]))

  #; {JSexpr -> Option<PubKnknowledge>}
  ;; used on player side only 
  (define (jsexpr->players j)
    (match j
      [(cons (app jsexpr->1player (? sop? first)) (list (? natural? n) ...))
       (cons first n)]
      [_ (eprintf "players array does not match schema\n  ~a\n" (jsexpr->string j #:indent 2))
         #false]))

  #; {JSexpr -> Option{SoPlayer}}
  (define (jsexpr->1player j)
    (match j
      [(hash-table
        [(? (curry eq? SCORE)) (? natural? s)]
        [(? (curry eq? TILES)) (app jsexpr->tiles (list t ...))])
       (sop s t 'player1)]
      [_ (eprintf "player object does not match schema\n  ~a\n" (jsexpr->string j #:indent 2))
         #false]))

  #; {JSexpr -> Option{[Listof Tile]}}
  (define (jsexpr->tiles j)
    (match j
      [(list (app jsexpr->tile (? tile? t)) ...) t]
      [_ (eprintf "tiles array does not match schema\n  ~a\n" (jsexpr->string j))
         #false]))

  #; {JSexpr -> Option{[Listof Placement]}}
  (define (jsexpr->placements j)
    (match j
      [(list (app jsexpr->1placement (? placement? p)) ...) p]
      [_ (eprintf "placements array does not match schema\n  ~a\n" (jsexpr->string j))
         #false]))

  #; {JSexpr -> Option{Placement>}}
  (define (jsexpr->1placement j)
    (match j
      [(hash-table
        [(? (curry eq? COORDINATE)) (app jsexpr->coordinate (? coordinate? co))]
        [(? (curry eq? ATILE)) (app jsexpr->tile (? tile? ti))])
       (placement co ti)]
      [_ (eprintf "placement array does not match schema\n  ~a\n" (jsexpr->string j))
         #false]))

  (provide jsexpr->1player jsexpr->players jsexpr->tiles jsexpr->1placement))
  
(module+ test
  (check-equal? (jsexpr->state (state->jsexpr info-starter-state)) info-starter-state)
  
  (define placement0 (map placement coord0 tiles0))
  (check-equal? (jsexpr->placements (placements->jsexpr placement0)) placement0)

  ;; -------------------------------------------------------------------------------------------------
  (define-syntax-rule (check-message port rgxp body ...)
    (let ([os (open-output-string)]
          [mg (~a "looking for " rgxp)])
      (begin0
        (parameterize ([port os])
          body ...)
        (close-output-port os)
        (check-true (cons? (regexp-match rgxp (get-output-string os))) mg))))
  
  (check-false (check-message current-error-port  #px"state" (jsexpr->state 1)))
  (check-false (check-message current-error-port  #px"player" (jsexpr->1player 1)))
  (check-false (check-message current-error-port  #px"players" (jsexpr->players 1)))
  (check-false (check-message current-error-port  #px"tiles" (jsexpr->tiles 1)))
  (check-false (check-message current-error-port  #px"placements" (jsexpr->placements '["1"])))
  (check-false (check-message current-error-port  #px"placement" (jsexpr->1placement 1))))
