#lang racket

;; a data representation of the generic game-state knowledge
;; ---------------------------------------------------------------------------------------------------

(provide
 #; {type [GameState X Y Z] = [state Map [Cons [SoPlayer X] [Listof Z]] Y]}
 ;; where [SoPlayer X] is the active player 
 ;; where [Listof _] specifies the order of turns that players take from here on out
 
 state?
 
 #; {[X Y Z] [Y -> Image] -> [GameState X Y Z] -> Image}
 render-ref-state/g
 
 (contract-out
  [create-state        (-> map? sop? [listof any/c] any/c state?)]
  [transform-state     (-> (-> sop? sop?) (-> sop? any/c) (-> any/c any) (-> state? state?))]

  [state-map++         (-> state? map? state?)]
  [state-score++       (-> state? natural? state?)]
  [state-replace-tiles (-> state? [listof tile?] (values (listof tile?) state?))]

  ;; this is ref-state specific 
  [state-rotate        (-> state? state?)]
  
  [legal
   ;; is the series of placements legale in this state; if so computer the new map 
   (-> state? (listof placement?) (or/c #false map?))]
  
  [score
   ;; legal confirmed, new map evaluated with placements that produced it 
   ;; referee must add bonus for finish
   ;; SHOULD THIS BE JUST A PART OF `complete-turn`?? NO, because the ref adds the 'finish bonus'
   ;; the ref must consult the state and determine whether the active player has placed all tiles
   ;; --> introduce score+ function that determines by itself whether this is true??? 
   (->* (map? (listof placement?)) (#:finishing natural?) natural?)]))

(module+ examples
  (provide
   +ref-starter-state
   +ref-atop-state
   special-state
   special-state+green-circle-at--2-2
   bad-state))

(module+ json
  (provide
   MAP PLAYERS TILES

   #; {[X Y Z U W]
       [GameState X Y Z]
       [[Listof Y] -> [Listof W]]
       [Z -> U]
       ->
       {MAP : JMap, PLAYERS : [Cons JPlayer [Listof W]], TILES : U}} ;; an Object w/ 3 fields 
   state->jsexpr/g


   #; {[X Y Z]
       [JSexpr -> Option<Z>]
       [JSexpr -> Option<Cons [SoPlayer X] [Listof Y]>]
       -> JSexpr
       -> Option<[GameState X Y Z]>}
   jsexpr->state/g))

;; ---------------------------------------------------------------------------------------------------
(require Qwirkle/Common/coordinates)
(require Qwirkle/Common/state-of-player)
(require Qwirkle/Common/map)
(require Qwirkle/Common/placement)
(require Qwirkle/Common/tiles)
(require SwDev/Lib/list)
(require (prefix-in 2: 2htdp/image))

(module+ examples
  (require (submod Qwirkle/Common/map examples))
  (require (submod Qwirkle/Common/placement examples))
  (require (submod Qwirkle/Common/tiles examples)))

(module+ json
  (require (submod Qwirkle/Common/state-of-player json))
  (require (submod Qwirkle/Common/map json))
  (require Qwirkle/Lib/parse-json))

(module+ test 
  (require (submod ".."))
  (require (submod ".." examples))
  (require (submod Qwirkle/Common/placement examples))
  (require (submod Qwirkle/Common/map examples))
  (require (submod Qwirkle/Common/tiles examples))
  (require rackunit))

;; ---------------------------------------------------------------------------------------------------
(struct state [map players tiles] #:prefab)

#; {[X Y Z] Map [SoPlayer X] [Listof Y] Z -> [GameState X Y Z]}
(define (create-state gmap player-one others tiles)
  (state gmap (cons player-one others) tiles))

#; {[X Y Z U V W]
    [[SoPlayer X] -> {SoPlayer U}]
    [Y -> V]
    [Y -> W]
    -> 
    [GameState X Y Z]
    ->
    [GameState U V W]}
(define ((transform-state t-1player t-player* t-tiles) s)
  (match-define [state gmap (cons first players) tiles] s)
  (state gmap (cons (t-1player first) (map t-player* players)) (t-tiles tiles)))

#; {[X Y] [GameState X Y] -> [GameState X Y]}
(define (state-rotate s)
  (match-define [state map players tiles] s)
  (state map (list-rotate+ players) tiles))

#; {[X Y] [GameSTate X Y] [Listof Tiles] -> (values [Listof Tile] [GameSTate X Y])}
;; produce the list of tiles to be handed to the player and the remainder 
(define (state-replace-tiles s placed-tile*)
  (match-define [state map (cons first others) tiles] s)
  (define n (length placed-tile*))
  (define k (length tiles))
  (define-values [handouts tiles++]
    (if (< n k)
        (values (take tiles n) (drop tiles n))
        (values tiles          '[])))
  (define players++ (cons (hand-to first handouts placed-tile*) others))
  (values handouts (state map players++ tiles++)))

#; {[X Y] [GameSTate X Y] N -> [GameSTate X Y]}
(define (state-score++ s delta-score)
  (match-define [state map (cons first players) tiles] s)
  (state map (cons (sop-score++ first delta-score) players) tiles))

#; {[X Y] [GameSTate X Y] Map -> [GameSTate X Y]}
(define (state-map++ s new-map)
  (match-define [state map players tiles] s)
  (state new-map players tiles))

;; ---------------------------------------------------------------------------------------------------

;                                                          
;                                                          
;                                      ;;;                 
;                                        ;                 
;    ;;;   ;   ;  ;;;;  ;;;;;;  ;;;;     ;     ;;;    ;;;  
;   ;;  ;   ; ;       ; ;  ;  ; ;; ;;    ;    ;;  ;  ;   ; 
;   ;   ;;  ;;;       ; ;  ;  ; ;   ;    ;    ;   ;; ;     
;   ;;;;;;   ;     ;;;; ;  ;  ; ;   ;    ;    ;;;;;;  ;;;  
;   ;       ;;;   ;   ; ;  ;  ; ;   ;    ;    ;          ; 
;   ;       ; ;   ;   ; ;  ;  ; ;; ;;    ;    ;      ;   ; 
;    ;;;;  ;   ;   ;;;; ;  ;  ; ;;;;      ;;   ;;;;   ;;;  
;                               ;                          
;                               ;                          
;                               ;                          

(define (create-1player-state map0 active-player-spec)
  (state map0 (list (apply sop 0 active-player-spec)) '[]))

(module+ examples ;; states and successor states
  
  (define +ref-starter-state (create-1player-state starter-map (list (list +starter-tile) 'p12)))
  
  (define +ref-atop-state (create-1player-state map0 (list (list #s(tile circle red)) 'p12)))

  (define special-tiles (map placement-tile special-placements))
  (define special-state (create-1player-state special-map (list special-tiles 'ps)))
  
  (define special-state+green-circle-at--2-2
    (create-1player-state special-map+green-circle-at--2-2 `[[#s(tile circle orange)] ppp]))

  (define bad-map   (legal special-state special-placements))
  (define bad-state (create-1player-state bad-map '[(#s(tile square orange)) ps])))

;; ---------------------------------------------------------------------------------------------------
;; legality of placements

#; {[X Y Z] [GameState X Y Z] Placement* -> Option<Map>}
;; are the placements legal according to the rules of Q? If so, produce the new map; otherwise #false
(define (legal gstate placements)
  (define placed-tiles (map placement-tile placements))
  (define coordinate*  (map placement-coordinate placements))
  (and
   (player-owns-tiles (first (state-players gstate)) placed-tiles)
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

(module+ test ;; legal integration tests 
  (check-false (legal +ref-atop-state place-atop-starter) "b/c can't place tile atop another")
  (check-false (legal bad-state bad-spec-plmnt))
  (check-true (map? (legal special-state special-placements)))

  (check-equal? (legal special-state+green-circle-at--2-2 place-green-circle-at--2-2)
                special-map+green-circle-at--2-2++)

  #; {Map [Listof Placement] Option<Map> String -> Void}
  (define (check-legal gmap pp expected msg)
    (define tiles* (map placement-tile pp))
    (define gstate0 (create-1player-state gmap `[,(cons +starter-tile tiles*) ,(~a 'player msg)]))
    (check-equal? (legal gstate0 pp) expected msg))

  ;; run all scenarios
  (for ([m0 (list map0 map1 map2 map3 map4 map5 map6 map7 map8 map9 map10)]
        [m+ (list map1 map2 map3 map4 map5 map6 map7 map8 map9 map10 map11)]
        [pp (list plmt0 plmt1 plmt2 plmt3 plmt4 plmt5 plmt6 plmt7 plmt8 plmt9 plmt10)]
        [ii (in-naturals)])
    (check-legal m0 pp m+ (~a "step " ii))))

;; ---------------------------------------------------------------------------------------------------
;; scoring placements

(define Q-BONUS 6)

#; {Map Placements -> Natural}
(define (score gmap placements #:finishing (finishing-bonus 0))
  (define coord (map placement-coordinate placements))
  (define line  (create-line gmap coord))
  (+ (length placements)                             ;; task 1 
     finishing-bonus                                 ;; task 2 
     (score-same-line-segments gmap line placements) ;; task 3 
     (score-orthoginal-lines gmap line placements))) ;; task 4 

#; {Map Line [Listof Coordinate] -> Natural}
;; lengths for all segments of the placement line that contain a new placement, plus bonus 
(define (score-same-line-segments gmap line placements)
  (define segment* (if (row? line) (all-row-segments gmap line) (all-column-segments gmap line)))
  (for/sum ([1segment segment*])
    (define the-tiles (map placement-tile 1segment))
    (q-bonus the-tiles (contains-1-placement 1segment placements))))

#;{Segment [Listof Placement] -> Natural}
;; lengt of segment if it contain one placement; 0 otherwise 
(define (contains-1-placement 1segment placements)
  (define count (for/first ([p 1segment] #:when (member p placements)) (length 1segment)))
  (if (false? count) 0 count))

#; {Map [Listof Placements] -> Natural}
;; lengths for all lines orthogonal to the placement line that contain one new placement, plus bonus
(define (score-orthoginal-lines gmap line placements)
  (define coord (map placement-coordinate placements))
  (define walk (if (row? line) walk-column-orthogonally walk-row-orthogonally))
  (for/sum ([co coord])
    (define continuous-run (walk gmap co))
    (q-bonus continuous-run (length continuous-run))))

#; {[Listof Tile] Natural -> Natural}
(define (q-bonus line count)
  (if (or (all-colors? line) (all-shapes? line)) (+ Q-BONUS count) count))

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

  ;; not a run of like-tiles or colors and not a Q !! 
  (check-equal? (score special-map+green-circle-at--2-2++ place-green-circle-at--2-2) 7 "not run")
  

  (check-equal? (score map10 plmt9) score10 "Q bonus missing")
  (check-equal? (score (legal special-state special-placements) special-placements) 10 "2 segments"))

;; ---------------------------------------------------------------------------------------------------
(define ((render-ref-state/g render-sop) gs)
  (match-define [state gmap (cons first [list sop ...]) _] gs)
  (define gmap-image (render-map gmap))
  (define sop-images (render-sop* first sop render-sop))
  (2:beside/align 'top gmap-image hblank sop-images))

(define hblank (2:rectangle 10 1 'solid 'white))

;; ---------------------------------------------------------------------------------------------------

(module+ json

  (define MAP 'map)
  (define PLAYERS 'players)
  
  #; {[X Y Z U W]
      [GameState X Y Z]
      [[Listof Y] -> [Listof W]]
      [Z -> U]
      ->
      {MAP : JMap, PLAYERS : [Cons JPlayer [Listof W]], TILES : U}}
  (define ((state->jsexpr/g players->jsexpr tiles->jsexpr) s)
    (match-define [state gmap (cons active players) tiles] s)
    (define jactive  (1player->jsexpr active))
    (define jplayers (players->jsexpr players))
    (hasheq MAP     (map->jsexpr gmap)
            PLAYERS (cons jactive jplayers)
            TILES   (tiles->jsexpr tiles)))

  #; {[X Y Z]
      [JSexpr -> Option<Z>]
      [JSexpr -> Option<Cons [SoPlayer X] [Listof Y]>]
      -> JSexpr
      -> Option<[GameState X Y Z]>}
  (define ((jsexpr->state/g jsexpr->players jsexpr->tiles) j)
    (def/jsexpr-> state
      #:object {[MAP     map     (? hash? gmap)]
                [PLAYERS players (cons (? sop? first) p)]
                [TILES   tiles   (? identity t)]} ;; not #false 
      (state gmap (cons first p) t))
    (jsexpr->state j)))
