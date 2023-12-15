#lang racket

;; a data representation of the generic game-state knowledge
;; ---------------------------------------------------------------------------------------------------

(provide
 #; {type [GameState X Y Z] = [state Map [Cons [SoPlayer X] [Listof Z]] Y]}
 ;; where [SoPlayer X] is the active player 
 ;; where [Listof _] specifies the order of turns that players take from here on out
 
 #; {type [RefState X]  = [GameState X [SoPlayer X] [Listof Tile]]}
 ;;      what the referee knows about the game 
 #; {type PubKnowledge  = [GameState String Natural Natural]}
 ;;      active player knows: its own state with nam; the scores of others; order of play; tiles left
 #; {type [SoPlayer Y]  = [sop Natural [Listof Tile] Y]}
 ;;      where Y is typically an external player or just a symbolic name


 state?
 state-map
 state-players 
 state-tiles
 
 #; {[X Y Z] [Y -> Image] -> [GameState X Y Z] -> Image}
 render-ref-state/g
 
 (contract-out
  [create-state          (-> map? sop? [listof any/c] any/c state?)]
  [default-referee-state state?]
  
  [transform-state      (-> (-> sop? sop?) (-> sop? any/c) (-> any/c any) (-> state? state?))]

  [state-map++          (-> state? map? state?)]

  [active-player        (-> state? sop?)]
  [active-sop-score++   (-> state? natural? state?)]
  [active-sop-tiles--   (-> state? (listof tile?) state?)]
  [active-sop-finished? (->* (state?) [(listof tile?)] boolean?)]
  [active-sop-hand      (-> state? (listof tile?) state?)]
  [active-sop-tiles     (-> state? (listof tile?))]

  [apply-action
   ;; apply a one-tile placement to the state 
   (-> state? placement? state?)]

  [legal
   ;; is the series of placements legale in this state; if so compute the new map 
   (-> state? (listof placement?) (or/c #false map?))]
  
  [score
   ;; legal confirmed, new map evaluated with placements that produced it 
   ;; referee must add bonus for finish
   ;; SHOULD THIS BE JUST A PART OF `complete-turn`?? NO, because the ref adds the 'finish bonus'
   ;; the ref must consult the state and determine whether the active player has placed all tiles
   ;; --> introduce score+ function that determines by itself whether this is true??? 
   (->* (map? (listof placement?)) (#:finishing natural?) natural?)]))

;; ---------------------------------------------------------------------------------------------------
(module+ examples ;; these are all referee states with tiles and all
  (provide Tests/ ForStudents/ Local)
  
  (provide
   +starter-state
   special-state
   special-state+green-circle-at--2-2
   bad-state
   
   state*
   state++*))

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

;                                                          
;                                                          
;                                  ;                       
;                                                          
;    ;;;;   ;;;    ;;;;  ;   ;   ;;;    ;;;;   ;;;    ;;;  
;    ;;  ; ;;  ;  ;; ;;  ;   ;     ;    ;;  ; ;;  ;  ;   ; 
;    ;     ;   ;; ;   ;  ;   ;     ;    ;     ;   ;; ;     
;    ;     ;;;;;; ;   ;  ;   ;     ;    ;     ;;;;;;  ;;;  
;    ;     ;      ;   ;  ;   ;     ;    ;     ;          ; 
;    ;     ;      ;; ;;  ;   ;     ;    ;     ;      ;   ; 
;    ;      ;;;;   ;;;;   ;;;;   ;;;;;  ;      ;;;;   ;;;  
;                     ;                                    
;                     ;                                    
;                     ;                                    

(require Qwirkle/scribblings/spec)
(require Qwirkle/Common/coordinates)
(require Qwirkle/Common/state-of-player)
(require Qwirkle/Common/map)
(require Qwirkle/Common/placement)
(require Qwirkle/Common/tiles)
(require (only-in (submod Qwirkle/Common/map examples) starter-map))
(require (prefix-in 2: 2htdp/image))

(module+ examples
  (require (submod Qwirkle/Common/map examples))
  (require (submod Qwirkle/Common/placement examples))
  (require (submod Qwirkle/Common/tiles examples))
  (require (for-syntax syntax/parse))
  (require (for-syntax racket/syntax)))

(module+ json
  (require (submod Qwirkle/Common/state-of-player json))
  (require (submod Qwirkle/Common/map json))
  (require Qwirkle/Lib/parse-json))

(module+ test 
  (require (submod ".."))
  (require (submod ".." examples))
  (require (submod Qwirkle/Common/placement examples))
  (require (submod Qwirkle/Common/tiles examples))
  (require (submod Qwirkle/Common/map examples))
  (require rackunit))

;                                                                 
;       ;                                  ;            ;;        
;       ;           ;                      ;           ;          
;       ;           ;                      ;           ;          
;    ;;;;  ;;;;   ;;;;;  ;;;;           ;;;;   ;;;   ;;;;;        
;   ;; ;;      ;    ;        ;         ;; ;;  ;;  ;    ;          
;   ;   ;      ;    ;        ;         ;   ;  ;   ;;   ;          
;   ;   ;   ;;;;    ;     ;;;;         ;   ;  ;;;;;;   ;          
;   ;   ;  ;   ;    ;    ;   ;         ;   ;  ;        ;          
;   ;; ;;  ;   ;    ;    ;   ;         ;; ;;  ;        ;     ;;   
;    ;;;;   ;;;;    ;;;   ;;;;          ;;;;   ;;;;    ;     ;;   
;                                                                 
;                                                                 
;                                                                 

(struct state [map players tiles] #:prefab)

(define default-referee-state (state starter-map '() (shuffle ALL-TILES)))

#; {[X Y Z] Map [SoPlayer X] [Listof Y] Z -> [GameState X Y Z]}
(define (create-state gmap player-one others tiles)
  (state gmap (cons player-one others) tiles))

(module+ test
  'default-state
  (render-pub-state default-referee-state))

#; {[X Y Z U V W]
    [[SoPlayer X] -> {SoPlayer U}]
    [Y -> V]
    [Y -> W]
    -> 
    [GameState X Y Z]
    ->
    [GameState U V W]}
(define ((transform-state t-1player t-player* t-tiles) s)
  (match-define [struct* state ([players (cons one players)] [tiles t])] s)
  (struct-copy state s [players (cons (t-1player one) (map t-player* players))] [tiles (t-tiles t)]))

(define (active-player s)
  (first (state-players s)))

#; {[X Y Z] [GameSTate X Y Z] Map -> [Listof Tile]}
(define (active-sop-tiles s)
  (sop-tiles (active-player s)))
  
#; {[X Y Z] [GameSTate X Y Z] {[Listof Tile]} -> Boolean}
(define (active-sop-finished? s [placed '[]])
  (define active (active-sop-tiles s))
  (or (empty? active) (= (length placed) (length active))))

#; {[X Y Z W] [SoPlayer W -> SoPlayer] -> [GameState X Y Z] W ->  [GameState X Y Z]}
(define [(active-sop-update f) s x]
  (match-define (cons one others) (state-players s))
  (struct-copy state s [players (cons (f one x) others)]))

#; {[X Y Z] [GameSTate X Y Z] [Listof Tiles] -> [GameSTate X Y Z]}
(define active-sop-tiles-- (active-sop-update sop-tiles--))

#; {State [Listof Tile] -> State}
(define active-sop-hand (active-sop-update sop-tiles++))

#; {[X Y Z] [GameSTate X Y Z] N -> [GameSTate X Y Z]}
(define active-sop-score++ (active-sop-update sop-score++))

#; {[X Y] [GameSTate X Y] Map -> [GameSTate X Y]}
(define (state-map++ s new-map)
  (struct-copy state s [map new-map]))

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

(module+ examples ;; states and successor states
  (define (create-1player-state map0 active-player-spec #:score (score 0) #:more-sops (more-sops '()))
    (state map0 (cons (apply sop score active-player-spec) more-sops) '[]))
  
  (define +starter-state (create-1player-state starter-map (list (list +starter-tile) "p12")))
  
  (provide +atop-state) ;; local testing only 
  (define +atop-state (create-1player-state map0 (list (list (tile 'circle 'red)) "p12")))

  (provide +starter-tile)

  (define special-tiles (map placement-tile special-placements))
  (define special-state (create-1player-state special-map (list special-tiles "ps")))
  
  (define special-state+green-circle-at--2-2
    (create-1player-state special-map+green-circle-at--2-2 `[[,(tile 'circle 'orange)] ppp]))

  (define bad-map   (legal special-state special-placements))
  (define bad-state (create-1player-state bad-map `[(,(tile 'square 'orange)) ps])))

(module+ examples ;; of basic states 
  #; {[X Y Z] Map Placement -> [GameState X Y Z]}
  (define (create-state-ready-for-placement gmap pp #:score (score 0) #:more-sops (more '()))
    (define tiles* (map placement-tile pp))
    (define extra  (tile 'clover 'green)) ;; to make placement not final.
    (set!   extra  (tile 'circle 'red))   ;; for debugging strategy
    (define spec `[,(append tiles* (list extra)) ,(~a 'player1)])
    (create-1player-state gmap spec #:score score #:more-sops more))

  (define-syntax (def/state stx)
    (syntax-parse stx
      [(_ n)
       #:with name (format-id stx "state~a" (syntax-e #'n))
       #:with nam+ (format-id stx "state~a++" (syntax-e #'n))
       #:with gmap (format-id stx "map~a" (syntax-e #'n))
       #:with map+ (format-id stx "map~a" (+ (syntax-e #'n) 1))
       #:with scor (format-id stx "score~a" (+ (syntax-e #'n) 1))
       #:with plmt (format-id stx "plmt~a" (syntax-e #'n))
       #'(begin
           (provide name nam+)
           
           (set! score* (append score* (list scor)))
           
           (define name (create-state-ready-for-placement gmap plmt))
           (set! state* (append state* (list name)))

           (define nam+ (create-state-ready-for-placement map+ '[] #:score scor))
           (set! state++* (append state++* (list nam+))))]))

  (provide score*)
  (define state* '[])
  (define state++* '[])
  (define score* '[])

  (define score1  10)
  (define score2   5)
  (define score3   8)
  (define score4   9)
  (define score5   8)
  (define score6   5)
  (define score7   5)
  (define score8  12)
  (define score9  10)
  (define score10 (+ 15 [Q-BONUS]))
  (define score11 11)
  
  (def/state 0) 
  (def/state 1) 
  (def/state 2)
  (def/state 3)
  (def/state 4)
  (def/state 5)
  (def/state 6)
  (def/state 7)
  (def/state 8)
  (def/state 9)
  (def/state 10))

;                                                                               
;                                                                               
;             ;                 ;;;                           ;                 
;                                 ;                           ;                 
;    ;;;    ;;;   ; ;;    ;;;;    ;     ;;;           ;;;   ;;;;;   ;;;   ;;;;  
;   ;   ;     ;   ;;  ;  ;;  ;    ;    ;;  ;         ;   ;    ;    ;;  ;  ;; ;; 
;   ;         ;   ;   ;  ;   ;    ;    ;   ;;        ;        ;    ;   ;; ;   ; 
;    ;;;      ;   ;   ;  ;   ;    ;    ;;;;;;         ;;;     ;    ;;;;;; ;   ; 
;       ;     ;   ;   ;  ;   ;    ;    ;                 ;    ;    ;      ;   ; 
;   ;   ;     ;   ;   ;  ;; ;;    ;    ;             ;   ;    ;    ;      ;; ;; 
;    ;;;    ;;;;; ;   ;   ;;;;     ;;   ;;;;          ;;;     ;;;   ;;;;  ;;;;  
;                            ;                                            ;     
;                         ;  ;                                            ;     
;                          ;;                                             ;     

;; apply a single placement to a PubKnowledge state w/o updating the score .. should it? 

#; {PubKnowledge Placement -> PubKnowledge}

(define (apply-action pk 1placement)
  (define new-map (add-tile (state-map pk) 1placement))
  (let* ([pk (active-sop-tiles-- pk (list (placement-tile 1placement)))]
         [pk (state-map++ pk new-map)])
    pk))

(module+ test
  (define state-after-first-special-placement
    (state
       (hash
	 (coordinate -1 1) (tile 'diamond 'green)
	 (coordinate -3 0) (tile 'star 'red)
	 (coordinate -2 0) (tile '8star 'red)
	 (coordinate -4 1) (tile 'clover 'green)
	 (coordinate  0 0) (tile 'circle 'red)
	 (coordinate  0 1) (tile 'circle 'green)
	 (coordinate -4 0) (tile 'clover 'red)
	 (coordinate -1 0) (tile 'diamond 'red))
       `(,(sop 0 `(,(tile 'star 'green)) "ps"))
       '[]))
  
  (check-equal? (apply-action special-state (first special-placements))
                state-after-first-special-placement "place first special on info-pk"))

;                                     
;                                     
;   ;;;                         ;;;   
;     ;                           ;   
;     ;     ;;;    ;;;;  ;;;;     ;   
;     ;    ;;  ;  ;;  ;      ;    ;   
;     ;    ;   ;; ;   ;      ;    ;   
;     ;    ;;;;;; ;   ;   ;;;;    ;   
;     ;    ;      ;   ;  ;   ;    ;   
;     ;    ;      ;; ;;  ;   ;    ;   
;      ;;   ;;;;   ;;;;   ;;;;     ;; 
;                     ;               
;                  ;  ;               
;                   ;;                

#; {[X Y Z] [GameState X Y Z] Placement* -> Option<Map>}
;; are the placements legal according to the rules of Q? If so, produce the new map; otherwise #false
(define (legal gstate placements)
  (define placed-tiles (map placement-tile placements))
  (define coordinate*  (map placement-coordinate placements))
  (and
   (player-owns-tiles (active-player gstate) placed-tiles)
   (or (same-row coordinate*) (same-column coordinate*))
   (all-adjacent-and-fits? (state-map gstate) placements)))

#; {Map Placemennt* -> Option<Map>}
;; create the map that the placements specify and check at each step that a tile can be placed & fits
(define (all-adjacent-and-fits? gmap0 placements)
  (let/ec return 
    (for/fold ([gmap gmap0]) ([p placements])
      (unless (and (adjacent? gmap (placement-coordinate p)) (candidate? (fits gmap p)))
        (return #false))
      (add-tile gmap p))))

;; ---------------------------------------------------------------------------------------------------
(module+ test ;; all-adjacent-and-fits 
  (check-equal? (all-adjacent-and-fits? map1 plmt1) map2)
  (check-true (map? (all-adjacent-and-fits? starter-map +starter-plmt)) "aa 1")
  (define plmnt-2-away (list (placement (coordinate +2 0) (tile 'circle 'red))))
  (check-false (all-adjacent-and-fits? starter-map plmnt-2-away) "aa 2"))

(module+ test ;; legal integration tests 
  (check-false (legal +atop-state place-atop-starter) "b/c can't place tile atop another")
  (check-false (legal bad-state bad-spec-plmnt))
  (check-true (map? (legal special-state special-placements)))
  
  (check-equal? (legal special-state+green-circle-at--2-2 place-orange-circle-at--2-2)
                special-map+green-circle-at--2-2++)
  
  ;; run all scenarios
  (for ([s0 (list state0 state1 state2 state3 state4 state5 state6 state7 state8 state9 state10)]
        [m+ (list map1   map2 map3 map4 map5 map6 map7 map8 map9 map10 map11)]
        [pp (list plmt0 plmt1 plmt2 plmt3 plmt4 plmt5 plmt6 plmt7 plmt8 plmt9 plmt10)]
        [ii (in-naturals)])
    (check-equal? (legal s0 pp) m+)))

;                                     
;                                     
;                                     
;                                     
;    ;;;    ;;;    ;;;    ;;;;   ;;;  
;   ;   ;  ;;  ;  ;; ;;   ;;  ; ;;  ; 
;   ;      ;      ;   ;   ;     ;   ;;
;    ;;;   ;      ;   ;   ;     ;;;;;;
;       ;  ;      ;   ;   ;     ;     
;   ;   ;  ;;     ;; ;;   ;     ;     
;    ;;;    ;;;;   ;;;    ;      ;;;; 
;                                     
;                                     
;                                     

#; {Map Placements -> Natural}

#| Working through an example:

   - a player places 3 tiles x
   - they are all in the same row 

      +-+
      |o|
      +-+
      +-+              +-+
      |o|	       |o|
      +-+              +-+ 
   +-++-++-++-+  +-++-++-+
   |x||x||o||o|	 |o||o||x|
   +-++-++-++-+	 +-++-++-+
   +-+                 +-+
   |o|		       |o|
   +-+		       +-+

   scoring works most easily as follows:
   - determine the line (row or column) to which the tiles were adeded
   - find all segments in this line and award points for those
   - find all segments that are orthogonal to this line where a tile was added 
   - for the last two, add Q bonus if applicable 
|#
(define (score gmap placements #:finishing (finishing-bonus 0))
  (define coord (map placement-coordinate placements))
  (define line  (create-line gmap coord))
  (+ (length placements)                     ;; task 1 
     finishing-bonus                         ;; task 2 
     (score-segments gmap line placements))) ;; tasks 3 and 4        

#; {Map Line [Listof Placement] -> Natural}
;; score the segments on the line plus all segments orthonal to the at addition points 
(define (score-segments gmap line placements)
  (+ (score-same-line-segments gmap line placements) ;; task 3 & 4 
     (score-orthogonal-lines gmap line placements))) ;; task 3 & 4 

;; ---------------------------------------------------------------------------------------------------
#; {Map Line [Listof Placement] -> Natural}
;; lengths for all segments of the placement line that contain a new placement,
(define (score-same-line-segments gmap line placements)
  (define segment* (if (row? line) (all-row-segments gmap line) (all-column-segments gmap line)))
  (for/sum ([1segment segment*] #:when (contains-1-placement 1segment placements))
    (score-1-segment (map placement-tile 1segment))))

#; {Segment [Listof Placement] -> [Option Segment]}
;; lengt of segment if it contain one placement; 0 otherwise 
(define (contains-1-placement 1segment placements)
  (for/first ([p 1segment] #:when (member p placements))
    1segment))

;; ---------------------------------------------------------------------------------------------------
#; {Map [Listof Placements] -> Natural}
;; lengths for all lines orthogonal to the placement line that contain one new placement,
(define (score-orthogonal-lines gmap line placements)
  (define coord* (map placement-coordinate placements))
  (define walk   (if (row? line) walk-column-orthogonally walk-row-orthogonally))
  (for/sum ([cord coord*])
    (score-1-segment (walk gmap cord))))

;; ---------------------------------------------------------------------------------------------------
#; {[Listof Tile] -> Natural}
(define (score-1-segment s)
  (+ (length s) (q-bonus s)))

;; ---------------------------------------------------------------------------------------------------
#; {[Listof Tile] -> Natural}
(define (q-bonus tile*) ;; task 4 
  (if (is-q? tile*) [Q-BONUS] 0))

#; {[Listof Tile] -> Boolean}
(define (is-q? tile*)
  (or (all-colors? tile*) (all-shapes? tile*)))

(module+ test ;; for just Q bonus
  (check-equal? (q-bonus exactly-all-colors) [Q-BONUS] "exactly all colors")
  (check-equal? (q-bonus duplicate-red-all-colors) 0 "an extra red, but all colors")
  (check-equal? (q-bonus exactly-all-shapes) [Q-BONUS] "exactly all shapes")
  (check-equal? (q-bonus duplicate-star-all-shapes) 0 "an extra star, but all shapes"))

;                                                                               
;                                                                               
;                                                                               
;                                                                               
;    ;;;    ;;;    ;;;   ; ;;                  ;;;    ;;;    ;;;    ;;;;   ;;;  
;   ;   ;  ;;  ;  ;;  ;  ;;  ;                ;   ;  ;;  ;  ;; ;;   ;;  ; ;;  ; 
;   ;      ;      ;   ;; ;   ;                ;      ;      ;   ;   ;     ;   ;;
;    ;;;   ;      ;;;;;; ;   ;                 ;;;   ;      ;   ;   ;     ;;;;;;
;       ;  ;      ;      ;   ;                    ;  ;      ;   ;   ;     ;     
;   ;   ;  ;;     ;      ;   ;   ;;           ;   ;  ;;     ;; ;;   ;     ;     
;    ;;;    ;;;;   ;;;;  ;   ;   ;;            ;;;    ;;;;   ;;;    ;      ;;;; 
;                                                                               
;                                                                               
;                                                                               

(module+ examples
  (define-syntax-rule (score-scenario kind game-map placement expected msg)
    (set! kind (append kind (list [list game-map placement expected msg]))))

  (define ForStudents/ '[])
  (score-scenario ForStudents/ map1 plmt0 (list-ref score* 0) "10")
  (score-scenario ForStudents/ map2 plmt1 (list-ref score* 1) "21")
  (score-scenario ForStudents/ map4 plmt3 (list-ref score* 3) "43")
  
  (define Tests/ '[])
  (score-scenario Tests/ map3 plmt2 (list-ref score* 2) "32")
  (score-scenario Tests/ map8 plmt7 (list-ref score* 7) "87")
  (score-scenario Tests/ map9 plmt8 (list-ref score* 8) "98")
  (score-scenario Tests/ map10 plmt9 (list-ref score* 9) "109")
  (score-scenario Tests/ map11 plmt10 (list-ref score* 10) "1110")

  ;; -----------------------------------------------------------------------------
  ;; bug
  (provide Ilyas/)
  (require (submod Qwirkle/Common/map json))
  (require (submod Qwirkle/Common/placement json))
  (define Ilyas/ '[])
  (score-scenario Ilyas/
                  (jsexpr->map 
                   '[(0 (0 #hasheq((color . "red")    (shape . "star"))))
                     (1 (0 #hasheq((color . "blue")   (shape . "star"))))
                     (2 (0 #hasheq((color . "green")  (shape . "star"))))
                     (3 (0 #hasheq((color . "yellow") (shape . "star"))))
                     (4 (0 #hasheq((color . "purple") (shape . "star"))))
                     (5 (0 #hasheq((color . "orange") (shape . "star"))))
                     (6 (0 #hasheq((color . "orange") (shape . "square"))))])
                  (jsexpr->placements 
                   '[#hasheq((1tile . #hasheq((color . "orange") (shape . "square")))
                             (coordinate . #hasheq((column . 0) (row . 6))))])
                  8
                  "Ilyas's witness to my failure")
  ;; -----------------------------------------------------------------------------

  (define Local '[])
  ;; not a run of like-tiles or colors and not a Q !! 
  (score-scenario Local special-map+green-circle-at--2-2++ place-orange-circle-at--2-2 7 "not run")
  
  (score-scenario Local map10 plmt9 (list-ref score* 9) "Q bonus missing")
  (score-scenario Local (legal special-state special-placements) special-placements 10 "2 segments"))
              

(module+ test ;; scoring tests
  (for ([m+ (list map1 map2 map3 map4 map5 map6 map7 map8 map9 map10 map11)]
        [pp (list plmt0 plmt1 plmt2 plmt3 plmt4 plmt5 plmt6 plmt7 plmt8 plmt9 plmt10)]
        [sc score*]
        [ii (in-naturals)])
    (check-equal? (score m+ pp) sc (~a "scoring map " (+ ii 1))))

  (define (check-score l)
    (match-define [list game-map placement expected msg] l)
    (check-equal? (score game-map placement) expected msg))

  (for-each check-score Local)
  (for-each check-score ForStudents/)
  (for-each check-score Tests/)
  (for-each check-score Ilyas/))

(module+ test
  (define (show-scenario s)
    (match-define [list game-map placement expected msg] s)
    (list (render-map game-map) placement))

  (map show-scenario ForStudents/))

;                                            
;                            ;               
;                            ;               
;                            ;               
;    ;;;;   ;;;   ; ;;    ;;;;   ;;;    ;;;; 
;    ;;  ; ;;  ;  ;;  ;  ;; ;;  ;;  ;   ;;  ;
;    ;     ;   ;; ;   ;  ;   ;  ;   ;;  ;    
;    ;     ;;;;;; ;   ;  ;   ;  ;;;;;;  ;    
;    ;     ;      ;   ;  ;   ;  ;       ;    
;    ;     ;      ;   ;  ;; ;;  ;       ;    
;    ;      ;;;;  ;   ;   ;;;;   ;;;;   ;    
;                                            
;                                            
;                                            

(define ((render-ref-state/g render-sop render-tiles) gs)
  (match gs
    {[state gmap (cons one [list sop ...]) tiles]
     (define gmap-image  (render-map gmap))
     (define sop-images  (render-sop* one sop render-sop))
     (2:above/align
      'left 
      (2:beside/align 'top gmap-image hblank sop-images)
      vblank
      (2:beside (2:text "tiles left: " 22 'black) (render-tiles tiles)))}
    {[state gmap '() tiles]
     (define gmap-image  (render-map gmap))
     (define sop-images  2:empty-image)
     (2:above/align
      'left 
      (2:beside/align 'top gmap-image hblank sop-images)
      vblank
      (2:beside (2:text "tiles left: " 22 'black) (render-tiles tiles)))}))

(define render-pub-state
  (render-ref-state/g
   render-sop
   (λ (t*)
     (define how-many (min (length t*) 12))
     (define dots-suf (if (> (length t*) 12) (2:text "   ..." 22 'black) 2:empty-image))
     (2:beside (apply 2:beside (map render-tile (take t* how-many))) dots-suf))))

(define hblank (2:rectangle 10 1 'solid 'white))
(define vblank (2:rectangle 1 10 'solid 'white))

;                              
;      ;                       
;                              
;                              
;    ;;;    ;;;    ;;;   ; ;;  
;      ;   ;   ;  ;; ;;  ;;  ; 
;      ;   ;      ;   ;  ;   ; 
;      ;    ;;;   ;   ;  ;   ; 
;      ;       ;  ;   ;  ;   ; 
;      ;   ;   ;  ;; ;;  ;   ; 
;      ;    ;;;    ;;;   ;   ; 
;      ;                       
;      ;                       
;    ;;                        

(module+ json ;; tested via ref-state in Referee/

  (define MAP 'map)
  (define PLAYERS 'players)
  
  #; {[X Y Z U W]
      [GameState X Y Z]
      [[Listof Y] -> [Listof W]]
      [Z -> U]
      ->
      {MAP : JMap, PLAYERS : [Cons JPlayer [Listof W]], TILES : U}}
  (define ((state->jsexpr/g players->jsexpr tiles->jsexpr) s)
    (match s
      {[state gmap (cons one players) tiles]
       (define jactive  (1player->jsexpr one))
       (define jplayers (players->jsexpr players))
       (hasheq MAP     (map->jsexpr gmap)
               PLAYERS (cons jactive jplayers)
               TILES   (tiles->jsexpr tiles))}
      {[state gmap '[] tiles]
       (hasheq MAP     (map->jsexpr gmap)
               PLAYERS '()
               TILES   (tiles->jsexpr tiles))}))
       
  #; {[X Y Z]
      [JSexpr -> Option<Z>]
      [JSexpr -> Option<Cons [SoPlayer X] [Listof Y]>]
      -> JSexpr
      -> Option<[GameState X Y Z]>}
  (define ((jsexpr->state/g jsexpr->players jsexpr->tiles) j)
    (def/jsexpr-> state/general
      #:object {[MAP     map     (? hash? gmap)]
                [PLAYERS players (cons (? sop? first) p)]
                [TILES   tiles   (? identity  t)]} ;; not #false 
      (state gmap (cons first p) t))
    (jsexpr->state/general j)))
