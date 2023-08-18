#lang racket

;; a data representation of the referee's knowledge about the game
;; ---------------------------------------------------------------------------------------------------

(provide
 ;; from Qwirkle/Common/game-state
 state?
 active-sop-score++   
 active-sop-tiles--   
 active-sop-finished? 
 active-sop-hand      
 active-sop-tiles
 #;
 (all-from-out Qwirkle/Common/game-state))

(provide
 #; {type [RefState Y] = [GameState Y [SoPlayer Y]] [Listof Tile]}
 ;; the referee knows the state of every player and the sequence of tiles it is handing out 

 (contract-out
  [create-ref-state
   (->i ([gmap map?] [player-specs [listof [list/c [listof tile?] any/c]]])
        (#:tiles0 (tiles (listof tile?)))
        #:pre/name (player-specs) "distinct names" (distinct? (map second player-specs))
        (r state?))]

  [set-ref-state-players
   ;; sets the external players in order 
   (->i ([s state?] [lop (listof player/c)])
        #:pre/name (s lop) "same number of player reps and player obj"
        (= (length (state-players s)) (length lop))
        (r state?))]

  [state-handouts
   #; (state-handouts s n) ; produce the tiles to be handed to the actual player and a revised state
   ;; -- if n is #false, use the tiles in possession of the player representation
   ;; -- otherwise, ; takes away at most n tiles from the current pile
   (-> state? (or/c #false natural?) (values (listof tile?) state?))]
  
  [state-take-back
   #; (state-take-back s lot) ; adds lot to the front of s's tiles 
   (-> state? (listof tile?) state?)]

  [state-kick
   #; (state-kick s)       ; kick active player out 
   #; (state-kick s #true) ; move tiles from active player back to pile 
   (->* (state?) (#:from-active boolean?) (or/c state? #false))]
  
  [ref-state-to-info-state
   (-> state? state?)]

  [determine-winners
   ;; determine winners and losers in the current state, if any 
   (-> (or/c #false state?) (list/c (listof sop?) (listof sop?)))]

  [fold-players
   (->i ([f (-> sop? state? (listof sop?) (values (or/c #false state?) (listof sop?)))]
         [s state?]
         [l (listof sop?)])
        (#:return (return (-> state? (listof sop?) (listof any/c))))
        ;; inexpressible contract: return whatever `f` or `return` return
        ;; and that can differ from call to call 
        (r (listof any/c)))]
  
  ;; this is ref-state specific 
  [state-rotate        (-> state? state?)]
  
  [render-ref-state (-> state? 2:image?)]
  [render-info-state (-> state? 2:image?)]))

(module+ examples
  (provide
   ref-starter-state
   ;; yields 
   ref-starter-state-handout

   info-starter-state
   info-all-tiles
   info-starter-state-handout
   info-+starter-state
   info-special-state
   info-bad-state))

(module+ json
  (provide
   (contract-out
    #; {type JState  = { MAP : JMap, PLAYERS : [Listof JPlayer], TILES : [Listof Tile]}}
    [state->jsexpr (-> state? j:jsexpr?)]
    [jsexpr->state (->* (j:jsexpr?) (#:names (listof any/c)) (or/c state? #false))]

    #; {type JPK = { MAP : JMap, PLAYERS : [Cons JPlayer [Listof N]], TILES : Natural}}
    [pk->jsexpr    (-> state? j:jsexpr?)]
    [jsexpr->pk    (->* (j:jsexpr?) (#:name any/c) (or/c state? #false))])))

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

(require Qwirkle/Common/map)
(require Qwirkle/Common/tiles)
(require Qwirkle/Common/game-state)
(require Qwirkle/Common/state-of-player)
(require Qwirkle/Common/player-interface)
(require SwDev/Lib/list)
(require SwDev/Contracts/unique)
(require (prefix-in 2: 2htdp/image))

(module+ examples
  (require (submod ".."))
  (require (submod Qwirkle/Common/game-state examples))
  (require (submod Qwirkle/Common/map examples))
  (require (submod Qwirkle/Common/tiles examples)))

(module+ json
  (require (submod Qwirkle/Common/game-state json))
  (require (submod Qwirkle/Common/state-of-player json))
  (require (submod Qwirkle/Common/tiles json))
  (require Qwirkle/Lib/parse-json)
  (require Qwirkle/Lib/json)
  (require (prefix-in j: json)))

(module+ test
  (require (submod ".."))
  (require (submod ".." examples))
  (require (submod ".." json))
  (require (submod Qwirkle/Common/game-state examples))
  (require (submod Qwirkle/Common/map examples))
  (require (submod Qwirkle/Common/placement examples))
  (require (submod Qwirkle/Common/tiles examples))
  (require rackunit))

;                                            
;                                            
;                                 ;          
;                                 ;          
;    ;;;    ;;;;   ;;;   ;;;;   ;;;;;   ;;;  
;   ;;  ;   ;;  ; ;;  ;      ;    ;    ;;  ; 
;   ;       ;     ;   ;;     ;    ;    ;   ;;
;   ;       ;     ;;;;;;  ;;;;    ;    ;;;;;;
;   ;       ;     ;      ;   ;    ;    ;     
;   ;;      ;     ;      ;   ;    ;    ;     
;    ;;;;   ;      ;;;;   ;;;;    ;;;   ;;;; 
;                                            
;                                            
;                                            

#; {[Y] Map [Listof [List [Listof Tile] Y]] -> [RefState Y]}
(define (create-ref-state gmap payload #:tiles0 (tiles0 '[]))
  (match-define (cons first others) (map (λ (p) (apply sop 0 p)) payload))
  (create-state gmap first others tiles0))

#; {[Y] [RefState Y] [Listof SoPlayer] -> [RefState Y]}
(define (set-ref-state-players state0 players)
  (match-define [cons first others] (map sop-set-player (state-players state0) players))
  (create-state (state-map state0) first others (state-tiles state0)))

#; {[Y] [RefState Y] -> PubKnowledge}
(define ref-state-to-info-state (transform-state sop-special sop-score length))

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

(module+ examples
  (provide handouts)
  (define handouts (make-list 6 #s(tile diamond green)))
  (provide starter-players)
  (define starter-players [list [list starter-tile* 'player1] [list qwirkle-tile* 'player2]])
  (define ref-starter-state (create-ref-state starter-map starter-players #:tiles0 handouts))
  (define starter-players-handout [list [list '() 'player1] [list qwirkle-tile* 'player2]])
  (define ref-starter-state-handout (create-ref-state starter-map starter-players-handout)))

(module+ examples ;; states and successor states
  (define info-starter-state (ref-state-to-info-state ref-starter-state))
  (define info-all-tiles (active-sop-hand info-starter-state ALL-TILE-COLOR-COMBOS))
  (define info-starter-state-handout (ref-state-to-info-state ref-starter-state-handout))
  (define info-+starter-state (ref-state-to-info-state +starter-state))
  (define info-special-state (ref-state-to-info-state special-state))
  (define info-bad-state (ref-state-to-info-state bad-state)))

(module+ examples
  (provide state1-with) 
  (define state1-with (create-ref-state map0 `[(,tiles1 player1) ([,(tile 'square 'green)] extra)])))

;                              
;      ;;                    ; 
;     ;           ;;;        ; 
;     ;             ;        ; 
;   ;;;;;   ;;;     ;     ;;;; 
;     ;    ;; ;;    ;    ;; ;; 
;     ;    ;   ;    ;    ;   ; 
;     ;    ;   ;    ;    ;   ; 
;     ;    ;   ;    ;    ;   ; 
;     ;    ;; ;;    ;    ;; ;; 
;     ;     ;;;      ;;   ;;;; 
;                              
;                              
;                              

;; run `f` over the current players in the given state
;; give `f` the state, the list of dropped out players
;; and it returns the new state and the new list of dropped out players
;; #:return allows the insertion of additional values into the return values 
;; fold-players

(define (fold-players f s0 ep-out0 #:return (r list))
  (define active* (state-players s0))
  (for/fold ([s s0] [ep-out ep-out0] #:result (r s ep-out)) ([ap active*] #:when s)
    (f ap s ep-out)))

;                                                          
;                                                          
;                               ;;;             ;          
;                                 ;             ;          
;    ;;;    ;;;  ;;;;;;  ;;;;     ;     ;;;   ;;;;;   ;;;  
;   ;;  ;  ;; ;; ;  ;  ; ;; ;;    ;    ;;  ;    ;    ;;  ; 
;   ;      ;   ; ;  ;  ; ;   ;    ;    ;   ;;   ;    ;   ;;
;   ;      ;   ; ;  ;  ; ;   ;    ;    ;;;;;;   ;    ;;;;;;
;   ;      ;   ; ;  ;  ; ;   ;    ;    ;        ;    ;     
;   ;;     ;; ;; ;  ;  ; ;; ;;    ;    ;        ;    ;     
;    ;;;;   ;;;  ;  ;  ; ;;;;      ;;   ;;;;    ;;;   ;;;; 
;                        ;                                 
;                        ;                                 
;                        ;                                 

(provide
 ;; STNTAX
 #; (legal-placement state successor-state tiles-placed)
 ;; checks whether the action is a set of placements and whether they are legal
 ;; if so, it produces the successor state, which removes the placed tiles from the active player
 ;; and binds them to `tiles-placed`
 legal-placement

 ;; SYNYAX
 #; (legal-re-placement s s+ tiles-to-replaced)
 ;; checks whether the action is a REPLACEMENT request and whether it is leal
 ;; if so, it takes all tiles from the active player in `s` (to `s+`
 ;; and binds them to `tiles-to-replaced`
 legal-re-placement)

(define-match-expander legal-re-placement
  (λ (stx)
    (syntax-case stx ()
      [(_ s s+ n)
       #'(and (? (curry equal? REPLACEMENT))
              (app (legal-replace s) (? state? s+))
              (app (λ (x) (sop-tiles (first (state-players s)))) [list (? tile? n) (... ...)]))])))

;; ---------------------------------------------------------------------------------------------------
#; {State -> REPLACEMENT -> [Option State]}
(define [(legal-replace s) _]
  (define active (first (state-players s)))
  (define tiles  (sop-tiles active))
  (and (>= (length (state-tiles s)) (length tiles))
       (active-sop-tiles-- s tiles)))

;; ---------------------------------------------------------------------------------------------------
(define-match-expander legal-placement 
  (λ (stx)
    (syntax-case stx ()
      [(_ s s+ n)
       #'(and (list (? placement?) (... ...))
              (app (λ (x) (complete-placements s x)) (? state? s+))
              (app (λ (x) (sop-tiles (first (state-players s)))) [list (? tile? n) (... ...)]))])))

#; {[Y] [RefState Y] [Listof Placememnt] -> [Option [RefState Y]]}
(define (complete-placements s placements)
  (define gmap (legal s placements))
  (cond
    [(false? gmap) #false]
    [else 
     (define tiles-placed (map placement-tile placements))
     (define finished?    (active-sop-finished? s tiles-placed))
     (define delta-score  (score gmap placements #:finishing (if finished? FINISH-BONUS 0)))
     (let*-values ([(s) (active-sop-tiles-- s tiles-placed)]
                   [(s) (active-sop-score++ s delta-score)]
                   [(s) (state-map++ s gmap)])
       s)]))

(define FINISH-BONUS 6)

(module+ test
  (check-false (complete-placements ref-starter-state +starter-plmt))

  ;; check all scenarios 
  (for ([ss state*] [s+ state++*] [pp plmt*] [ii (in-naturals)])
    (check-equal? (complete-placements ss pp) s+ (~a "step" ii))))


;                                                          
;   ;                        ;                             
;   ;                        ;                  ;          
;   ;                        ;                  ;          
;   ; ;;   ;;;;   ; ;;    ;;;;   ;;;   ;   ;  ;;;;;   ;;;  
;   ;;  ;      ;  ;;  ;  ;; ;;  ;; ;;  ;   ;    ;    ;   ; 
;   ;   ;      ;  ;   ;  ;   ;  ;   ;  ;   ;    ;    ;     
;   ;   ;   ;;;;  ;   ;  ;   ;  ;   ;  ;   ;    ;     ;;;  
;   ;   ;  ;   ;  ;   ;  ;   ;  ;   ;  ;   ;    ;        ; 
;   ;   ;  ;   ;  ;   ;  ;; ;;  ;; ;;  ;   ;    ;    ;   ; 
;   ;   ;   ;;;;  ;   ;   ;;;;   ;;;    ;;;;    ;;;   ;;;  
;                                                          
;                                                          
;                                                          

#; {[X] [RefState X] [Option Narural] -> (values [Listof Tile] [RefState X])}
(define (state-handouts s n)
  (cond
    [(false? n)
     (match-define [cons first others] (state-players s))
     (values (sop-tiles first) s)]
    [else 
     (define tile* (state-tiles s))
     (define k (length tile*))
     (define-values [handouts tiles++]
       (if (< n k)
           (values (take tile* n) (drop tile* n))
           (values tile*          '[])))
     (match-define [cons first others] (state-players s))
     (values handouts (create-state (state-map s) first others tiles++))]))

;; ---------------------------------------------------------------------------------------------------
#; {[X] [RefState X] [Listof Tile] -> [RefState X]}
(define (state-take-back s lot)
  (match-define [cons first others] (state-players s))
  (create-state (state-map s) first others (append lot (state-tiles s))))

;                                                                                             
;   ;                    ;                                                              ;     
;   ;         ;          ;               ;;;                          ;     ;           ;     
;   ;                    ;              ;                                   ;           ;     
;   ;  ;    ;;;    ;;;   ;  ;           ;             ;;;  ;     ;  ;;;   ;;;;;   ;;;   ; ;;  
;   ;  ;      ;   ;;  ;  ;  ;           ;;           ;   ; ;     ;    ;     ;    ;;  ;  ;;  ; 
;   ; ;       ;   ;      ; ;            ;;           ;      ; ; ;     ;     ;    ;      ;   ; 
;   ;;;       ;   ;      ;;;           ;  ; ;         ;;;   ; ; ;     ;     ;    ;      ;   ; 
;   ; ;       ;   ;      ; ;           ;  ;;;            ;  ;; ;;     ;     ;    ;      ;   ; 
;   ;  ;      ;   ;;     ;  ;          ;;  ;         ;   ;  ;; ;;     ;     ;    ;;     ;   ; 
;   ;   ;   ;;;;;  ;;;;  ;   ;          ;;; ;         ;;;    ; ;    ;;;;;   ;;;   ;;;;  ;   ; 
;                                                                                             
;                                                                                             
;                                                                                             

#; {[X Y] [GameState X Y] -> [GameState X Y]}
(define (state-rotate s)
  (match-define [cons first others] (list-rotate+ (state-players s)))
  (create-state (state-map s) first others (state-tiles s)))

(module+ test
  (define rotated (create-ref-state starter-map (list-rotate+ starter-players) #:tiles0 handouts))
  (check-equal? (state-rotate ref-starter-state) rotated))

;; ---------------------------------------------------------------------------------------------------

#; {State -> [Optiona State]}
(define (state-kick s #:from-active (fa #false))
  (match-define [cons one others] (state-players s))
  (cond
    [(empty? others) #false]
    [else
     (define tiles++ (append (if fa (sop-tiles one) '[]) (state-tiles s)))
     (create-state (state-map s) (first others) (rest others) tiles++)]))

(module+ test
  (define kicked (create-ref-state starter-map (rest starter-players) #:tiles0 handouts))
  (check-equal? (state-kick ref-starter-state) kicked))


;                                                   
;                                                   
;             ;                                     
;                                                   
;  ;     ;  ;;;   ; ;;   ; ;;    ;;;    ;;;;   ;;;  
;  ;     ;    ;   ;;  ;  ;;  ;  ;;  ;   ;;  ; ;   ; 
;   ; ; ;     ;   ;   ;  ;   ;  ;   ;;  ;     ;     
;   ; ; ;     ;   ;   ;  ;   ;  ;;;;;;  ;      ;;;  
;   ;; ;;     ;   ;   ;  ;   ;  ;       ;         ; 
;   ;; ;;     ;   ;   ;  ;   ;  ;       ;     ;   ; 
;    ; ;    ;;;;; ;   ;  ;   ;   ;;;;   ;      ;;;  
;                                                   
;                                                   
;                                                   

#; {[Option [RefState Player]] -> [List [Listof SoPlayer] [Listof SoPlayer]]}
(define (determine-winners s+)
  (cond
    [(false? s+) '[[] []]]
    [else 
     (define players (state-players s+))
     (define highest (sop-score (argmax sop-score players)))
     (list 
      (filter (λ (p) (= (sop-score p) highest)) players)
      (filter (λ (p) (not (= (sop-score p) highest))) players))]))

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

#; {[Y] [RefState Y] -> Image}
(define render-ref-state
  (render-ref-state/g
   render-sop
   (λ (tiles)
     ;; MAGIC ##### 
     (define n (min (length tiles) 6))
     (cond
       [(zero? n) 2:empty-image]
       [(= n 1)   (render-tile (first tiles))]
       [else (apply 2:beside (map render-tile (take tiles n)))]))))

#; {PubKnowledge -> Image}
(define render-info-state
  (render-ref-state/g (λ (s) (2:text (~a s) 20 'black)) (λ (t) (2:text (~a t) 20 'black))))

(module+ test
  'infor-starter-state
  (render-info-state info-starter-state)
  'ref-starte-state
  (render-ref-state ref-starter-state)

  '+starter-state
  (render-ref-state +starter-state)

  'special-state
  (render-ref-state special-state)
  
  'special-state+green-circle-at--2-2
  (render-ref-state special-state+green-circle-at--2-2)
  
  'bad-state
  (render-ref-state bad-state)

  'info-all-tiles 
  (render-info-state info-all-tiles))

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

(module+ json
  (define state->jsexpr (state->jsexpr/g players->jsexpr tiles->jsexpr))
  (define pk->jsexpr    (state->jsexpr/g natural->jsexpr natural->jsexpr))

  (define (jsexpr->state j #:names (names #false))
    (define (j->1 j) ;; kludge for easy testing to get name back 
      (define name (begin0 (first names) (set! names (rest names))))
      (jsexpr->1player j #:name name))
    (def/jsexpr-> players #:array [(list (app j->1 (? sop? x)) ...) x])
    (define jsexpr->state (jsexpr->state/g jsexpr->players jsexpr->tiles))
    (jsexpr->state j))
  
  (define (jsexpr->pk j #:name (name "a name"))
    (define (j->1 j) (jsexpr->1player j #:name name))
    (def/jsexpr-> players #:array [(cons (app j->1 (? sop? f)) `(,(? natural? n) ...)) (cons f n)])
    (define jsexpr->pk (jsexpr->state/g jsexpr->players jsexpr->natural))
    (jsexpr->pk j)))

(module+ test
  (check-equal? (jsexpr->pk (pk->jsexpr info-starter-state) #:name 'player1) info-starter-state)

  (define P '[player1 player2]) ;; artificial .. for easy testing 
  (check-equal? (jsexpr->state (state->jsexpr ref-starter-state) #:names P) ref-starter-state))
