#lang racket

;; a data representation of the referee's knowledge about the game
;; ---------------------------------------------------------------------------------------------------

(provide
 #; {type [RefState Y] = [GameState Y [SoPlayer Y]] [Listof Tile]}
 ;; the referee knows the state of every player and the sequence of tiles it is handing out 

 (contract-out
  [create-ref-state
   (->* (map? [listof [list/c [listof tile?] any/c]]) (#:tiles0 (listof tile?)) state?)]
  
  [ref-state-to-info-state
   (-> state? state?)]
  
  [complete-turn
   ;; a possibly new map, points, the placed tiles, the tiles handed out yield a new game state
   #; {complete-turn s gmap delta old-tiles new-tiles}
   ;; PROTICOL assume that for some placements
   #; (legal s placements)
   ;; yields gmap
   #; (score gmap placements)
   ;; yields delta-score between `(state-map s)` and `gmap`
   ;; noew call complete-turn -- hand out the tiles fails, remove player if this fails
   (-> state? map? natural? (listof tile?) (values (listof tile?) state?))]

  ;; this is ref-state specific 
  [state-rotate        (-> state? state?)]
  
  [render-ref-state (-> state? 2:image?)]))

(module+ examples
  (provide
   ref-starter-state
   ;; yields 
   ref-starter-state-handout

   info-starter-state
   info-starter-state-handout
   info-+ref-starter-state
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
  
;; ---------------------------------------------------------------------------------------------------
(require Qwirkle/Common/map)
(require Qwirkle/Common/tiles)
(require Qwirkle/Common/state-of-player)
(require SwDev/Lib/list)
(require (prefix-in 2: 2htdp/image))

(require Qwirkle/Common/game-state)

(module+ examples
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
  (require (submod Qwirkle/Common/map examples))
  (require (submod Qwirkle/Common/tiles examples))
  (require rackunit))

;; ---------------------------------------------------------------------------------------------------
#; {[Y] Map [Listof [List [Listof Tile] Y]] -> [RefState Y]}
(define (create-ref-state gmap payload #:tiles0 (tiles0 '[]))
  (match-define (cons first others) (map (λ (p) (apply sop 0 p)) payload))
  (create-state gmap first others tiles0))

#; {[Y] [RefState Y] -> PubKnowledge}
(define ref-state-to-info-state (transform-state sop-special sop-score length))

;; ---------------------------------------------------------------------------------------------------
#; {[Y] [RefStatet Y] Map Natural [Listof Tile] [Listof Tile] -> (values [Listof Tile] [RefState Y])}

(define (complete-turn s new-map delta-score tiles-placed)
  (let*-values ([(s) (state-tiles-- s tiles-placed)]
                [(s) (state-score++ s delta-score)]
                [(s) (state-map++ s new-map)])
    (state-handouts s (length tiles-placed))))

#; {[X] [RefState X] [Listof Tiles] -> (values [Listof Tile] [RefState X])}
;; produce the list of tiles to be handed to the player and the remainder 
(define (state-handouts s n)
  (define tile* (state-tiles s))
  (define k (length tile*))
  (define-values [handouts tiles++]
    (if (< n k)
        (values (take tile* n) (drop tile* n))
        (values tile*          '[])))
  (match-define [cons first others] (state-players s))
  (values handouts (create-state (state-map s) first others tiles++)))

(module+ examples
  (provide handouts)
  (define handouts (make-list 6 #s(tile diamond green)))
  (define starter-players [list [list starter-tile* 'player1] [list qwirkle-tile* 'player2]])
  (define ref-starter-state (create-ref-state starter-map starter-players #:tiles0 handouts))
  (define starter-players-handout [list [list '() 'player1] [list qwirkle-tile* 'player2]])
  (define ref-starter-state-handout (create-ref-state starter-map starter-players-handout)))

(module+ test
  (check-equal?
   (let-values ([(h s) (complete-turn ref-starter-state starter-map 0 starter-tile*)]) s)
   ref-starter-state-handout)
  (check-equal?
   (let-values ([(h s) (complete-turn ref-starter-state starter-map 0 starter-tile*)]) h)
   handouts))

(module+ examples ;; states and successor states
  (define info-starter-state (ref-state-to-info-state ref-starter-state))
  (define info-starter-state-handout (ref-state-to-info-state ref-starter-state-handout))
  (define info-+ref-starter-state (ref-state-to-info-state +ref-starter-state))
  (define info-special-state (ref-state-to-info-state special-state))
  (define info-bad-state (ref-state-to-info-state bad-state)))

;; ---------------------------------------------------------------------------------------------------
#; {[X Y] [GameState X Y] -> [GameState X Y]}
(define (state-rotate s)
  (match-define [cons first others] (list-rotate+ (state-players s)))
  (create-state (state-map s) first others (state-tiles s)))

;; ---------------------------------------------------------------------------------------------------
;; render states 

#; {[Y] [RefState Y] -> Image}
(define render-ref-state (render-ref-state/g render-sop))

#; {PubKnowledge -> Image}
(define render-info-state (render-ref-state/g (λ (s) (2:text (~a s) 20 'black))))

(module+ test
  'infor-starter-state
  (render-info-state info-starter-state)
  'ref-starte-state
  (render-ref-state ref-starter-state))

;; ---------------------------------------------------------------------------------------------------
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
