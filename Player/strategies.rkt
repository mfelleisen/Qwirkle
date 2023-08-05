#lang racket

;; simple strategies for picking a single placement and iterating those
;; ---------------------------------------------------------------------------------------------------

(provide
 #; {type Strategy = PubKnowledge -> (U Placement 'pass 'replace)}
 ;; if there are no tiles and the referee has enough for a 'swap', ask for a 'replacement
 ;; otherwise 'pass
 (contract-out
  [iterate-strategy
   ;; iterate the strategy as far as possible to obtain a sqeuence of placements or PASS/REPALCEMENT
   (-> (-> state? action?) state? action*?)]
  (dag-strategy
   ;; strategy 1: dumb and greedy
   ;; choose "smallest tile" that has candidates; break tie among candidates via coodinate<
   (-> state? action?))
  (ldasg-strategy
   ;; strategy 2: a bit more sophisticate and still greedy
   ;; choose "smallest tile" that has candidates; pick most-constrained candidates;
   ;; break tie among candidates via coodinate<   
   (-> state? action?))
  ))

;; Tiles are lexically ordered as follows:
;; 'star '8star 'square 'circle 'clover 'diamond
;; 'red 'green 'blue 'yellow 'orange 'purple

(module+ examples
  (provide
   the-special-place
   ref-place
   constrained-special))

;; ---------------------------------------------------------------------------------------------------
(require Qwirkle/Common/game-state)
(require Qwirkle/Common/map)
(require Qwirkle/Common/coordinates)
(require Qwirkle/Common/placement)
(require Qwirkle/Common/tiles)

(module+ test
  (require (submod ".."))
  (require (submod ".." examples))
           
  (require (submod Qwirkle/Common/game-state examples))
  (require rackunit))

;; ---------------------------------------------------------------------------------------------------
#; {-> Strategy}
(define ((make-strategy smallest) pk)
  (define gmap      (state-map pk))
  (define my-tiles  (sort (sop-tiles (first (state-players pk))) tile<))
  (define remaining (state-tiles pk))
  (define exists?   (is-there-a-placement gmap my-tiles))
  (cond
    [exists? (apply smallest exists?)]
    [(> (length my-tiles) remaining) PASS]
    [else REPLACEMENT]))

(module+ examples
  (define constrained-special #s(placement #s(coordinate -3 1) #s(tile star green)))
  (define the-special-place #s(placement #s(coordinate -5 1) #s(tile star green)))
  (define ref-place #s(placement #s(coordinate -1 0) #s(tile circle red))))

;; ---------------------------------------------------------------------------------------------------
;; dumb and greedy strategy: pick "smallest" coordinate of all feasible candidates for the first tile 

#; {Tile [NonMTSet Candidate] -> Placement}
(define (smallest t cs)
  (define as-list (set->list cs))
  (define sorted  (sort as-list top-down-left-to-right-order< #:key candidate-place))
  (placement (candidate-place (first sorted)) t))

#; Strategy
(define dag-strategy (make-strategy smallest))

(module+ test
  (check-equal? (dag-strategy info-special-state) the-special-place)
  (check-equal? (dag-strategy info-+ref-starter-state) ref-place)
  (check-equal? (dag-strategy info-starter-state) REPLACEMENT)
  (check-equal? (dag-strategy info-bad-state) PASS))

;; ---------------------------------------------------------------------------------------------------
;; less dumb & still greedy strategy: pick "most constrained" coordinate 

#; {Tile [NonMTSet Candidate] -> Placement}
(define (most-constrained-placement t cs)
  (define with#  (for/list ([c (in-set cs)]) (list (candidate-constraints# c) c)))
  (define large* (pick-largest with#))
  (define theone (sort large* top-down-left-to-right-order< #:key candidate-place))
  (placement (candidate-place (first theone)) t))

#; {[Listof Candidate] -> [Listof Candidate]}
;; pick the ones with the largest number of constraints 
(define (pick-largest with#)
  (define mx (first (argmax first with#)))
  (filter-map (λ (x) (if (= (first x) mx) (second x) #false)) with#))

#; Strategy
(define ldasg-strategy (make-strategy most-constrained-placement))

(module+ test
  (check-equal? (ldasg-strategy info-special-state) constrained-special)
  (check-equal? (ldasg-strategy info-+ref-starter-state) ref-place)
  (check-equal? (ldasg-strategy info-starter-state) REPLACEMENT)
  (check-equal? (ldasg-strategy info-bad-state) PASS))

;; ---------------------------------------------------------------------------------------------------
#; {Map [Listof Tiles] -> Option<Placement>}
(define (is-there-a-placement gmap mine)
  (for*/first ([t (in-list mine)] [cs (in-value (find-candidates gmap t))] #:unless (set-empty? cs))
    (list t cs)))

;; ---------------------------------------------------------------------------------------------------

#; {Strategy PubKnowledge -> (U PASS REPLACE [Listof Placement])}
(define (iterate-strategy s pk0)
  (let until ([so-far '()] [pk pk0])
    (define action (s pk))
    (cond
      [(or (equal? PASS action) (equal? REPLACEMENT action))
       (if (empty? so-far) action (reverse so-far))]
      [else (until (cons action so-far) (apply-action pk action))])))

(module+ test
  (define info-special-place*
    '(#s(placement #s(coordinate -5 1) #s(tile star green))
      #s(placement #s(coordinate -6 1) #s(tile diamond green))))
  (define info-special-places*dug
    '(#s(placement #s(coordinate -3 1) #s(tile star green))
      #s(placement #s(coordinate -1 1) #s(tile diamond green))))

  ;; these have tiles that don't fit 
  (check-equal? (iterate-strategy ldasg-strategy info-bad-state) PASS)
  (check-equal? (iterate-strategy dag-strategy info-starter-state) REPLACEMENT)

  ;; the following two run out of tiles
  (check-equal? (iterate-strategy ldasg-strategy info-special-state) info-special-places*dug)
  (check-equal? (iterate-strategy dag-strategy info-special-state) info-special-place*)
  (check-equal? (iterate-strategy dag-strategy info-+ref-starter-state) (list ref-place)))
