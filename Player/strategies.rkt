#lang racket

;; simple strategies for picking a single placement 

(provide
 #; {type Strategy = PubKnowledge -> (U Placement 'pass 'replace)}
 ;; if there are no tiles and the referee has enough for a 'swap', ask for a 'replacement
 ;; otherwise 'pass
 (contract-out
  (d&g-strategy
   ;; strategy 1: dumb and greedy
   ;; choose "smallest tile" that has candidates; break tie among candidates via coodinate<
   (-> state? action?))
  (nsd&g-strategy
   ;; strategy 2: a bit more sophisticate and still greedy
   ;; choose "smallest tile" that has candidates; pick most-constrained candidates;
   ;; break tie among candidates via coodinate<   
   (-> state? action?))
  ))

;; Tiles are lexically ordered as follows:
;; 'star '8star 'square 'circle 'clover 'diamond
;; 'red 'green 'blue 'yellow 'orange 'purple

;; ---------------------------------------------------------------------------------------------------
(require Qwirkle/Common/game-state)
(require Qwirkle/Common/map)
(require Qwirkle/Common/coordinates)
(require Qwirkle/Common/placement)
(require Qwirkle/Common/tiles)

(module+ test
  (require (submod Qwirkle/Common/game-state examples))
  (require rackunit))

;; ---------------------------------------------------------------------------------------------------
#; Strategy 
(define (d&g-strategy pk)
  (define gmap      (state-map pk))
  (define my-tiles  (sort (sop-tiles (first (state-players pk))) tile<))
  (define remaining (state-tiles pk))
  (define exists?   (is-there-a-placement gmap my-tiles))
  (cond
    [exists? (apply smallest exists?)]
    [(> (length my-tiles) remaining) PASS]
    [else REPLACEMENT]))

#; {Map [Listof Tiles] -> Option<Placement>}
(define (is-there-a-placement gmap mine)
  (for*/first ([t (in-list mine)] [cs (in-value (find-candidates gmap t))] #:unless (set-empty? cs))
    (list t cs)))

#; {Tile [NonMTSet Candidate] -> Placement}
(define (smallest t cs)
  (define as-list (set->list cs))
  (define sorted  (sort as-list top-down-left-to-right-order< #:key candidate-place))
  (placement (candidate-place (first sorted)) t))

(module+ test
  (define the-special-place #s(placement #s(coordinate -5 1) #s(tile star green)))
  (define ref-place #s(placement #s(coordinate -1 0) #s(tile circle red)))
  (check-equal? (d&g-strategy info-special-state) the-special-place)
  (check-equal? (d&g-strategy info-+ref-starter-state) ref-place)
  (check-equal? (d&g-strategy info-starter-state) REPLACEMENT)
  (check-equal? (d&g-strategy info-bad-state) PASS))

;; ---------------------------------------------------------------------------------------------------
;; find candidates for tiles 

#; Strategy 
(define (nsd&g-strategy pk)
  (define gmap      (state-map pk))
  (define my-tiles  (sort (sop-tiles (first (state-players pk))) tile<))
  (define remaining (state-tiles pk))
  (define exists?   (are-there-candidates gmap my-tiles))
  (cond
    [exists? (apply most-constrained-placement exists?)]
    [(> (length my-tiles) remaining) PASS]
    [else REPLACEMENT]))

#; {Map [Listof Tiles] -> [List Tile [Set Candidate]]}
(define (are-there-candidates gmap mine)
   (for*/first ([t (in-list mine)] [cs (in-value (find-candidates gmap t))] #:unless (set-empty? cs))
     (list t cs)))

#;{Tile [Setof Candidate] -> Placement}
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

(module+ test
  (define constrained-special #s(placement #s(coordinate -3 1) #s(tile star green)))
  (check-equal? (nsd&g-strategy info-special-state) constrained-special)
  (check-equal? (nsd&g-strategy info-+ref-starter-state) ref-place)
  (check-equal? (nsd&g-strategy info-starter-state) REPLACEMENT)
  (check-equal? (nsd&g-strategy info-bad-state) PASS))
