#lang racket

;; the Q specific rule for fitting a tile into a space

(define opt-tile/c (or/c #false tile?))
(define fit-into-spot/c (-> tile? opt-tile/c opt-tile/c opt-tile/c opt-tile/c boolean?))

(provide
 fit-into-spot/c
 
 (contract-out
  ;; does `tile` fit into an empty space with left, right, top, below as neighbors (if any)
  [q-fits fit-into-spot/c]))

;; ---------------------------------------------------------------------------------------------------
(require Qwirkle/Common/tiles)

;; ---------------------------------------------------------------------------------------------------
#; {Tile Tile Tile Tile Tile -> Boolean}
(define (q-fits tile left right top below)
  (and (fit-line left tile right)
       (fit-line top tile below)))

#; {[Option Tile] Tile [Option Tile] -> Boolean}
;; is the tile compatible with the tiles on either side (if any) or both (if they exist) 
(define (fit-line one-side tile other-side)
  (define color (tile-color tile))
  (define shape (tile-shape tile))
  ;; tile/m
  (cond
    [(and (not one-side) (not other-side)) #true]
    [(not one-side)
     (or (equal? (tile-shape other-side) shape) (equal? (tile-color other-side) color))]
    ((not other-side)
     (or (equal? (tile-shape one-side) shape) (equal? (tile-color one-side) color)))
    [else (or
           (and (equal? (tile-shape one-side) shape) (equal? (tile-shape other-side) shape))
           (and (equal? (tile-color one-side) color) (equal? (tile-color other-side) color)))]))
