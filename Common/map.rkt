#lang racket

;; data representation of map

;; ---------------------------------------------------------------------------------------------------
(provide
 #; {type Map}
 map? 
 
 (contract-out
  [start-map  (-> tile? map?)]
  [add-tile   (->i ([b map?] [c coordinate?] [t tile?]) #:pre (b c) (adjacent? b c) (r map?))]
  [render-map (-> map? 2:image?)]))

(module+ examples
  (provide
   starter-map
   lshaped-map))

;; ---------------------------------------------------------------------------------------------------
(require Qwirkle/Common/coordinates)
(require Qwirkle/Common/tiles)
(require (prefix-in 2: 2htdp/image))

(module+ test
  (require (submod ".." examples))
  (require rackunit))

;; ---------------------------------------------------------------------------------------------------

#; {type map = [Hashof Coordinate Tile]}
#; {type Candidate  = [candidate Coordinate Option<Tile> Option<Tile> Option<Tile> Option<Tile>]}
#; {type Coordinate = [coordinate Integer Integer]}

(define map? hash?)

(define (start-map tile0)
  (hash origin tile0))

;; ---------------------------------------------------------------------------------------------------
;; add a tile that shares a side with an existing one on the map 

#; {Map Coordinate Tile -> Map}
(define (add-tile b c t)
  (hash-set b c t))

#; {Map Coordinate -> Boolean}
(define (adjacent? b c)
  (or (occupied b (left-of c))
      (occupied b (top-of c))
      (occupied b (right-of c))
      (occupied b (below-of c))))

#; {Map Coordinate -> (U Tile False)}
(define (occupied b co)
  (hash-ref b co #false))

;; ---------------------------------------------------------------------------------------------------
;; rendering a map

#; {Map -> Image}
(define (render-map b)
  (define-values [-column +column sorted] (sort-map-in-top-down-left-to-right-order b))
  (define rows (map (render-row -column) sorted))
  (apply 2:above/align 'left (cons #;{needed for the starter maps} 2:empty-image rows)))

#; {type TileMatrix = [Listof TileRow]}
#; {type TileRow    = [Listof [List Coordinate Tile]]}

#; {Integer -> TileRow -> [Listof Image]}
;; also fills gaps 
(define ((render-row left-most) row0)
  (define tile-1 (first row0))
  (define-values (image1 row1)
    (if (= (coordinate-column (first tile-1)) left-most)
        (values (render-tile (second tile-1)) (rest row0))
        (values blank row0)))
  (for/fold ([last (+ left-most 1)] [r image1] #:result r) ([cell row1])
    (match-define [list c tile] cell)
    (define column (coordinate-column c))
    (values (+ column 1) (2:beside r (blank-spaces (abs (- column last))) (render-tile tile)))))

#; {N -> Image}
(define (blank-spaces n)
  (cond
    [(= n 0) 2:empty-image]
    [(= n 1) blank]
    [else (apply 2:beside (make-list n blank))]))

#; {Map -> TileRow}
(define (sort-map-in-top-down-left-to-right-order b)
  (define as-list (hash-map b list))
  (define row-min (apply min (map (compose coordinate-row first) as-list)))
  (define row-max (apply max (map (compose coordinate-row first) as-list)))
  (define col-min (apply min (map (compose coordinate-column first) as-list)))
  (define col-max (apply max (map (compose coordinate-column first) as-list)))
  (define sorted  (sort as-list top-down-left-to-right-order< #:key first))
  (define matrix
    (for/fold ([m '()] [cr '()] [i row-min] #:result (reverse (add-cell cr m))) ([cell sorted])
      (define row# (coordinate-row (first cell)))
      (if (= row# i)
          (values m                (cons cell cr) i)
          (values (add-cell cr m)  (list cell)    row#))))
  (values col-min col-max matrix))

#; {TileRow TileMatrixƒ  -> TileMatrixƒ}
(define (add-cell cr m) (cons (reverse cr) m))

;; ---------------------------------------------------------------------------------------------------
(module+ examples
  (define starter-map (start-map (tile 'star 'red)))
  (define lshaped-map
    (let* ([s starter-map]
           [s (add-tile s (coordinate -1  0) (tile '8star 'green))]
           [s (add-tile s (coordinate +1  0) (tile 'square 'blue))]
           [s (add-tile s (coordinate +1 +1) (tile 'circle 'yellow))]
           [s (add-tile s (coordinate +1 +2) (tile 'clover 'purple))]
           [s (add-tile s (coordinate +1 +3) (tile 'diamond 'yellow))]
           [s (add-tile s (coordinate  0 +3) (tile 'circle 'orange))])
      s)))

(module+ test
  (render-map starter-map)
  (render-map lshaped-map))