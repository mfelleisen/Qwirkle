#lang racket

;; data representation of map

;; ---------------------------------------------------------------------------------------------------
(provide
 #; {type Map}
 map? 
 
 (contract-out
  [start-map  (-> tile? map?)]
  [add-tile   (->i ([b map?] [c coordinate?] [t tile?]) #:pre (b c) (adjacent? b c) (r map?))]
  [fits       (-> map? coordinate? tile? (or/c candidate? #false))]
  [render-map (-> map? 2:image?)]))

(module+ examples
  (provide starter-free
           start+1-map
           start+1-free
           start+1-can))

(module+ examples
  (provide
   starter-map
   starter-can
   lshaped-map))

;; ---------------------------------------------------------------------------------------------------
(require Qwirkle/Common/coordinates)
(require Qwirkle/Common/tiles)
(require (prefix-in 2: 2htdp/image))

(module+ examples
  (require (submod Qwirkle/Common/coordinates examples))
  (require (submod Qwirkle/Common/tiles examples)))

(module+ test
  (require (submod ".."))
  (require (submod ".." examples))
  (require (submod Qwirkle/Common/tiles examples))
  (require rackunit))

;; ---------------------------------------------------------------------------------------------------

#; {type map = [Hashof Coordinate Tile]}
#; {type Candidate  = [candidate Coordinate Option<Tile> Option<Tile> Option<Tile> Option<Tile>]}

(define map? hash?)

(define (start-map tile0)
  (hash origin tile0))

;; ---------------------------------------------------------------------------------------------------
;; add a tile that shares a side with an existing one on the map 

#; {Map Coordinate Tile -> Map}
;; pre: c must be adjacent to some other tile meaning the two share a tile 
(define (add-tile b c t)
  (hash-set b c t))

#; {Map Coordinate -> Boolean}
(define (adjacent? b c)
  (or (occupied b (left-of c))
      (occupied b (top-of c))
      (occupied b (right-of c))
      (occupied b (below-of c))))

;; ---------------------------------------------------------------------------------------------------
#; { Map Tile -> [Listof Candidate]}
;; at which coordinates can the given tile be placed to satisfy the "continues line" predicate
;; and what are its neighbors

(struct candidate [place left top right below] #:prefab)

#; {Map Tile -> [Setof Candidate]}
(module+ test
  (check-equal? (find-candidates starter-map starter-tile) starter-can)
  (check-equal? (find-candidates start+1-map starter-tile) start+1-can))
(define (find-candidates map t)
  (for*/set ([co (in-set (all-free-neighbors map))] [can (in-value (fits map co t))] #:when can)
    can))

#; {Map Coordinate Tile -> [Option Candidate]}
;; would the `tile` fit into this `map` at coordinate `co`
(define (fits map co tile)
  (define left  (neighbor-tile map co left-of))
  (define top   (neighbor-tile map co top-of))
  (define right (neighbor-tile map co right-of))
  (define below (neighbor-tile map co below-of))
  (and (fit-line left tile right)
       (fit-line top tile below)
       ;; --- if it fits both ways, return: 
       (candidate co left top right below)))

#; {Map Coordinate [Coordinate -> Coordinate] -> [Option Tile]}
;; what's the tile (if any) that neighbors `co` in `direction`
(define (neighbor-tile map co direction)
  (hash-ref map (direction co) #false))

#; {[Option Tile] Tile [Option Tile] -> Boolean}
;; is the tile compatible with the tiles on either side (if any) or both (if they exist) 
(define (fit-line left tile right)
  (define color (tile-color tile))
  (define shape (tile-shape tile))
  ;; tile/m
  (cond
    [(and (not left) (not right)) #true]
    [(not left) (or (equal? (tile-shape right) shape) (equal? (tile-color right) color))]
    ((not right) (or (equal? (tile-shape left) shape) (equal? (tile-color left) color)))
    [else (or
           (and (equal? (tile-shape left) shape) (equal? (tile-shape right) shape))
           (and (equal? (tile-color left) color) (equal? (tile-color right) color)))]))

#; {Map -> [Setof Coordinate]}
(module+ test
  (check-equal? (all-free-neighbors starter-map) (apply set starter-free))
  (check-equal? (all-free-neighbors start+1-map) start+1-free))
(define (all-free-neighbors map)
  (define as-list (hash-map map list))
  (for/fold ([r (set)]) ([cell (in-list as-list)])
    (foldr (λ (n s) (set-add s n)) r (free-neighbors map (first cell)))))

#; {Map Coordinate -> [Listof Coordinate]}
(module+ test
  (check-equal? (free-neighbors starter-map origin) starter-free))
(define (free-neighbors map co)
  (append (1-neighbor map co left-of)
          (1-neighbor map co top-of)
          (1-neighbor map co right-of)
          (1-neighbor map co below-of)))

#; {Map Coordinate [Coordinate -> Coordinate] -> [Listof Coordinate]}
;; is `(next-neighbor co)` unoccupied? if so, it's returned in a list; otherwise '[]
(define (1-neighbor map co next-coordinate)
  (if (occupied map (next-coordinate co)) '[] (list (next-coordinate co))))

;; ---------------------------------------------------------------------------------------------------
#; {Map Coordinate -> [Option Tile]}
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
        (values (render-tile+ tile-1) (rest row0))
        (values blank row0)))
  (for/fold ([last (+ left-most 1)] [r image1] #:result r) ([cell row1])
    (match-define [list c tile] cell)
    (define column (coordinate-column c))
    (values (+ column 1) (2:beside r (blank-spaces (abs (- column last))) (render-tile+ cell)))))

#; {[List Coordinate Tile] -> Image}
(define (render-tile+ co+tile)
  (match-define [list co tile] co+tile)
  (define co-as-text (~a "(" (coordinate-row co) ","  (coordinate-column co) ")"))
  (2:overlay (2:text co-as-text 11 'black) (render-tile tile)))
  
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
  (define starter-map (start-map starter-tile))
  (define starter-free [list [coordinate 0 -1] [coordinate -1 0] [coordinate 0 +1] [coordinate +1 0]])
  (define starter-can
    [set (candidate [coordinate  0 -1] #f #f starter-tile #f)
         (candidate [coordinate -1  0] #f #f #f starter-tile)
         (candidate [coordinate  0 +1] starter-tile #f #f #f)
         (candidate [coordinate +1  0] #f starter-tile #f #f)])
  
  (define start+1-map (add-tile starter-map (coordinate 0 -1) (tile '8star 'green)))
  (define start+1-free
    (let* ([s (rest starter-free)]
           [t (list (coordinate 0 -2) (coordinate -1 -1) (coordinate +1 -1))]
           [s (append s t)])
      (apply set s)))
  (define start+1-can (set-remove starter-can (candidate [coordinate  0 -1] #f #f starter-tile #f)))
  
  (define lshaped-map
    (for/fold ([s starter-map]) ([t starter-tile*] [co lshaped-coordinates]) (add-tile s co t))))

(module+ test
  (render-map starter-map)
  '---
  (render-map start+1-map)
  '---
  (render-map lshaped-map))