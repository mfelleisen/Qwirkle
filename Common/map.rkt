#lang racket

;; data representation of map

;; ---------------------------------------------------------------------------------------------------
(provide
 #; {type Map}
 map?

 #; {type Candidate  = [candidate Coordinate Option<Tile> Option<Tile> Option<Tile> Option<Tile>]}
 ;; at which coordinates can the given tile be placed to satisfy the "continues line" predicate
 ;; and what are its neighbors
 
 candidate?
 candidate-place 
 candidate-left
 candidate-top
 candidate-right
 candidate-below
 
 (contract-out
  [start-map  (-> tile? map?)]
  [add-tile   (->i ([b map?] [c coordinate?] [t tile?]) #:pre (b c) (adjacent? b c) (r map?))]
  [render-map (-> map? 2:image?)]
  [find-candidates
   (-> map? tile? (set/c candidate?))]
  [adjacent?
   ;; is the coordinate adjacent to, and not on top of, an occupied space?
   (-> map? coordinate? boolean?)]
  [fits
   ;; would the `tile` fit into this `map` at coordinate `co`
   (-> map? coordinate? tile? (or/c candidate? #false))]))

(module+ examples
  (provide starter-free
           start+1-map-unfit
           start+1-free
           start+1-can)
  (provide map1 map2 map3 map4 map5 map6 map7 map8 map9 map10 map11)
  (provide
   map0
   starter-map
   starter-can
   lshaped-map-unfit))

(module+ json
  (provide
   (contract-out
    [map->jsexpr (-> map? jsexpr?)]
    [jsexpr->map (-> jsexpr? (or/c map? #false))])))

;; ---------------------------------------------------------------------------------------------------
(require Qwirkle/Common/coordinates)
(require Qwirkle/Common/tiles)
(require (prefix-in 2: 2htdp/image))

(module+ examples
  (require (submod Qwirkle/Common/coordinates examples))
  (require (submod Qwirkle/Common/tiles examples)))

(module+ json
  (require (submod Qwirkle/Common/tiles json))
  (require json))

(module+ test
  (require (submod ".."))
  (require (submod ".." examples))
  (require (submod ".." json))
  (require (submod Qwirkle/Common/tiles examples))
  (require rackunit))

;; ---------------------------------------------------------------------------------------------------

#; {type map = [Hashof Coordinate Tile]}

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
(module+ test
  (check-true (adjacent? starter-map #s(coordinate 0 -1)))
  (check-false (adjacent? starter-map origin)))
(define (adjacent? b c)
  (and (false? (hash-ref b c #false))
       (tile? (or (occupied b (left-of c))
                  (occupied b (top-of c))
                  (occupied b (right-of c))
                  (occupied b (below-of c))))))

;; ---------------------------------------------------------------------------------------------------
#; {Map Tile -> [Listof Candidate]}

(struct candidate [place left top right below] #:prefab)

#; {Map Tile -> [Setof Candidate]}
(module+ test
  (check-equal? (find-candidates starter-map starter-tile) starter-can)
  (check-equal? (find-candidates start+1-map-unfit starter-tile) start+1-can))
(define (find-candidates map t)
  (for*/set ([co (in-set (all-free-neighbors map))] [can (in-value (fits map co t))] #:when can)
    can))

#; {Map Coordinate Tile -> [Option Candidate]}
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

#; {Map -> [Setof Coordinate]}
;; all coordinates of spots that neighbor an existing tile 
(module+ test
  (check-equal? (all-free-neighbors starter-map) (apply set starter-free))
  (check-equal? (all-free-neighbors start+1-map-unfit) start+1-free))
(define (all-free-neighbors map)
  (define as-list (hash-map map list))
  (for/fold ([r (set)]) ([cell (in-list as-list)])
    (foldr (λ (n s) (set-add s n)) r (free-neighbors map (first cell)))))

#; {Map Coordinate -> [Listof Coordinate]}
;; the free neighbors of one coordinate 
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

;; ---------------------------------------------------------------------------------------------------
#; {type TileMatrix = [Listof TileRow]}
#; {type TileRow    = [Listof [List Coordinate Tile]]}

#; {Map -> (values Integer Intger TileMatrix)}
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

  #; {Map [Listof Tile] [Listof Coordinate] -> Map}
  (define (add-tile* starter-map starter-tile* lshaped-coordinates)
    (for/fold ([s starter-map]) ([t starter-tile*] [co lshaped-coordinates]) (add-tile s co t))))

(module+ examples
  (define map0 ;; base scenario 
    (let* ([s (start-map #s(tile clover red))]
           [s (add-tile s #s(coordinate +1 0) #s(tile diamond red))]
           [s (add-tile s #s(coordinate +2 0) #s(tile circle red))])
      s))

  ;; maps from the Qwirkle web page 
  (define map1 (add-tile* map0 tiles0 coord0))
  (define map2 (add-tile* map1 tiles1 coord1))
  (define map3 (add-tile* map2 tiles2 coord2))
  (define map4 (add-tile* map3 tiles3 coord3))
  (define map5 (add-tile* map4 tiles4 coord4))
  (define map6 (add-tile* map5 tiles5 coord5))
  (define map7 (add-tile* map6 tiles6 coord6))
  (define map8 (add-tile* map7 tiles7 coord7))
  (define map9 (add-tile* map8 tiles8 coord8))
  (define map10 (add-tile* map9 tiles9 coord9))
  (define map11 (add-tile* map10 tiles10 coord10)))
  
(module+ examples
  (define starter-map (start-map starter-tile))
  (define starter-free [list [coordinate 0 -1] [coordinate -1 0] [coordinate 0 +1] [coordinate +1 0]])
  (define starter-can
    [set (candidate [coordinate  0 -1] #f #f starter-tile #f)
         (candidate [coordinate -1  0] #f #f #f starter-tile)
         (candidate [coordinate  0 +1] starter-tile #f #f #f)
         (candidate [coordinate +1  0] #f starter-tile #f #f)])
  
  (define start+1-map-unfit (add-tile starter-map (coordinate 0 -1) (tile '8star 'green)))
  (define start+1-free
    (let* ([s (rest starter-free)]
           [t (list (coordinate 0 -2) (coordinate -1 -1) (coordinate +1 -1))]
           [s (append s t)])
      (apply set s)))
  (define start+1-can (set-remove starter-can (candidate [coordinate  0 -1] #f #f starter-tile #f)))
  
  (define lshaped-map-unfit (add-tile* starter-map starter-tile* lshaped-coordinates)))

(module+ test ;; simplistic examples
  'start+1-map-unfit
  (render-map start+1-map-unfit)

  'lshaped-map-unfit
  (render-map lshaped-map-unfit)

  'starter-map 
  (render-map starter-map))

(module+ test ;; scenarios from Qwirkle 
  'map0 (render-map map0) 
  'map1 (render-map map1)
  'map2 (render-map map2)
  'map3 (render-map map3)
  'map4 (render-map map4)
  'map5 (render-map map5)
  'map6 (render-map map6)
  'map7 (render-map map7)
  'map8 (render-map map8)
  'map10 (render-map map10)
  'map11 (render-map map11))

;; ---------------------------------------------------------------------------------------------------
(module+ json

  #; {type JMap = [List [List Integer Cell ...] ...]}
  ;; the Integer denotes the row index for all Cells in this row 
  #; {type Cell = [List Integer JTile]}
  ;; the Integer denotes the column index for the tile 
  
  #; {Map -> JMap}
  (define (map->jsexpr b)
    (define-values [-column +column sorted] (sort-map-in-top-down-left-to-right-order b))
    (rows->jsexpr sorted))

  #; {TileMatrix -> JRow}
  (define (rows->jsexpr rows)
    (for/list ([r rows])
      (define row-index (coordinate-row (first (first r))))
      (cons row-index (1row->jsexpr r))))

  #; {TileRow -> JCell}
  (define (1row->jsexpr row)
    (for/list ([c row])
      (match-define [list co ti] c)
      (list (coordinate-column co) (tile->jsexpr ti))))

  #; {JSexpr -> Option<Tile>}
  (define (jsexpr->map j)
    (match j
      [(list (and row (list (? integer?) (list (? integer?) (app jsexpr->tile (? tile?))) ...)) ...)
       (for/fold ([h (hash)]) ([r row])
         (match-define (list ri cell ...) r)
         (for/fold ([h h]) ([c cell])
           (match-define [list ci ti] c)
           (hash-set h (coordinate ri ci) (jsexpr->tile ti))))]
      [_ (eprintf "map object does not match schema\n  ~a\n" (jsexpr->string j #:indent 4))
         #false])))
                     
(module+ test
  (check-equal? (jsexpr->map (map->jsexpr map0)) map0)
  (check-equal? (jsexpr->map (map->jsexpr map1)) map1)
  (check-equal? (jsexpr->map (map->jsexpr map2)) map2))
  