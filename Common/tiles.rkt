#lang racket

;; data representation of tiles, shapes, and colors

(provide
 #; {type Tile}
 tile?

 (contract-out 
  [tile (-> (or/c 'star '8star 'square 'circle 'clover 'diamond)
            (or/c 'red 'green 'blue 'yellow 'orange 'purple)
            tile?)])
 
 #; {Tile -> Image}
 render-tile

 #; Image
 blank

 #; {type Shape = (U 'star '8star 'square 'circle 'clover 'diamond)}
 #; {type Color = (U 'red 'green 'blue 'yellow 'orange 'purple)}
 
 #; {Any -> Boolean : ShapeSymbol}
 shape?

 #; {Any -> Boolean : ColorSymbol}
 color?)

(provide ;; for documentation 
 #; {-> [Pair [Listof name-of-color:String]
              [Listof [Pair name-of-shape:String [Listof Image]]]]}
 ;; the very first string is "" to make the table square 
 render-all-shapes)

;; ---------------------------------------------------------------------------------------------------
(require 2htdp/image)

;; ---------------------------------------------------------------------------------------------------
(struct tile [shape color] #:prefab)

;; ---------------------------------------------------------------------------------------------------
;; the shapes and colors

(define (4point-star c) (radial-star 4 15 42 'solid c))
(define (8point-star c) (radial-star 8 30 15 'solid c))
(define (square-ish c)  (square 58 'solid c))
(define (circle-ish c)  (circle 29 'solid c))
(define (diamondish c)  (rhombus 41 90 'solid c))
(define (clover-ish c)
  (define one (rectangle 2 2 'solid c))
  (let* ([s (circle 14 'solid c)]
         [t (above s one s)]
         [s (beside s one s)]
         [s (overlay s t)])
    s))

(define ALL-SHAPES
  (list
   (list 'star    4point-star)
   (list '8star   8point-star)
   (list 'square  square-ish)
   (list 'circle  circle-ish)
   (list 'clover  clover-ish)
   (list 'diamond diamondish)))

(define (shape? x) (cons? (assq x ALL-SHAPES)))

(unless (and (apply = (map image-width (map (lambda (x) (x 'green)) (map second ALL-SHAPES))))
             (apply = (map image-height (map (lambda (x) (x 'green)) (map second ALL-SHAPES)))))
  (error "images must be same size"))

(define ALL-COLORS '(red green blue yellow orange purple))

(define (color? x) (cons? (member x ALL-COLORS)))

;; ---------------------------------------------------------------------------------------------------
(define (render-all-shapes [t values])
  (define all-shapes
    (for/list ([s ALL-SHAPES])
      (define sname  (first s))
      (define render (second s))
      (define tiles (map (compose frame render) ALL-COLORS))
      (cons (t (~a sname)) tiles)))
  (cons (map t (cons "" (map ~a ALL-COLORS))) all-shapes))

#; {Image [N] -> Image}
;; provide equal white space around all edges 
(define (frame i [white 4])
  (define w (image-width i))
  (define h (image-height i))
  (overlay i (rectangle (+ white w) (+ white h) 'solid 'white)))

;; ---------------------------------------------------------------------------------------------------
(define (render-tile 1tile)
  (match-define [tile s c] 1tile)
  (define t [(second [assq s ALL-SHAPES]) (first (member c ALL-COLORS))])
  (overlay t (rectangle (+ (image-width t) 6) (+ (image-height t) 6) 'outline 'black)))

(define blank
  (let* ([s (render-tile (tile 'star 'green))]
         [w (image-width s)]
         [h (image-height s)]
         [s (rectangle w h 'solid 'white)])
    s))