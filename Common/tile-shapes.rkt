#lang racket

;; the shapes of tiles

(provide shape? ALL-SHAPES shape< shape= complete-set-of-shapes SHAPES# shape-of-color)

(module+ json
  (provide jsexpr->shape shape->jsexpr))

;; ---------------------------------------------------------------------------------------------------
(require (prefix-in 2: 2htdp/image))
(module+ json
  (require Qwirkle/Lib/parse-json))

;; ---------------------------------------------------------------------------------------------------
(define (4point-star c) (2:radial-star 4 15 42 'solid c))
(define (8point-star c) (2:radial-star 8 30 15 'solid c))
(define (square-ish c)  (2:square 58 'solid c))
(define (circle-ish c)  (2:circle 29 'solid c))
(define (diamondish c)  (2:rhombus 41 90 'solid c))
(define (clover-ish c)
  (define one (2:rectangle 2 2 'solid c))
  (let* ([s (2:circle 14 'solid c)]
         [t (2:above s one s)]
         [s (2:beside s one s)]
         [s (2:overlay s t)])
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

(unless (and (apply = (map 2:image-width (map (lambda (x) (x 'green)) (map second ALL-SHAPES))))
             (apply = (map 2:image-height (map (lambda (x) (x 'green)) (map second ALL-SHAPES)))))
  (error "images must be same size"))

(define SHAPE-NAMES (map first ALL-SHAPES))

(define (shape< ss ts)
  (< (index-of SHAPE-NAMES ss) (index-of SHAPE-NAMES ts)))

(define (shape= ss ts)
  (= (index-of SHAPE-NAMES ss) (index-of SHAPE-NAMES ts)))

(define SHAPES# (length ALL-SHAPES))
(define (complete-set-of-shapes shapes)
  (= (set-count (apply set shapes)) SHAPES#))

(define (shape-of-color s)
  (second [assq s ALL-SHAPES]))

;; ---------------------------------------------------------------------------------------------------
(module+ json
  (define (shape->jsexpr shape) (symbol->jsexpr shape))

  (define (jsexpr->shape j)
    (define s (jsexpr->symbol j))
    (and (shape? s) s)))