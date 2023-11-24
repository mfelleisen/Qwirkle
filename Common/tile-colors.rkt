#lang racket

;; the colors of tiles 

(provide color? ALL-COLORS color< complete-set-of-colors COLORS#)

(module+ json
  (provide jsexpr->color color->jsexpr))

;; ---------------------------------------------------------------------------------------------------
(module+ json
  (require Qwirkle/Lib/parse-json))

;; ---------------------------------------------------------------------------------------------------
(define ALL-COLORS '(red green blue yellow orange purple))

(define (color? x) (cons? (member x ALL-COLORS)))

(define (color< sc tc)
  (< (index-of ALL-COLORS sc) (index-of ALL-COLORS tc)))

(define COLORS# (length ALL-COLORS))

(define (complete-set-of-colors colors)
  (= (set-count (apply set colors)) COLORS#))

;; ---------------------------------------------------------------------------------------------------
(module+ json
  (define (color->jsexpr c) (symbol->jsexpr c))

  (define (jsexpr->color j)
    (define c (jsexpr->symbol j))
    (and (color? c) c)))
