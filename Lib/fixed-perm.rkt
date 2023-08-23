#lang racket

(provide
 (contract-out 
  [pick-fixed-permutation
   (->i ([l (listof any/c)]) [r (l) (and/c (listof any/c) (λ (x) (= (length x) (length l))))])]))

;; ---------------------------------------------------------------------------------------------------

;; make fixed random number generator 
(define prgv (vector 0 1 4294967086 4294944442 2 0))
(define rgen (vector->pseudo-random-generator prgv))

#; {{Listof X} -> [Listof X]}
(define (pick-fixed-permutation lox)
  (let loop ([l lox])
    (cond
      [(empty? l) l]
      [else 
       (define n (random (length l) rgen))
       (define e (list-ref l n))
       (cons e (loop (remove e l)))])))
