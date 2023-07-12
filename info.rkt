#lang info

(define collection "Qwirkle")

(define deps
  '("htdp-lib"
    "base"))

(define build-deps
  '("at-exp-lib"
    "scribble-abbrevs"
    "typed-racket-lib"
    "scribble-lib" "racket-doc" "rackunit-lib"))

(define scribblings '(("scribblings/qwirkle.scrbl" ())))

(define pkg-desc "A Q Game Implementation")

(define version "0.1")

(define pkg-authors '(matthias))

(define license '(Apache-2.0 OR MIT))
