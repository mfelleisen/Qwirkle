#lang info

(define collection "Qwirkle")

(define deps
  (list
    "htdp-lib"
    "SwDev"
    "rackunit-lib"
    "gui-lib"
    "base"))

(define build-deps
  (list
    "at-exp-lib"
    "scribble-abbrevs"
    "scribble-lib"
    "typed-racket-lib"
    "sandbox-lib"
    "racket-doc"
    "rackunit-lib"))

(define scribblings '(("scribblings/qwirkle.scrbl" ())))

(define pkg-desc "A Q Game Implementation")

(define version "0.1")

(define pkg-authors '(matthias))

(define license '(Apache-2.0 OR MIT))
