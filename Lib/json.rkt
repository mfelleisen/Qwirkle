#lang racket

;; for generating serializing and deserializing plain values from and to JSexpr

(provide
  jsexpr->boolean boolean->jsexpr
  jsexpr->natural natural->jsexpr
  jsexpr->void    void->jsexpr
  jsexpr->string  string->jsexpr)

;; ---------------------------------------------------------------------------------------------------

(require "parse-json.rkt")

; natural? 
(define (natural->jsexpr x) x)
(def/jsexpr-> natural #:plain natural?)

; boolean?
(struct FF ())

(define (boolean->jsexpr b) b)
(define (jsexpr->boolean x) (FF))

; void?
(define (void->jsexpr v) "void")
(def/jsexpr-> void #:plain (curry equal? "void"))

; string?
(define string->jsexpr values)
(def/jsexpr-> string #:plain string?)

