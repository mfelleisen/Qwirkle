#lang racket

;; for generating serializing and deserializing plain values from and to JSexpr

(provide
  jsexpr->boolean boolean->jsexpr
  jsexpr->natural natural->jsexpr
  jsexpr->void    void->jsexpr
  jsexpr->string  string->jsexpr
  jsexpr->symbol  symbol->jsexpr)

;; ---------------------------------------------------------------------------------------------------

(require "parse-json.rkt")

; natural? 
(define (natural->jsexpr x) x)
(def/jsexpr-> natural #:plain natural?)

; boolean?
(define (boolean->jsexpr b) b)
(define (jsexpr->boolean x) x) ;; this makes sense because a parser must test boolean? first

; void?
(define (void->jsexpr v) "void")
(def/jsexpr-> void #:plain (curry equal? "void") void)

; string?
(define string->jsexpr values)
(def/jsexpr-> string #:plain string?)

;; symbols
(define symbol->jsexpr ~a)
(define jsexpr->symbol string->symbol)