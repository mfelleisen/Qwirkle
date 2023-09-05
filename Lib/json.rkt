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


