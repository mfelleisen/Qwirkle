#lang racket

(provide
 ;; SYNTAX

 #; (def/jsexpr-> name #:array [pat body ...])
 #; (def/jsexpr-> name #:object {[key optional:parse pat] ... [key2 pat2] ...})
 ;; defines a function named `jsexpr->name` that parses either an JSexpr array or object
 ;;   where an object's value may optionally be parsed with `jsexpr->parse`
 ;; 
 ;; `jsexpr->name` :: JSexpr -> Option<... type of body ...>
 ;; if `jsexpr->name` returns #false, it also prints a suitable error message to error-port,
 ;; including the mismatched object or array 
 def/jsexpr->)

(provide
  jsexpr->boolean boolean->jsexpr
  jsexpr->natural natural->jsexpr
  jsexpr->void    void->jsexpr
  jsexpr->string  string->jsexpr
  jsexpr->symbol  symbol->jsexpr)

(provide
 jsexpr->string/
 write-json/)

(provide
 (all-from-out json))

;; ---------------------------------------------------------------------------------------------------
(require (for-syntax syntax/parse))
(require (for-syntax racket/syntax))

(require (except-in json write-json jsexpr->string))
(require (prefix-in old: (only-in json write-json jsexpr->string)))

(module+ test
  (require Qwirkle/Lib/check-message)
  (require rackunit))

;; ---------------------------------------------------------------------------------------------------

(define (jsexpr->string/ content)
  (define r (regexp-match #px"8.10\\." (version)))
  (if r 
      (old:jsexpr->string content #:indent 4)
      (old:jsexpr->string content)))

(define [write-json/ content]
  (define r (regexp-match #px"8.10\\." (version)))
  (if r 
      (old:write-json content #:indent 4)
      (old:write-json content)))


;; ---------------------------------------------------------------------------------------------------
(define-syntax (def/jsexpr-> stx)
  (syntax-parse stx
    [(_ to #:plain pred? (~optional r))
     #:with name (format-id stx "jsexpr->~a" #'to #:source #'to #:props stx)
     #`(define name
         (λ (j)
           (match j
             [(? pred? x) (~? (r x) x)]
             [_ (eprintf "~a value does not satisfy ~a?\n  ~a\n" 'name 'to (jsexpr->string j))
                #false])))]

    [(_ to #:array [pat body ...])
     #:with name (format-id stx "jsexpr->~a" #'to #:source #'to #:props stx)
     #'(define name
         (λ (j)
           (match j
             [pat body ...]
             [_ (eprintf "~a object does not match schema\n ~a\n" 'name (jsexpr->string/ j))
                #false])))]
    
    [(_ to #:object {{key (~optional parse #:defaults ([parse #'id])) pat} ...} body ...)
     #:with jsexpr-parser  (format-id stx "jsexpr->~a" #'to #:source #'to #:props stx)
     #:do [(define plist (syntax->list #'(parse ...)))]
     #:with (jsexpr-> ...) (map (λ (u) (format-id u "jsexpr->~a" u #:source u #:props u)) plist)
     #'(define jsexpr-parser
         (λ (j)
           (match j
             [(hash-table [(? (curry eq? key)) (app jsexpr-> pat)] ...) body ...]
             [_ (eprintf "~a object does not match schema\n ~a\n" 'name (jsexpr->string/ j))
                #false])))]))

(define (jsexpr->id x) x)

;; ---------------------------------------------------------------------------------------------------
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

;; ---------------------------------------------------------------------------------------------------
(module+ test
  (def/jsexpr-> tst #:object {['key aux (? natural? n)] ['key2 x]} (list (list 'key (list n x))))
  (def/jsexpr-> aux #:array [[list (? natural? n)] n])

  (check-equal? (jsexpr->tst (hash 'key '[10] 'key2 'x)) '[[key (10 x)]])
  (check-false (check-message "json" current-error-port #px"aux" (jsexpr->aux 11)))
  (check-false (check-message "json" current-error-port #px"aux" (jsexpr->tst (hash 'key 11)))))
