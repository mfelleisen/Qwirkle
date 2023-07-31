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

;; ---------------------------------------------------------------------------------------------------
(require (for-syntax syntax/parse))
(require (for-syntax racket/syntax))
(require json)

(module+ test
  (require Qwirkle/Lib/check-message)
  (require rackunit))

;; ---------------------------------------------------------------------------------------------------
(define-syntax (def/jsexpr-> stx)
  (syntax-parse stx
    [(_ arr #:array [pat body ...])
     #:with unparser (format-id stx "jsexpr->~a" #'arr #:source #'arr #:props stx)
     #'(define unparser
         (λ (j)
           (match j
             [pat body ...]
             [_ (eprintf "~a object does not match schema\n  ~a\n" 'arr (jsexpr->string j #:indent 2))
                #false])))]
    [(_ obj #:object {{key (~optional parse #:defaults ([parse #'id])) pat} ...} body ...)
     #:with jsexpr-parser  (format-id stx "jsexpr->~a" #'obj #:source #'obj #:props stx)
     #:do [(define plist (syntax->list #'(parse ...)))]
     #:with (jsexpr-> ...) (map (λ (u) (format-id u "jsexpr->~a" u #:source u #:props u)) plist)
     #'(define jsexpr-parser
         (λ (j)
           (match j
             [(hash-table [(? (curry eq? key)) (app jsexpr-> pat)] ...) body ...]
             [_ (eprintf "~a object does not match schema\n  ~a\n" 'obj (jsexpr->string j #:indent 2))
                #false])))]))

(define (jsexpr->id x) x)

;; ---------------------------------------------------------------------------------------------------
(module+ test
  (def/jsexpr-> tst #:object {['key aux (? natural? n)] ['key2 x]} (list (list 'key (list n x))))
  (def/jsexpr-> aux #:array [[list (? natural? n)] n])

  (check-equal? (jsexpr->tst (hash 'key '[10] 'key2 'x)) '[[key (10 x)]])
  (check-false (check-message current-error-port #px"aux" (jsexpr->aux 11)))
  (check-false (check-message current-error-port #px"tst" (jsexpr->tst (hash 'key 11)))))