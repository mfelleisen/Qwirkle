#lang racket

;; support for defininf configurations

;; ---------------------------------------------------------------------------------------------------

(provide
 ;; SYNTAX
 #; (define-configuration name [key value] ...)
 ;; creates configurations as dictionaries 
 ;; -- default dict configuration named default-name-config
 ;; -- a list of availble options named name-options
 ;; -- a domain-contract named name-config/c ensuring that the domain consists of just the keys
 ;; -- a configure-set function named set-name-config
 #;    (set-name-config c KEY VALUE ...)
 define-configuration

 is-list-of-key-value-pairs)

;; ---------------------------------------------------------------------------------------------------
(require SwDev/Lib/hash-contract)
(require (for-syntax syntax/parse))
(require (for-syntax racket/syntax))

;; ---------------------------------------------------------------------------------------------------
(define-syntax (define-configuration stx)
  (syntax-parse stx
    [(_ name [key value0 (~optional (~seq #:jsexpr to #;function) #:defaults ([to #'identity]))] ...)
     #:do   [(define n (syntax-e #'name))]
     #:with name-options (format-id stx "~a-options" n #:source #'name #:props stx)
     #:with name/c       (format-id stx "~a-config/c" n #:source #'name #:props stx)
     #:with default-name (format-id stx "default-~a-config" n #:source #'name #:props stx)
     #:with set-name     (format-id stx "set-~a-config" n #:source #'name #:props stx)
     #:with name->jsexpr (format-id stx "~a-config->jsexpr" n #:source #'name #:props stx)
     #'(begin
         (define key (gensym 'key)) ...
         (define name-options [list key ...])
         (define default-name
           (let* ([h (hash)]
                  [h (dict-set h key value0)]
                  ...)
             h))

         #; {Contract}
         (define name/c [hash-carrier/c name-options])
         
         #; {(set-name c Key1 Value1 ... KeyN ValueN) : Void}
         (define (set-name config . key-value-pairs0)
           (define key-value-pairs (is-list-of-key-value-pairs key-value-pairs0))
           (cond
             [(false? key-value-pairs)
              (error 'set-name "key-value pair expected; given ~a" key-value-pairs0)]
             [else 
              (for/fold ([h config]) ([kv-pair key-value-pairs])
                (match-define [list k v] kv-pair)
                (dict-set h k v))]))

         #; {(name->jsexpr c) :: JSexpr}
         (define [name->jsexpr c]
           (let* ([h (hash)]
                  [h (dict-set h 'key (to (dict-ref c key)))] ...)
             h))
         )]))

#; {[Listof Any] -> [Option [Listof [List Symbol Any]]]}
(define (is-list-of-key-value-pairs key-value-pairs)
  (let loop ([key-value-pairs key-value-pairs] [h '()])
    (match key-value-pairs
      [(list) (reverse h)]
      [(list x) #false]
      [(list* k v key-value-pairs) (loop key-value-pairs (cons [list k v] h))])))

;; ---------------------------------------------------------------------------------------------------
(module+ test
  (require rackunit)
  (require json)

  (define-configuration server
    (PORT            0)
    (SERVER-TRIES    1)
    (SERVER-WAIT     2)
    (WAIT-FOR-SIGNUP 3)
    (REF-SPEC        4 #:jsexpr (λ (x) 0))
    (QUIET           5))

  (check-equal? [length server-options] 6)
  [check-equal? [contract? server-config/c] #true]
  [check-equal? (dict-ref default-server-config REF-SPEC) 4]
  [check-equal? (dict-ref (set-server-config default-server-config PORT 1 REF-SPEC 17) REF-SPEC) 17]

  (check-true
   (jsexpr? (server-config->jsexpr (set-server-config default-server-config PORT 1 REF-SPEC 17))))

  [check-exn #px"key-value pair" (λ () (set-server-config default-server-config PORT 1 REF-SPEC))])

