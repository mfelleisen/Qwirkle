#lang at-exp racket

;; support for defining configurations

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
 ;; -- to and from JSexpr conversions
 #;    (name-config->jsexpr c)
 #;    (jsexpr->name-config j)
 ;; -- a scribble rendering as a "schema definition" named name->text
 define-configuration

 is-list-of-key-value-pairs)

;; ---------------------------------------------------------------------------------------------------
(require SwDev/Lib/hash-contract)
(require (for-syntax syntax/parse))
(require (for-syntax racket/syntax))

;; ---------------------------------------------------------------------------------------------------
(define-syntax (define-configuration stx)
  (syntax-parse stx
    [(_ name
        [key value0
         (~optional (~seq #:to-jsexpr to #;function) #:defaults ([to #'identity]))
         (~optional (~seq #:from-jsexpr from #;function) #:defaults ([from #'identity]))
         (~optional (~seq #:is-a is-a ... #;string) #:defaults ([(is-a 1) null]))]
        ...)
     #:do   [(define n (syntax-e #'name))]
     #:with (keyv ...)   (generate-temporaries #'(key ...))
     #:with name-options (format-id stx "~a-options" n #:source #'name #:props stx)
     #:with name/c       (format-id stx "~a-config/c" n #:source #'name #:props stx)
     #:with default-name (format-id stx "default-~a-config" n #:source #'name #:props stx)
     #:with set-name     (format-id stx "set-~a-config" n #:source #'name #:props stx)
     #:with name->jsexpr (format-id stx "~a-config->jsexpr" n #:source #'name #:props stx)
     #:with jsexpr->name (format-id stx "jsexpr->~a-config" n #:source #'name #:props stx)
     #:with name->def    (format-id stx "~a-config->definition" n #:source #'name #:props stx)
     #'(begin
         (define key (gensym 'key)) ...
         (define name-options [list key ...])
         (define default-name (add-to 'default (hash) [list [list key value0] ...] "" name-options))

         #; {Contract}
         (define name/c [hash-carrier/c name-options])
         
         #; {(set-name c Key1 Value1 ... KeyN ValueN) : Void}
         (define (set-name config . key-value-pairs0)
           (define key-value-pairs (is-list-of-key-value-pairs key-value-pairs0))
           (add-to 'set-name config key-value-pairs key-value-pairs0 name-options))
           
         #; {Configuration -> JSexpr}
         (define key*   `[,(normalize 'key) ...])
         (define g-key* `[,key ...])
         (define to*    `[,to ...])
         (define [name->jsexpr c] (config->jsexpr c key* g-key* to*))

         #; {JSexpr -> [Option Configuration]}
         (define [jsexpr->name j]
           (match j
             [(hash-table
               [(? (curry eq? (normalize 'key))) (app from (? (compose not false?) keyv))] ...)
              (add-to 'jsexpr (hash) [list [list key keyv] ...] "can't happen" name-options)]
             [_ (eprintf "JSON value does not match ~a schema:\n ~a\n" 'name (jsexpr->string/ j))
                #false]))

         #; {Configuration -> ScribbleTable}
         (define t* 
           (for/list ([k key*] [c `((,is-a ...) ...)])
             (list (~a k) c)))
         (define [name->def] (fields->data-def 'name t*)))]))

#; {Symbol -> Symbol}
(define (normalize key)
  (string->symbol (string-downcase (~a key))))

#; {[Listof Any] -> [Option [Listof [List Symbol Any]]]}
(define (is-list-of-key-value-pairs key-value-pairs)
  (let loop ([key-value-pairs key-value-pairs] [h '()])
    (match key-value-pairs
      [(list) (reverse h)]
      [(list x) #false]
      [(list* k v key-value-pairs) (loop key-value-pairs (cons [list k v] h))])))

#; {Symbol Configuration [Listof [List Symbol Any]] [Listof Any] [Listof Symbol] -> Congiguration}
(define (add-to tag config key-value-pairs key-value-pairs0 name-options)
  (cond
    [(false? key-value-pairs)
     (error tag "key-value pair expected; given ~a" key-value-pairs0)]
    [else 
     (for/fold ([h config]) ([kv-pair key-value-pairs])
       (match-define [list k v] kv-pair)
       (unless (member k name-options)
         (error tag "configuration key expected, given ~a" k))
       (dict-set h k v))]))

;; ---------------------------------------------------------------------------------------------------
;; dealing with JSON

(module json racket
  (provide
   #; {Configuration [Listof Symbol] [Listof Symbol] [Listof (Any -> JSexpr)] -> JSexpr}
   config->jsexpr
   
   jsexpr->string/)

  (require (prefix-in old: json))
  
  (define (config->jsexpr c key* g-key* to*)
    (for/fold ([h (hash)]) ([k key*] [g-key g-key*] [to to*])
      (dict-set h k (to (dict-ref c g-key)))))
  
  (define (jsexpr->string/ content)
  (define r (regexp-match #px"8.10\\." (version)))
  (if r 
      (old:jsexpr->string content #:indent 4)
      (old:jsexpr->string content))))

(require 'json)

;; ---------------------------------------------------------------------------------------------------
;; rendering a configuration as a scribble documentation

(module scribble racket
  (provide
   #; {Symbol [NEListof [List String [Cons String [Listof String]]]] -> ScribbleTable}
   ;; render the object specification as a scribble verbatim block
   fields->data-def
   
   table? #;"for testing")

  (require Qwirkle/scribblings/shared)

  (define (fields->data-def name t*)
    (define the-defined (deftech (case-name name)))
    (define def-name    [list "A " the-defined (~a " is an object with " (length t*) " fields:")])
    (define definition  (splice-fields t* (blanks-needed t*) #true))
    (apply verbatim #:indent 4 (element-join (cons def-name definition) ",\n")))

  (define (case-name name)
    (define name* (string->list (~a name)))
    (define one (string-titlecase (string (first name*))))
    (~a one (apply string (rest name*)) 'Config))
  
  (define (blanks-needed t*)
    (define mx (apply max (map string-length (map first t*))))
    (λ (s) (white (make-string (- mx (string-length s)) #\_))))

  (define (splice-fields t* blanks [start #false])
    (match t*
      ['() '()]
      [(list [list field [cons def explanation]])
       (list (1-field field def explanation start (blanks field) #true))]
      [(cons [list field [cons def explanation]] x*)
       (cons (1-field field def explanation start (blanks field) #false) (splice-fields x* blanks))]))

  (define space  @tt|{   { }|)
  (define (1-field field def explanation start blanks end)
    (let* ([s (if (empty? explanation) '() (list " (" explanation ")"))]
           [s [list* (tt (~s field)) blanks " :    " (tech def) s]]
           [s (if start (cons space s) (cons @white[space] s))]
           [s (if end (append s (list "  } ")) s)])
      s))

  (define (white s) @element[(make-style #false (list 'hspace (color-property "white"))) s]))

(require 'scribble)

;; ---------------------------------------------------------------------------------------------------
(module+ test
  (provide  server-config->definition default-server-config)

  (require Qwirkle/Lib/check-message)
  (require rackunit)
  (require json)

  (define-configuration server
    (PORT            0 #:is-a "Natural" "between 10000 and 60000")
    (SERVER-TRIES    1 #:is-a "Natural")
    (SERVER-WAIT     2 #:is-a "PositiveReal")
    (WAIT-FOR-SIGNUP 3 #:is-a "Natural")
    (REF-SPEC        4 #:to-jsexpr (λ (x) x) #:from-jsexpr (λ (x) x) #:is-a "RefSpec")
    (QUIET           5 #:is-a "Boolean" ))
  
  (check-equal? [length server-options] 6)
  [check-equal? [contract? server-config/c] #true]
  [check-equal? (dict-ref default-server-config REF-SPEC) 4]
  [check-equal? (dict-ref (set-server-config default-server-config PORT 1 REF-SPEC 17) REF-SPEC) 17]

  (check-equal?
   (jsexpr->server-config (server-config->jsexpr default-server-config))
   default-server-config)

  (check-false
   (check-message
    "json" current-error-port #px"schema" 
    (jsexpr->server-config (dict-set (server-config->jsexpr default-server-config) 'A 11))))

  (check-true
   (jsexpr? (server-config->jsexpr (set-server-config default-server-config PORT 1 REF-SPEC 17))))

  [check-exn #px"key-value pair" (λ () (set-server-config default-server-config PORT 1 REF-SPEC))]

  (check-true (table? (server-config->definition))))

