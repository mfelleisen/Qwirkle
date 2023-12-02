#lang racket

;; create a remote proxy-context from types of the player's methods 

;                                                          
;                                                          
;                                                          
;                                             ;            
;                                             ;            
;    ;;;;   ;   ;;  ; ;;;    ;;;;    ;;;;;  ;;;;;;   ;;;;  
;    ;  ;;   ;  ;   ;;  ;   ;;  ;;   ;;       ;     ;    ; 
;   ;    ;    ;;    ;    ;  ;    ;   ;        ;     ;      
;   ;;;;;;    ;;    ;    ;  ;    ;   ;        ;     ;;;    
;   ;         ;;    ;    ;  ;    ;   ;        ;        ;;; 
;   ;         ;;    ;    ;  ;    ;   ;        ;          ; 
;    ;       ;  ;   ;;  ;   ;;  ;;   ;        ;     ;    ; 
;    ;;;;;  ;    ;  ; ;;;    ;;;;    ;         ;;;   ;;;;  
;                   ;                                      
;                   ;                                      
;                   ;                                      
;                                                          

(provide
 #; [define-remote-proxy-context f c:dispatcher-clause ... c:dispatcher-clause]
 ;; where a dispatcher-clause has the shape 
 #; [(method:id arg:id ...) return:id]
 ;; introduces a remote proxy-context named `f`
 ;; when the last clause matches, the proxy-context shuts down

 ;; The syntax assumes that functions called `jsexpr->arg` and `arg?` are defined in the context. 
 ;; These functions translate JSexprs to Racket values for the method.
 ;; Converserly, it also assumes that a function called `return->jsexpr` exists in the context.
 ;; These functions translate Racket values to JSexpres, which the receiver can send back to a server.

 define-remote-proxy-context)

#; (define create-reply/c   (-> (or/c eof-object? jsexpr?) jsexpr?))
#; (define receiver/c       (-> create-reply/c any))
#; (define remote-proxy-context/c (-> receiver/c custodian? (-> proxy-context-player/c any)))

;; the `remote-proxy-context` for a player is a function that
;; -- repeatedly receives JSexpr and turns them into arguments so that it can 
;; -- call the appropriate method in the given player and then
;; -- turn the result into a JSexpr that can be sent back 
;; 
;; the `receiver` is supposed to be a function that handles the side of a remote-call interaction 
;; -- its argument is called on the received JSON or EOF turned into JSxpr or EOF
;;    and its result is what the `receiver` turns back into a remote reply
;;    [I have developed a library that sets up both a sender and a receiver.]
;;    
;; `create-reply` is the best name I could come up with for the argument of the `receiver`

;                                                                                                  
;                                                                                                  
;        ;                                       ;                             ;                   
;        ;                                       ;                             ;                   
;        ;                                       ;                                                 
;    ;;; ;   ;;;;   ; ;;;    ;;;;   ; ;;;    ;;; ;   ;;;;   ; ;;;     ;;;    ;;;     ;;;;    ;;;;  
;    ;  ;;   ;  ;;  ;;  ;    ;  ;;  ;;   ;   ;  ;;   ;  ;;  ;;   ;   ;   ;     ;     ;  ;;  ;    ; 
;   ;    ;  ;    ;  ;    ;  ;    ;  ;    ;  ;    ;  ;    ;  ;    ;  ;          ;    ;    ;  ;      
;   ;    ;  ;;;;;;  ;    ;  ;;;;;;  ;    ;  ;    ;  ;;;;;;  ;    ;  ;          ;    ;;;;;;  ;;;    
;   ;    ;  ;       ;    ;  ;       ;    ;  ;    ;  ;       ;    ;  ;          ;    ;          ;;; 
;   ;    ;  ;       ;    ;  ;       ;    ;  ;    ;  ;       ;    ;  ;          ;    ;            ; 
;    ;  ;;   ;      ;;  ;    ;      ;    ;   ;  ;;   ;      ;    ;   ;   ;     ;     ;      ;    ; 
;    ;;; ;   ;;;;;  ; ;;;    ;;;;;  ;    ;   ;;; ;   ;;;;;  ;    ;    ;;;   ;;;;;;;  ;;;;;   ;;;;  
;                   ;                                                                              
;                   ;                                                                              
;                   ;                                                                              
;                                                                                                  

(require SwDev/Testing/communication)
(require (for-syntax syntax/parse))
(require (for-syntax (only-in racket ~a)))

;                                                  
;                                                  
;                                                  
;                                     ;            
;                                     ;            
;    ;;;;;   ;;;;   ;;;;;;   ;;;;   ;;;;;;   ;;;;  
;    ;;      ;  ;;  ;  ;  ; ;;  ;;    ;      ;  ;; 
;    ;      ;    ;  ;  ;  ; ;    ;    ;     ;    ; 
;    ;      ;;;;;;  ;  ;  ; ;    ;    ;     ;;;;;; 
;    ;      ;       ;  ;  ; ;    ;    ;     ;      
;    ;      ;       ;  ;  ; ;    ;    ;     ;      
;    ;       ;      ;  ;  ; ;;  ;;    ;      ;     
;    ;       ;;;;;  ;  ;  ;  ;;;;      ;;;   ;;;;; 
;                                                  
;                                                  
;                                                  
;                                                  

(define (([make-remote-proxy-context make-dispatcher] receiver custodian) player)
  (define done? (box (gensym)))
  (define dispatcher (make-dispatcher player done?))
  (parameterize ([io-time-out 1000]
                 [error-print-width 1000])
    (let loop ()
      (with-handlers ([void (λ (xn)
                                (eprintf "remote proxy-context caught exn:\n ~a\n" (exn-message xn))
                                (custodian-shutdown-all custodian))])
        (receiver dispatcher)
        (unless (boolean? (unbox done?)) (loop)))))
  (unbox done?))

;                                                  
;                                                  
;                                                  
;                             ;                    
;                             ;                    
;    ;;;;   ;    ;  ; ;;;   ;;;;;;    ;;;   ;   ;; 
;   ;    ;   ;   ;  ;;   ;    ;      ;   ;   ;  ;  
;   ;        ;  ;   ;    ;    ;          ;    ;;   
;   ;;;      ;  ;   ;    ;    ;      ;;;;;    ;;   
;      ;;;    ; ;   ;    ;    ;     ;    ;    ;;   
;        ;    ;;    ;    ;    ;     ;    ;    ;;   
;   ;    ;    ;;    ;    ;    ;     ;   ;;   ;  ;  
;    ;;;;      ;    ;    ;     ;;;   ;;; ;  ;    ; 
;              ;                                   
;             ;                                    
;            ;;                                    
;                                                  

(begin-for-syntax
  (define-syntax-class dispatcher-clause
    (pattern [(method:id arg:id ...) return:id])))

(define-syntax (define-remote-proxy-context stx)
  (syntax-parse stx
    [[_ f c:dispatcher-clause ...]
     #:with ((pattern right-hand-side) ...) (map (match-clause #'player) (syntax->list #'[ c ...]))
     #:with (pat1 pat ... patN) #'(pattern ...)
     #:with (rhs1 rhs ... rhsN) #'(right-hand-side ...)
     
     #'(define f
         (let ()
           ;; this player is referenced in rhs1, rhs, ... rhsN
           (define ([make-dispatcher player done?] j) 
             (match j
               [(? eof-object?) #false]
               [pat1 (set-box! done? 'go) rhs1]
               [pat  rhs]
               ...
               [patN (set-box! done? #true) rhsN]
               [ill (error 'dispatcher "server sent inappropriate message: ~e" ill)]))
           (make-remote-proxy-context make-dispatcher)))]))

(define-for-syntax ((match-clause player) stx)
  (syntax-parse stx
    [[(method:id arg:id ...) return:id]
     #:with (names pat) (create-pattern #'(method arg ...))
     #:with right-hand-side (create-rhs player #'return #'method #'names)
     (list #'pat #'right-hand-side)]))

(define-for-syntax (create-pattern stx)
  (syntax-parse stx
    [(method:id arg:id ...)
     #:with method-str    (symbol->string (syntax-e #'method))
     #:with (x ...)       (generate-temporaries #'(arg ...))
     #:with (arg? ...)    (for/list ([a (syntax->list #'(arg ...))]) (predicate-name a))
     #:with (p-arg ...)   (for/list ([a (syntax->list #'(arg ...))]) (parse-name a))
     #:with (arg-pat ...) #'[(app p-arg (? arg? x)) ...]
     (list #'(x ...) #'`[method-str [,arg-pat ...]])]))

(define-for-syntax (create-rhs player return method names)
  #`(#,(jsexpr-name return) (send #,player #,method #,@names)))

(define-for-syntax (parse-name id)
  (datum->syntax id (string->symbol (~a "jsexpr->" (syntax-e id))) id id))

(define-for-syntax (predicate-name id)
  (datum->syntax id (string->symbol (~a (syntax-e id) "?")) id id))

(define-for-syntax (jsexpr-name id)
  (datum->syntax id (string->symbol (~a (syntax-e id) "->jsexpr")) id id))
