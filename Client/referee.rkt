#lang racket

;; the proxy referee runs a player in the same context as a referee proper

(require (prefix-in json: (only-in json jsexpr->string jsexpr?)))

(define create-reply/c   (-> (or/c eof-object? json:jsexpr?) (or/c json:jsexpr? broken?)))
(define receiver/c       (-> create-reply/c any))
(define proxy-referee/c  (-> receiver/c custodian? (-> player/c any)))

;; the `proxy-referee` for a player is a function that
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

(provide
 proxy-referee/c
 
 (contract-out
  
  [create-proxy-referee
   #; (create-proxy-referee name port ip quiet? rm)
   #; (create-proxy-referee name port ip quiet? rm sender-routine)
   ;; the resulting proxy referee gets connected to the server with `name`
   ;; using the given proxy-referee; output to error is managed
   ;; the optional sender routine allows sending the name in a weird maner 
   (->* (string? port/c string? boolean? proxy-referee/c) (any/c) [-> any/c])]
  
  [connect-and-run
   #; {[-> ProxyReferee] Player OutputPort -> Void}
   ;; connect the Proxy to the server and the player, then start it up
   (-> (-> any/c) any/c output-port? any)]
  
  [default-proxy-ref-maker proxy-referee/c]
  
  (pick-referee
   ;; pick a proxy-referee given the string (usually the name of a player)
   (-> string? (or/c #false proxy-referee/c)))
  
  ;; for xtranslate only 
  [*referee-list (listof (list/c string? proxy-referee/c))]))

;                                                          
;                                                          
;                                  ;                       
;                                                          
;    ;;;;   ;;;    ;;;;  ;   ;   ;;;    ;;;;   ;;;    ;;;  
;    ;;  ; ;;  ;  ;; ;;  ;   ;     ;    ;;  ; ;;  ;  ;   ; 
;    ;     ;   ;; ;   ;  ;   ;     ;    ;     ;   ;; ;     
;    ;     ;;;;;; ;   ;  ;   ;     ;    ;     ;;;;;;  ;;;  
;    ;     ;      ;   ;  ;   ;     ;    ;     ;          ; 
;    ;     ;      ;; ;;  ;   ;     ;    ;     ;      ;   ; 
;    ;      ;;;;   ;;;;   ;;;;   ;;;;;  ;      ;;;;   ;;;  
;                     ;                                    
;                     ;                                    
;                     ;                                    

(require Qwirkle/Remote/define-dispatcher)

(require Qwirkle/Common/player-interface)
(require Qwirkle/Common/tiles)
(require Qwirkle/Lib/json)

(require (submod Qwirkle/Common/placement json))
(require (submod Qwirkle/Common/tiles json))
(require (submod Qwirkle/Referee/ref-state json))

(require SwDev/Testing/make-client)
(require SwDev/Testing/communication)

(require (for-syntax syntax/parse))
(require (for-syntax racket/format racket/string))

(module+ test
  (require (submod ".."))
  (require (submod Qwirkle/Common/placement json))
  
  (require (submod Qwirkle/Common/tiles examples))
  (require (submod Qwirkle/Player/strategies examples))
  (require (except-in (submod Qwirkle/Referee/ref-state examples) ForStudents/ Tests/))
  
  (require Qwirkle/Player/mechanics)
  (require Qwirkle/Player/strategies)
  
  (require SwDev/Lib/should-be-racket)
  (require rackunit))

;                                                                               
;                                                        ;                      
;                                                        ;            ;         
;                                                        ;                      
;   ;;;;    ;;;;   ;;;   ;   ;  ;   ;         ;;;;    ;;;; ;;;;;;   ;;;   ; ;;  
;   ;; ;;   ;;  ; ;; ;;   ; ;   ;   ;             ;  ;; ;; ;  ;  ;    ;   ;;  ; 
;   ;   ;   ;     ;   ;   ;;;    ; ;              ;  ;   ; ;  ;  ;    ;   ;   ; 
;   ;   ;   ;     ;   ;    ;     ; ;           ;;;;  ;   ; ;  ;  ;    ;   ;   ; 
;   ;   ;   ;     ;   ;   ;;;    ; ;          ;   ;  ;   ; ;  ;  ;    ;   ;   ; 
;   ;; ;;   ;     ;; ;;   ; ;    ;;           ;   ;  ;; ;; ;  ;  ;    ;   ;   ; 
;   ;;;;    ;      ;;;   ;   ;    ;            ;;;;   ;;;; ;  ;  ;  ;;;;; ;   ; 
;   ;                             ;                                             
;   ;                            ;                                              
;   ;                           ;;

(define *sender (λ (name ip) (send-message name ip)))
(define [(create-proxy-referee name port# ip quiet remote-referee (sender *sender))]
  (with-handlers ([exn:fail:network? (λ (xn) (eprintf "fail! ~a" name) (λ (_) 'failed-connection))])
    (define-values (receiver custodian)
      (connect-to-server-as-receiver ip port# #:init (λ (ip) (sender name ip))))
    (remote-referee receiver custodian)))

(define (connect-and-run proxy-referee 1player error-port)
  (parameterize ([prefix-with-spaces 5000]
                 [current-error-port error-port]
                 [trickle-output?    #true])
    ([proxy-referee] 1player)))

;; ---------------------------------------------------------------------------------------------------
(define *referee-list '())

(define (pick-referee x)
  (define r (assoc x *referee-list))
  (and r (second r)))

(define-syntax (def-and-add-rm stx)
  (syntax-parse stx
    [(_ name:id add?
        (~optional (~seq #:setup     ret-setup:id) #:defaults ([ret-setup #'void]))
        (~optional (~seq #:take-turn ret-tt:id)    #:defaults ([ret-tt    #'action*]))
        (~optional (~seq #:new-tiles ret-nt:id)    #:defaults ([ret-nt    #'void]))
        (~optional (~seq #:win       ret-win:id)   #:defaults ([ret-win   #'void])))
     #:do [[define name-as-string (string-replace (~a (syntax-e #'name)) "make-" "")]]
     #`(begin
         (define-remote-proxy-context name
           [[setup pk tiles]  ret-setup]
           [[take-turn pk]    ret-tt]
           [[new-tiles tiles] ret-nt]
           ;; when the last clause matches,
           ;; the dispatcher signals the end of the cycle by setting done? to #true
           [[win boolean]     ret-win])
         (when add?
           (set! *referee-list (cons [list #,name-as-string name] *referee-list))))]))

(def-and-add-rm default-proxy-ref-maker #false)
(def-and-add-rm make-nonSetup       #true  #:setup     non-json-void)
(def-and-add-rm make-illTT          #true  #:take-turn ill-formed-action)
(def-and-add-rm make-invTTBadString #true  #:take-turn invalid-action-bad-string)
(def-and-add-rm make-nonNewTiles    #true  #:new-tiles non-json-void)
(def-and-add-rm make-nonWin         #true  #:win       non-json-void)

(define (ill-formed-action->jsexpr x)
  (define y (json:jsexpr->string (action*->jsexpr x)))
  [broken (~a (make-string 4095 #\space) (substring y 0 (sub1 (string-length y))))])

(define (invalid-action-bad-string->jsexpr c)
  (match c
    [(or (== PASS) (== REPLACEMENT)) (action->jsexpr c)]
    [(cons p others) (cons (ill-formed-placement p) (placements->jsexpr others))]))

(define (ill-formed-placement p)
  (match (action->jsexpr p)
    [(hash-table [(== COORDINATE) c] [(== ATILE) t])
     (define coordinate (string->symbol (substring (~a COORDINATE) 1 5)))
     (hash coordinate c ATILE t)])) 

(define (non-json-void->jsexpr a)
  [broken "void"])


;                                     
;                                     
;     ;                    ;          
;     ;                    ;          
;   ;;;;;   ;;;    ;;;   ;;;;;   ;;;  
;     ;    ;;  ;  ;   ;    ;    ;   ; 
;     ;    ;   ;; ;        ;    ;     
;     ;    ;;;;;;  ;;;     ;     ;;;  
;     ;    ;          ;    ;        ; 
;     ;    ;      ;   ;    ;    ;   ; 
;     ;;;   ;;;;   ;;;     ;;;   ;;;  
;                                     
;                                     
;                                     

(module+ test ;; test individual remote calls 
  (define plr1 (create-player "bye" dag-strategy))
  (define jspecial-state (pk->jsexpr info-special-state))
  (define jstarter-state (pk->jsexpr info-starter-state))
  (define jplmt0 (action*->jsexpr info-special-place*))
  (define jstarter-tile* (tiles->jsexpr starter-tile*))

  (define [(mk-rm f) escape j] (f [mk-escape-dispatcher escape j] (make-custodian)))
  (define [[mk-escape-dispatcher escape j] dispatcher] (escape (dispatcher j)))

  #; {JSexpr -> JSexpr}
  (define (run j [rm0 #false])
    (parameterize ([current-error-port (open-output-string)])
      (let/cc escape
        (define rm (if rm0 [rm0 escape j] [[mk-rm default-proxy-ref-maker] escape j]))
        (rm plr1))))
  
  (check-false [run eof])
  (check-equal? (run `["setup" [,jspecial-state ,jstarter-tile*]]) "void" "setup")
  (check-equal? (run `["take-turn" [,jspecial-state]]) jplmt0 "tt placements")
  (check-equal? (run `["take-turn" [,jstarter-state]]) (action*->jsexpr REPLACEMENT) "tt replacement")
  (check-equal? (run `["new-tiles" [,jstarter-tile*]]) "void" "nt")
  (check-equal? (run `["win" [#t]]) "void" "win tt")
  (check-equal? (run `["win" [#f]]) "void" "win ff"))

;; ---------------------------------------------------------------------------------------------------
(module+ test ;; winning call? 
  (define rm (default-proxy-ref-maker (λ (f) (f `["win" [#true]])) (make-custodian)))
  (check-equal? (dev/null (rm plr1)) #true))

;; ---------------------------------------------------------------------------------------------------
(module+ test ;; check an entire sequence of calls, ending in a winning call 
  (define b*
    `[["setup" [,jspecial-state ,jstarter-tile*]]
      ["take-turn" [,jspecial-state]]
      ["new-tiles" [,jstarter-tile*]]
      ["win" [#true]]])
  (define chop! (λ (f) (begin0 (f (first b*)) (set! b* (rest b*)))))
  (define rm+ (default-proxy-ref-maker chop! (make-custodian)))
  (check-equal? (dev/null (rm+ (create-player "Al" dag-strategy))) #t))

;; ---------------------------------------------------------------------------------------------------
(module+ test ;; checks that the proxy-referee shuts down the given custodian if the JSON is bad
  (check-true
   (let ([cust (make-custodian)])
     (parameterize ([current-custodian cust])
       (define port (open-input-file "referee.rkt"))
       (dev/null ((default-proxy-ref-maker (λ (f) (f `[0 [#t]])) cust) plr1))
       (port-closed? port)))))

;; ---------------------------------------------------------------------------------------------------
(module+ test ;; test all the mostly broken proxy-referees 
  (define nonSetup (pick-referee "nonSetup"))
  (check-pred broken? [run `["setup" [,jspecial-state ,jstarter-tile*]] [mk-rm nonSetup]])

  (define illTT (pick-referee "illTT"))
  (check-pred broken? (run `["take-turn" [,jspecial-state]] [mk-rm illTT]))
  
  (define invTTBadString (pick-referee "invTTBadString"))
  (check-false (action*? (run `["take-turn" [,jspecial-state]] [mk-rm invTTBadString])))
  (check-equal? (run `["take-turn" [,jstarter-state]] [mk-rm invTTBadString]) JREPLACEMENT)
  
  (define non-nt (pick-referee "nonNewTiles"))
  (check-pred broken? (run `["new-tiles" [,jstarter-tile*]] [mk-rm non-nt]))
  
  (define nonWin (pick-referee "nonWin"))
  (check-pred broken? (run `["win" [#true]] [mk-rm nonWin])))
