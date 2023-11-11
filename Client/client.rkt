#lang racket

;; a client that signs up some players with a server at a given IP address and port,
;; and that then participate in a distributed games 

                        
(require Qwirkle/Common/player-interface)
(require (only-in SwDev/Testing/make-client port/c))

(provide
 ;; for homework
 PLAYERS PORT HOST WAIT QUIET
 WAIT-BETWEEN-THREADS ;; s between launching threads
 
 client-config->definition

 client-config->jsexpr
 jsexpr->client-config

 (contract-out

  [default-client-config client-config/c]

  [set-client-config (-> client-config/c any/c ... client-config/c)]
  
  [clients 
   #; (client players ip port# wait? #:quiet q #:remote remote-manager #:baddies lo-bad-clients)
   ;; runs each p in `players` as a client that connects to a server at `ip` on `port#`
   ;; if `wait?` the main thread waits for all of clients -- NEEDED FOR INDEPENDENT RUNS
   ;; of `clients` in a shell process  (why?)
   ;; 
   ;; players with special names -- see pick at Client/referee -- get JSON-bad remote-referees
   (->i ([config  client-config/c])         ;; BY DEFAULT: 
        ([wait? boolean?]                   ;; `wait?` is #false
         #:baddies [b (listof procedure?)]) ;; `b` is '[]
        (r any/c))]
    
  (make-client-for-name-sender
   #; (make-client-for-name-sender ns ip port# wait? #:quiet q)
   (->i ([send-name (-> output-port? any)])  ;; BY DEFAULT: 
        ([port# port/c]                     ;; `port#` is a common port
         [ip string?]                       ;; `ip` is LOCALHOST
         #:quiet [quiet any/c])             ;; `quiet` is #true
         (r any/c)))))

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

(require Qwirkle/Client/referee)
(require (submod Qwirkle/Player/mechanics json))
(require Qwirkle/Lib/configuration)
(require (except-in SwDev/Testing/make-client port/c))
(require SwDev/Testing/communication)

;                                                                                             
;                           ;;                                                                
;                          ;       ;                                 ;       ;                
;                          ;                                         ;                        
;    ;;;    ;;;   ; ;;   ;;;;;   ;;;    ;;;;  ;   ;   ;;;;  ;;;;   ;;;;;   ;;;    ;;;   ; ;;  
;   ;;  ;  ;; ;;  ;;  ;    ;       ;   ;;  ;  ;   ;   ;;  ;     ;    ;       ;   ;; ;;  ;;  ; 
;   ;      ;   ;  ;   ;    ;       ;   ;   ;  ;   ;   ;         ;    ;       ;   ;   ;  ;   ; 
;   ;      ;   ;  ;   ;    ;       ;   ;   ;  ;   ;   ;      ;;;;    ;       ;   ;   ;  ;   ; 
;   ;      ;   ;  ;   ;    ;       ;   ;   ;  ;   ;   ;     ;   ;    ;       ;   ;   ;  ;   ; 
;   ;;     ;; ;;  ;   ;    ;       ;   ;; ;;  ;   ;   ;     ;   ;    ;       ;   ;; ;;  ;   ; 
;    ;;;;   ;;;   ;   ;    ;     ;;;;;  ;;;;   ;;;;   ;      ;;;;    ;;;   ;;;;;  ;;;   ;   ; 
;                                          ;                                                  
;                                       ;  ;                                                  
;                                        ;;                                                   

(define LOCAL "127.0.0.1")
(define PORT0 45678)
(define WAIT-BETWEEN-THREADS 2)

(define-configuration client
  [PORT PORT0 #:is-a "Natural" "between 10000 and 60000"] ;; `port#` is a common port
  [HOST LOCAL #:is-a "String" "either an IP address or a domain name"] ;; `ip` is LOCALHOST
  [WAIT WAIT-BETWEEN-THREADS #:is-a "Natural" "less than 10s"]
  [QUIET #true #:is-a "Boolean"]
  [PLAYERS
   '()
   #:to-jsexpr   player*->jsexpr
   #:from-jsexpr (λ (x) (jsexpr->player* x #:loops #true #:cheating #true))
   #:is-a        "JActorsB"])

;                                                  
;                                                  
;            ;;;       ;                           
;              ;                              ;    
;              ;                              ;    
;     ;;;      ;     ;;;     ;;;;   ; ;;;   ;;;;;; 
;    ;   ;     ;       ;    ;    ;  ;;   ;    ;    
;   ;          ;       ;    ;;;;;;  ;    ;    ;    
;   ;          ;       ;    ;       ;    ;    ;    
;   ;          ;       ;    ;       ;    ;    ;    
;    ;   ;     ;       ;    ;;   ;  ;    ;    ;    
;     ;;;       ;;;  ;;;;;   ;;;;;  ;    ;     ;;; 
;                                                  
;                                                  
;                                                  
;                                                  

(define (clients cc (wait? #f) #:baddies [bad-clients '()])
  (define players (dict-ref cc PLAYERS))
  (define ip (dict-ref cc HOST))
  (define p# (dict-ref cc PORT))
  (define quiet (dict-ref cc QUIET))

  (define clients-for-players (map (make-client-for-player p# ip quiet) players))
  (define running-clients     (launch-clients (append clients-for-players bad-clients)))
  (when wait?
    (wait-for-all running-clients)
    (displayln "all done")))

#; {type Client = [-> Void]}
;; a client connects to the server as a callee (receiver) and then runs a thread that deals with RMCs 

;; ---------------------------------------------------------------------------------------------------
#; {[Listof Client] -> [Listof Thread]}
(define (launch-clients clients-for-players)
  (for/list ([1-client clients-for-players])
    (sleep WAIT-BETWEEN-THREADS) ;; to make determinism extremely likely
    (thread 1-client)))

;; ---------------------------------------------------------------------------------------------------
#; {[InputPort -> Void] PortNumber IPAddress Boolean -> Client}
;; the resulting client sends a name and then performs no work; but the name-sending thunk can loop
;; `ns` is a name sender: it consumes an input port and `send-message`s a string to it 
(define (make-client-for-name-sender ns (port# PORT0) (ip LOCAL) #:quiet (quiet #false)) 
  (define referee-maker (make-referee-maker "bad client" port# ip quiet make-remote-manager ns))
  (λ () [referee-maker] 'fake-client-is-done))

;; ---------------------------------------------------------------------------------------------------
#; {PortNumber IPAddress Boolean -> Player -> Client}
;; the connection to the server musy happens "at the same time" as the player becomes a thread
;; because the server may immediately start the game as as sufficient number of players connected 
(define [(make-client-for-player port# ip quiet) 1player]
  (define player-name   (send 1player name))
  (define remote-ref    (or (pick-referee player-name) make-remote-manager))
  (define referee-maker (make-referee-maker (send 1player name) port# ip quiet remote-ref))
  (define error-port (if quiet (open-output-string) (current-error-port)))
  (λ () (connect-and-run-together referee-maker 1player error-port)))

#; {String PortNumber IPAddress Boolean RemoetManager -> [-> ProxyReferee]}
;; the resulting proxy referee gets connected to the server with `name`
(define [(make-referee-maker name port# ip quiet rm (sender (λ (ip) (send-message name ip))))]
  (with-handlers ([exn:fail:network? (λ (xn) (eprintf "fail! ~a" name) (λ (_) 'failed-connection))])
    (define-values (r c) (connect-to-server-as-receiver ip port# #:init sender))
    (rm r c)))

#; {[-> ProxyReferee] Player OutputPort -> Void}
(define (connect-and-run-together referee-maker 1player error-port)
  (parameterize ([prefix-with-spaces 5000]
                 [current-error-port error-port]
                 [trickle-output?    #true])
    ([referee-maker] 1player)))

;; ---------------------------------------------------------------------------------------------------
#; {[Listof Thread] -> Void}
;; display the results 
(define (wait-for-all client-threads)
  (when (cons? client-threads)
    (define removes-itself
      (for/list ((dp client-threads))
        (handle-evt dp (λ (r) (wait-for-all (remq dp client-threads))))))
    (apply sync removes-itself)))
