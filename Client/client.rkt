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
   (->i ([name string?] [send-name (-> string? output-port? any)])  ;; BY DEFAULT: 
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
;          ;;;       ;                   ;          
;            ;                           ;          
;    ;;;     ;     ;;;    ;;;   ; ;;   ;;;;;   ;;;  
;   ;;  ;    ;       ;   ;;  ;  ;;  ;    ;    ;   ; 
;   ;        ;       ;   ;   ;; ;   ;    ;    ;     
;   ;        ;       ;   ;;;;;; ;   ;    ;     ;;;  
;   ;        ;       ;   ;      ;   ;    ;        ; 
;   ;;       ;       ;   ;      ;   ;    ;    ;   ; 
;    ;;;;     ;;   ;;;;;  ;;;;  ;   ;    ;;;   ;;;  
;                                                   
;                                                   
;                                                   

;; ---------------------------------------------------------------------------------------------------
(define (clients cc (wait? #f) #:baddies [bad-clients '()])
  (define clients-for-players (map (make-client-for-player cc) (dict-ref cc PLAYERS)))
  (define running-clients     (launch-all-clients (append clients-for-players bad-clients)))
  (when wait?
    (wait-for-all running-clients)
    (displayln "all done")))

;; ---------------------------------------------------------------------------------------------------
#; {[Listof Client] -> [Listof Thread]}
(define (launch-all-clients clients-for-players)
  (for/list ([1-client clients-for-players])
    (sleep WAIT-BETWEEN-THREADS) ;; to make determinism extremely likely
    (launch 1-client)))

;; ---------------------------------------------------------------------------------------------------
#; {[Listof Thread] -> Void}
;; display the results 
(define (wait-for-all client-threads)
  (when (cons? client-threads)
    (define removes-itself
      (for/list ((dp client-threads))
        (handle-evt dp (λ (r) (wait-for-all (remq dp client-threads))))))
    (apply sync removes-itself)))

;                                                   
;                                                   
;     ;;     ;;;  ;;;       ;                   ;   
;    ; ;    ;   ;   ;                           ;   
;      ;   ;        ;     ;;;    ;;;   ; ;;   ;;;;; 
;      ;   ;        ;       ;   ;;  ;  ;;  ;    ;   
;      ;   ;        ;       ;   ;   ;; ;   ;    ;   
;      ;   ;        ;       ;   ;;;;;; ;   ;    ;   
;      ;   ;        ;       ;   ;      ;   ;    ;   
;      ;    ;   ;   ;       ;   ;      ;   ;    ;   
;    ;;;;;   ;;;     ;;   ;;;;;  ;;;;  ;   ;    ;;; 
;                                                   
;                                                   
;                                                   

#; {type Client = [-> Void]}
;; a client connects to the server as a callee (receiver) and then runs a thread that deals with RMCs

#; {Client -> Thread}
(define (launch 1-client)
  (thread 1-client))

;; ---------------------------------------------------------------------------------------------------
#; {PortNumber IPAddress Boolean -> Player -> Client}
;; the connection to the server musy happens "at the same time" as the player becomes a thread
;; because the server may immediately start the game as as sufficient number of players connected 
(define [(make-client-for-player cc) 1player]
  (define ip    (dict-ref cc HOST))
  (define port# (dict-ref cc PORT))
  (define quiet (dict-ref cc QUIET))
  
  (define make-proxy-ref (or (pick-referee (send 1player name)) default-proxy-ref-maker))
  (define proxy-referee  (create-proxy-referee (send 1player name) port# ip quiet make-proxy-ref))
  (define error-port     (if quiet (open-output-string) (current-error-port)))
  (λ () (connect-and-run proxy-referee 1player error-port)))

;                                                                        
;                     ;                                                  
;   ;;;;              ;           ;;;  ;;;       ;                   ;   
;   ;   ;             ;          ;   ;   ;                           ;   
;   ;   ;  ;;;;    ;;;;         ;        ;     ;;;    ;;;   ; ;;   ;;;;; 
;   ;   ;      ;  ;; ;;         ;        ;       ;   ;;  ;  ;;  ;    ;   
;   ;;;;       ;  ;   ;         ;        ;       ;   ;   ;; ;   ;    ;   
;   ;   ;   ;;;;  ;   ;         ;        ;       ;   ;;;;;; ;   ;    ;   
;   ;   ;  ;   ;  ;   ;         ;        ;       ;   ;      ;   ;    ;   
;   ;   ;  ;   ;  ;; ;;          ;   ;   ;       ;   ;      ;   ;    ;   
;   ;;;;    ;;;;   ;;;;           ;;;     ;;   ;;;;;  ;;;;  ;   ;    ;;; 
;                                                                        
;                                                                        
;                                                                        

#; {[InputPort -> Void] PortNumber IPAddress Boolean -> Client}
;; the resulting client sends a name and then performs no work; but the name-sending thunk can loop
;; `ns` is a name sender: it consumes an input port and `send-message`s a string to it 
(define (make-client-for-name-sender name name-sender (port# PORT0) (ip LOCAL) #:quiet (quiet #false))
  (define proxy-ref (create-proxy-referee name port# ip quiet default-proxy-ref-maker name-sender))
  (λ () [proxy-ref] 'fake-client-is-done))

