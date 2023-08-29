#lang racket

;; a client that signs up some players with a server at a given IP address and port,
;; and that then participate in a distributed games 

                        
(require Qwirkle/Common/player-interface)
(require (only-in SwDev/Testing/make-client port/c))

(provide
 WAIT-BETWEEN-THREADS ;; s between launching threads 

 (contract-out
  [client 
   #; (client players ip port# wait?)
   ;; runs each p in `players` as a client that connects to a server at `ip` on `port#`
   ;; if `wait?` the main thread waits for all of clients -- NEEDED FOR INDEPENDENT RUNS
   ;; of the client in a shell process  (why?)
   ;; BY DEFAULT: 
   ;; `ip` is localhost
   ;; `port#` is a common port
   ;; `wait?` is #false
   ;; `quiet` is #true 
   (->i ([players (listof player/c)]) ([port# port/c] [wait? boolean?] [ip string?] #:quiet [quiet any/c]
         #:remote-manager [rm any/c])
        (r any/c))]))

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
(require (except-in SwDev/Testing/make-client port/c))
(require SwDev/Testing/communication)

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

(define LOCAL "127.0.0.1")
(define PORT0 45678)
(define WAIT-BETWEEN-THREADS 3)

(define (client players (port PORT0) (wait? #false) (ip LOCAL)
                #:quiet (quiet #true)
                #:remote-manager (rm make-remote-manager))
  (define client-threads (run-clients players port ip quiet rm))
  (when wait?
    (wait-for-all client-threads)
    (displayln "all done")))

#; {type ChanneledThreads = [Listof [List Channel Thread]]}

#; {[Listof Player] Port IP Boolean RemoteReferee -> ChanneledThreads}
(define (run-clients players port ip quiet rr)
  (for/list ((1player players))
    (define name (send 1player name))
    (define referee
      (with-handlers ([exn:fail:network? [make-failed name]])
        (define-values (receiver cust) (connect-to-server-as-receiver ip port #:init (sign-up name)))
        (rr receiver cust)))
    (sleep WAIT-BETWEEN-THREADS) ;; to make determinism extremely likely
    (thread (make-thread name 1player referee quiet))))

#; {String Player RemoteRef Boolean -> (-> Any)}
(define [(make-thread name 1player referee quiet)]
  (parameterize ([prefix-with-spaces 5000]
                 [current-error-port (if quiet (open-output-string) (current-error-port))]
                 [trickle-output?    #true])
    (referee 1player)))

#; {String -> OutputPort -> Void}
(define (sign-up name) (λ (ip) (send-message name ip)))

#; {String -> (Exn -> Symbol)}
(define ([make-failed name] xn)
  (eprintf "client ~a failed to connect\n" name)
  (λ (_) 'failed-connection))

#; {ChanneledThreads -> Void}
;; display the results 
(define (wait-for-all client-threads)
  (when (cons? client-threads)
    (define removes-itself
      (for/list ((dp client-threads))
        (handle-evt dp (λ (r) (wait-for-all (remq dp client-threads))))))
    (apply sync removes-itself)))
