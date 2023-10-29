#lang racket

;; a server that signs up players over TCP and runs a game 

(require (submod (lib "Qwirkle/scribblings/qwirkle.scrbl") spec))

;; ---------------------------------------------------------------------------------------------------
(provide
 ascending-by-age
 descending-by-age
 
 (contract-out
  (default-server-config server-config/c)
  
  [server
   ;; returns the list of winners and cheaters/failures

   ;; get at least `MIN-PLAYERS while running at most `SERVER-TRIES` wait periods of `SERVER-WAIT`s
   ;; but as soon as `MAX-PLAYERS` have signed up, stop accepting more players and run the game
   ;; if at the end of a wait period `MIN-PLAYERS` are around, also launch the game and stop signups
   ;; otherwise, shut down.
   ;;   (a production server would have a "wait in line" queue for late comers; and it would restart.)
   ;; 
   ;; runs a referee on the players that signed up properly port# plus the house players (if any) 
   (->i ([confg    server-config/c])
        ([ordering (-> list? list?)]
         [plyrs    list?]
         #:result (return-results-or-void (-> list? any/c)))
        (result any/c))]))

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

(require Qwirkle/Common/player-interface) ;; type Player 
(require Qwirkle/Server/player)
(require Qwirkle/Referee/referee)

(require Qwirkle/Lib/configuration)

(require SwDev/Lib/hash-contract)
(require SwDev/Testing/communication)

(module+ examples
  (require (submod ".."))
  (require Qwirkle/Client/client)
  (require (prefix-in i: Qwirkle/Common/state-of-player))
  (require Qwirkle/Player/mechanics)
  (require Qwirkle/Player/strategies)
  (require (submod Qwirkle/Referee/referee examples))
  (require (submod Qwirkle/Referee/ref-state json))
  (require rackunit))

(module+ test

  (require Qwirkle/Client/client)
  (require (prefix-in i: Qwirkle/Common/state-of-player))
  (require Qwirkle/Player/mechanics)
  (require Qwirkle/Player/strategies)
  (require (submod Qwirkle/Referee/referee examples))
  (require (submod Qwirkle/Referee/ref-state json))
  
  (require (submod ".."))
  (require (submod ".." examples))
  (require (submod Qwirkle/Referee/referee examples))
  (require SwDev/Lib/list)
  (require rackunit))

;                                                          
;                                                          
;                              ;;;     ;                   
;                             ;        ;                   
;                             ;                            
;     ;;;    ;;;;   ; ;;;   ;;;;;;   ;;;     ;;; ;   ;;;;  
;    ;   ;  ;;  ;;  ;;   ;    ;        ;     ;  ;;  ;    ; 
;   ;       ;    ;  ;    ;    ;        ;    ;    ;  ;      
;   ;       ;    ;  ;    ;    ;        ;    ;    ;  ;;;    
;   ;       ;    ;  ;    ;    ;        ;    ;    ;     ;;; 
;   ;       ;    ;  ;    ;    ;        ;    ;    ;       ; 
;    ;   ;  ;;  ;;  ;    ;    ;        ;     ;  ;;  ;    ; 
;     ;;;    ;;;;   ;    ;    ;     ;;;;;;;  ;;; ;   ;;;;  
;                                                ;         
;                                            ;  ;;         
;                                             ;;;          
;                                                          

(define PORT0 45678)

(define-configuration server
  (PORT            PORT0)
  (SERVER-TRIES    1)
  (SERVER-WAIT     20)
  (WAIT-FOR-SIGNUP 2)
  (REF-SPEC        4 #:jsexpr (λ (x) 0))
  (QUIET           #true))

;                                            
;                                            
;                                            
;                                            
;    ;;;    ;;;    ;;;;  ;   ;   ;;;    ;;;; 
;   ;   ;  ;;  ;   ;;  ; ;   ;  ;;  ;   ;;  ;
;   ;      ;   ;;  ;      ; ;   ;   ;;  ;    
;    ;;;   ;;;;;;  ;      ; ;   ;;;;;;  ;    
;       ;  ;       ;      ; ;   ;       ;    
;   ;   ;  ;       ;       ;    ;       ;    
;    ;;;    ;;;;   ;       ;     ;;;;   ;    
;                                            
;                                            
;                                            

(define LOCAL     "127.0.0.1")
(define MAX-TCP   30)
(define REOPEN    #true)
(define DEFAULT-RESULT '[[] []])

(define test-run?  (make-parameter #false)) ;; [Channel [Listof N]] when used 
(define MIN-ERROR  "server did not sign up enough (~a) players")

;; IMPLEMENTATION 
;; create separate thread that signs up one player at time
;; this thread sends a list of players to the main thread as specified
;; it also waits for a question from the main thread (at the end of a wait period)
;;   when this question arrives, it stops running if `MIN-PLAYERS` have signed up
;;   otherwise it returns `#false` to indicate it wishes to sign up more players 

(define ascending-by-age values) ;; youngest first 
(define descending-by-age reverse) ;; oldest first 

(define (server config [age-ordering ascending-by-age] [house-players '()] #:result (show void))
  ;; set up custodian so `server` can clean up all threads, TCP ports, etc. -- in case it is re-used
  (parameterize ([current-custodian (make-custodian)])
    (define players (wait-for-players house-players config))
    (begin0
      (cond
        [(empty? players) (send-message DEFAULT-RESULT) (show DEFAULT-RESULT)]
        [(test-run?) => (λ (result) (channel-put result (age-ordering players)) DEFAULT-RESULT)]
        [else (configure-and-run-referee (age-ordering players) config show)])
      (custodian-shutdown-all (current-custodian)))))

#; {[Listof Player] ImmutableHash -> [List [Listof Player] [Listof Player]]}
(define (configure-and-run-referee players config optionally-return-result)
  (define result
    (with-handlers ([exn:fail? (λ (n) (eprintf "ref failed: ~a\n" (exn-message n)) DEFAULT-RESULT)])
      (referee/config (dict-ref config REF-SPEC '[]) players)))
  (send-message result)
  (optionally-return-result result))

#; {[Listof Player] ServerConfig -> [Listof Player]}
;; collect list of playaers in reverse order of sign-up [youngest first]
;; it spawns a thread to manage time and number of tries in parallel 
(define (wait-for-players house-players config)
  (define max-time    (dict-ref config SERVER-WAIT))
  (define max-tries   (dict-ref config SERVER-TRIES))
  (define communicate-with-sign-up (make-channel))
  (thread (sign-up-players communicate-with-sign-up house-players config))
  (let loop ([n max-tries])
    (cond
      [(zero? n) '()]
      [(sync/timeout max-time communicate-with-sign-up) => identity] 
      [else
       (channel-put communicate-with-sign-up "are there at least then min# of players signed up")
       (cond
         [(channel-get communicate-with-sign-up) => values]
         [else (loop (- n 1))])])))

#; {[Channel String] [Listof Player] ServerConfig -> (- Void)}
;; create a process function for signing up players
;; stops if `MAX-PLAYERS` have signed up _or_
;;       if `wait-for-players` requests players and `MIN-PLAYERS` have signed up
(define [(sign-up-players send-players house-players config)]
  (let/ec return
    (define listener (tcp-listen (dict-ref config PORT) MAX-TCP REOPEN))
    (define *players (box house-players))
    (parameterize ([io-time-out (dict-ref config WAIT-FOR-SIGNUP)])
      (let collect-players ()
        (cond
          [(= (length (unbox *players)) MAX-PLAYERS)
           (return (channel-put send-players (unbox *players)))]
          [(>= (length (unbox *players)) MIN-PLAYERS)
           (sync
            (handle-evt listener (sign-up->add-to *players listener))
            (handle-evt send-players (λ (_) (return (channel-put send-players (unbox *players))))))
           (collect-players)]
          [else
           (sync
            (handle-evt listener (sign-up->add-to *players listener))
            (handle-evt send-players (λ (_) (channel-put send-players #false))))
           (collect-players)])))))

#; ([Box [Listof Player]] TCP-Listener -> Void)
(define [(sign-up->add-to *players listener) _]
  (with-handlers ((exn:fail:network? (lambda (x) (log-error "connect: ~a" (exn-message x)) *players)))
    (define-values (in out) (tcp-accept listener))
    (define name (read-message in))
    (cond
      [(player-name? name)
       (define nxt (if (test-run?) (add1 (length (unbox *players))) (make-remote-player name in out)))
       (set-box! *players (cons nxt (unbox *players)))]
      [else
       (eprintf "improper sign-up: ~a\n" (bad-name->message name))
       (close-input-port in)
       (close-output-port out)])))

#; {JSexpr -> String}
(define (bad-name->message name)
  [if (string? name)
      (if (regexp-match #px"Timed" name) "timed out" name)
      (~a "not a string: " name)])

;                                                                                      
;                                                                                      
;     ;       ;             ;                          ;                    ;          
;     ;                                                ;                    ;          
;   ;;;;;   ;;;  ;;;;;;   ;;;   ; ;;    ;;;;         ;;;;;   ;;;    ;;;   ;;;;;   ;;;  
;     ;       ;  ;  ;  ;    ;   ;;  ;  ;;  ;           ;    ;;  ;  ;   ;    ;    ;   ; 
;     ;       ;  ;  ;  ;    ;   ;   ;  ;   ;           ;    ;   ;; ;        ;    ;     
;     ;       ;  ;  ;  ;    ;   ;   ;  ;   ;           ;    ;;;;;;  ;;;     ;     ;;;  
;     ;       ;  ;  ;  ;    ;   ;   ;  ;   ;           ;    ;          ;    ;        ; 
;     ;       ;  ;  ;  ;    ;   ;   ;  ;; ;;           ;    ;      ;   ;    ;    ;   ; 
;     ;;;   ;;;;;;  ;  ;  ;;;;; ;   ;   ;;;;           ;;;   ;;;;   ;;;     ;;;   ;;;  
;                                          ;                                           
;                                       ;  ;                                           
;                                        ;;                                            

;; these tests are entirely independent of the game that's run

(module+ test ;; timing
  
  #; {Port-Number (U False n:N) -> (U False [Listof 0]: (=/c (length) n))}
  #; (run-server-test p k)
  ;; runs the server on port p, waitig for m players, but receiving k
  (define (run-server-test port k #:delay (d #false) #:non-string (n #false))
    [define custodian (make-custodian)]
    [define result    (make-channel)]
    [define err-p     (open-output-string)]
    [define sconfig   (set-server-config default-server-config PORT port)]
    (define th        (launch-server-with-clients sconfig custodian result err-p port k d n))
    (begin0
      (cond
        [(or (not k) (< k MIN-PLAYERS))
         (sync th)
         (get-output-string err-p)]
        [else (channel-get result)])
      (custodian-shutdown-all custodian)))

  #; {Config Custodian [Channel list] Output-port Port [Option Natural] [Option Natural] -> Void}
  (define (launch-server-with-clients config2 custodian result err-out port k d n)
    (parameterize ([test-run?          result]
                   [current-custodian  custodian]
                   [current-error-port err-out])
      (define th (thread (λ () (server config2 #:result values))))
      (sleep 1)
      (sign-up-fake-clients k port d n)
      th))

  #;{Thread [Option Natural] Port-Number [Option Natural] [Option Natural] -> Void}
  (define (sign-up-fake-clients how-many port delay-this-one-s-name-submission this-one-sends-number)
    (unless (boolean? how-many)
      (for ([i how-many])
        (define-values (_ out) (tcp-connect LOCAL port))
        (when (and delay-this-one-s-name-submission (= delay-this-one-s-name-submission i))
          (sleep (+ (dict-ref default-server-config WAIT-FOR-SIGNUP) .1)))
        (if (and this-one-sends-number (= this-one-sends-number i))
            (send-message 42 out)
            (send-message "a" out)))))
  
  'timing-tests
  (check-equal? (run-server-test 45671 #f) "" "no sign ups")
  (check-equal? (run-server-test 45677 10) (range 4 0 -1) "sign up enough players")
  (check-equal? (run-server-test 45677 11 #:delay 3) (range 4 0 -1) "sign up enough players T")
  (check-equal? (run-server-test 45677 11 #:non-string 3) (range 4 0 -1) "sign up enough players NS")
  (check-equal? (run-server-test 45676 1) "" "sign up too few players"))

;                                                                 
;      ;;                                                         
;     ;           ;;;    ;;;             ;                    ;   
;     ;             ;      ;             ;                    ;   
;   ;;;;;  ;   ;    ;      ;           ;;;;;   ;;;    ;;;   ;;;;; 
;     ;    ;   ;    ;      ;             ;    ;;  ;  ;   ;    ;   
;     ;    ;   ;    ;      ;             ;    ;   ;; ;        ;   
;     ;    ;   ;    ;      ;             ;    ;;;;;;  ;;;     ;   
;     ;    ;   ;    ;      ;             ;    ;          ;    ;   
;     ;    ;   ;    ;      ;             ;    ;      ;   ;    ;   
;     ;     ;;;;     ;;     ;;           ;;;   ;;;;   ;;;     ;;; 
;                                                                 
;                                                                 
;                                                                 


;; the following tests are game-specific 
(module+ test
  
  #; {[Listof Player] RefState String -> Void}
  (define ((test-server-client-plus msg  #:quiet (quiet #true)  #:extras (bad-clients '[]))
           rconfig0 players expected msg2)

    ;; WARNING -----------------------------------------------------------------
    ;; reverse so that they sign up in the order in which they are in the state
    ;; for external integration tests, this must be moved to the `expected` part
    ;; WARNING -----------------------------------------------------------------

    (define rconfig  (dict-set rconfig0 PER-TURN PER-TURN-s))
    (define rconfig+ (dict-set rconfig QUIET quiet))
    (define sconfig+ (set-server-config default-server-config QUIET quiet))
    (check-equal? (run-server-client players rconfig+ sconfig+ bad-clients) expected msg))
  
  (define (run-server-client players rconfig sconfig bad-clients)
    (define PORT# 45674)
    (parameterize ([current-custodian (make-custodian)])
      (define sconfig+ (set-server-config sconfig PORT PORT# REF-SPEC rconfig))
      (define quiet    (dict-ref sconfig+ QUIET #true))
      (define client   (launch-clients players PORT# quiet bad-clients))
      (define result   (launch-server sconfig+ quiet))
      (begin0
        result
        (sync client)
        (custodian-shutdown-all (current-custodian)))))

  #; {ServerConfiguration Boolean -> Result}
  (define (launch-server sconfig quiet)
    (define err-out (if quiet (open-output-string) (current-error-port)))
    (parameterize ([current-error-port err-out])
      (server sconfig descending-by-age #:result values)))

  #; {[Listof Player] PortNo Boolean -> Thread}
  (define (launch-clients players port# quiet bad-clients)
    (define err-out (if quiet (open-output-string) (current-error-port)))
    (thread
     (λ ()
       (parameterize ([current-error-port err-out])
         ;; for failuers before the threads are launched
         ;; `quiet` is passed along so that thrreads can quiet the proxy refs
         (define baddies (map (λ (f) (f port#)) bad-clients))
         (clients players port# #:quiet quiet #:baddies baddies)))))

  #; {ServerConfiguration RefereeConfiguration PortNumber -> ServerConfiguration}
  (define (adjust-server-configuration sconfig0 rconfig port#)
    (set-server-config sconfig0 PORT port# REF-SPEC rconfig)))

(module+ test ;; tests with broken players 
  (require (submod Qwirkle/Referee/referee examples))

  'for-tests-7
  (for ([t for-tests-7] [i (in-naturals)])
    (define k (test-server-client-plus (~a "7: " i) #:quiet 'yes!))
    (t k 1 2))

  'for-tests-8
  (for ([t for-tests-9] [i (in-naturals)])
    (define k (test-server-client-plus (~a "8: " i) #:quiet 'yes!))
    (t k 1 2))

  'for-bonus-A
  (for ([t for-bonus-A] [i (in-naturals)])
    (define k (test-server-client-plus (~a "8: " i) #:quiet 'yes!))
    (t k 1 2)))

(module+ test ;; tests with broken clients 
  (define [client-diverge-before-sending-name port#]
    (make-client-for-name-sender (λ (ip) (let L () (L))) port#))

  'special1
  (mixed-all-tiles-rev-inf-exn-dag2-A
   (test-server-client-plus
    (~a "a client that connects but does not complete the registration")
    #:quiet #false  #;'yes!
    #:extras (list client-diverge-before-sending-name))
   1 2)
  
  (define [client-diverge-after-sending-name port#]
    (make-client-for-name-sender (λ (ip) (send-message "a" ip)) (let L () (L))) port#)

  'special2
  #;
  (mixed-all-tiles-rev-inf-exn-dag2-A
   (test-server-client-plus
    (~a "a client that connects but does not complete the registration")
    #:quiet #false  #;'yes!
    #:extras (list client-diverge-after-sending-name))
   1 2))
