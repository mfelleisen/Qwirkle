#lang racket

;; a server that signs up players over TCP and runs a game 

(require (submod (lib "Qwirkle/scribblings/qwirkle.scrbl") spec))

;; ---------------------------------------------------------------------------------------------------
(provide
 ascending-by-age
 descending-by-age

 ;; for homework 
 server-config->definition
 set-server-config
 REF-SPEC PORT 
 
 jsexpr->server-config
 server-config->jsexpr
 
 (contract-out
  [default-server-config server-config/c]
  
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

(module+ examples
  (require Qwirkle/Referee/ref-state))


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
(require (except-in Qwirkle/Referee/referee STATE0 QUIET OBSERVE CONFIG-S PER-TURN))
(require (submod Qwirkle/Referee/referee json))

(require Qwirkle/Lib/configuration)

(require SwDev/Testing/communication)
(require (only-in SwDev/Lib/should-be-racket all-but-last))

(module+ examples
  (require (submod ".."))
  (require
    (prefix-in
     c: (only-in
         Qwirkle/Client/client
         make-client-for-name-sender default-client-config set-client-config QUIET PORT PLAYERS)))
  (require (submod Qwirkle/Referee/referee examples))
  (require (prefix-in r: (only-in Qwirkle/Referee/referee QUIET OBSERVE CONFIG-S PER-TURN)))
  (require (except-in Qwirkle/Referee/referee QUIET OBSERVE CONFIG-S PER-TURN)))

(module+ test
  (require (submod ".."))
  (require (submod ".." examples))
  (require (prefix-in c: Qwirkle/Client/client))
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
  (PORT            PORT0 #:is-a "Natural" "between 10000 and 60000")
  (SERVER-TRIES    2 #:is-a "Natural" "less than 10")
  (SERVER-WAIT     20 #:is-a "Natural" "less than 30[s]")
  (WAIT-FOR-SIGNUP 2 #:is-a "Natural" "less than 10")
  (QUIET           #true #:is-a "Boolean")
  (REF-SPEC
   default-referee-config
   #:to-jsexpr referee-config->jsexpr #:from-jsexpr jsexpr->referee-config #:is-a "RefereeConfig"))
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
    (with-handlers ([exn:fail?
                     (λ (n)
                       (eprintf "server reports referee failure\n")
                       (eprintf "~a\n" (exn-message n))
                       DEFAULT-RESULT)])
      (referee/config (dict-ref config REF-SPEC (hash)) players)))
  (send-message result)
  (optionally-return-result result))

#; {[Listof Player] ServerConfig -> [Listof Player]}
;; collect list of playaers in reverse order of sign-up [youngest first]
;; it spawns a thread to manage time and number of tries in parallel 
(define (wait-for-players house-players config)
  (define max-time  (dict-ref config SERVER-WAIT))
  (define max-tries (dict-ref config SERVER-TRIES))
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

#;
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
;                                                                 
;                                                ;                
;                                                                 
;    ;;;    ;;;    ;;;   ; ;;   ;;;;    ;;;;   ;;;    ;;;    ;;;  
;   ;   ;  ;;  ;  ;;  ;  ;;  ;      ;   ;;  ;    ;   ;; ;;  ;   ; 
;   ;      ;      ;   ;; ;   ;      ;   ;        ;   ;   ;  ;     
;    ;;;   ;      ;;;;;; ;   ;   ;;;;   ;        ;   ;   ;   ;;;  
;       ;  ;      ;      ;   ;  ;   ;   ;        ;   ;   ;      ; 
;   ;   ;  ;;     ;      ;   ;  ;   ;   ;        ;   ;; ;;  ;   ; 
;    ;;;    ;;;;   ;;;;  ;   ;   ;;;;   ;      ;;;;;  ;;;    ;;;  
;                                                                 
;                                                                 
;                                                                 

(module+ examples
  
  (provide
   scenarios-for-7
   scenarios-for-8
   scenarios-for-9
   scenarios-for-A)

  (provide
   scenario-special-1
   scenario-special-2)
  
  #; {type ServerClientScenario =
           [List ServerConfiguration ClientConfiguration [Listof Clients] Any String]}

  #; {Natural [Listof RefereeScenario] -> [Listof ServerClientScenario]}
  (define (scenario* n ref-scenario* #:quiet (quiet #true) #:extras (client* '[]))
    (for/list ([ref-scenario ref-scenario*] [i (in-naturals)])
      (define K (server-client-scenario (~a n ": " i) #:quiet quiet #:extras client*))
      (ref-scenario K "dummy arg 1" "dummy arg 2")))

  #; {String #:quiet Boolean #:extras [Listof Client]
             #:drop  (-> [Listof Player] [Listof Player])
             #:expected (-> Result Result)
             -> RefereeConfig [Listof Player] Any String
             -> ServerClientScenario}
  ;; set up basics, then: 
  ;; create a server-clinet secnarior from a referee scenario, which contains
  ;; (1) a referee configuration, (2) a list of players, (3) expected value; (4) a test message
  (define p# 45674)
  (define ((server-client-scenario
            msg
            #:expected (result identity)
            #:drop     (drop identity)
            #:quiet    (q #true)
            #:extras   (client* '[])
            ;; the update function injects the actual players into the game state to
            ;; circumvent the naming conflict 
            #:update-state-players (update #false) #; (-> [Listof Player] [Listof Player]))
           rc0 player* expected msg2)

    (set! p# (+ p# 1))

    (define nu-players (drop player*))
    (define cc (c:set-client-config c:default-client-config c:PLAYERS nu-players c:QUIET q c:PORT p#))
    
    (define rc
      (cond
        [(not update) (set-referee-config rc0 r:PER-TURN PER-TURN-s r:QUIET q)]
        [else ;; this is a kludge to accommodate special2, which may not be needed
         (parameterize ([dont-double-check-names #true])
           (define state (dict-ref rc0 STATE0))
           (set-referee-config rc0 STATE0 (set-ref-state-players state (update nu-players))))]))
    (define sc (set-server-config default-server-config QUIET q PORT p# REF-SPEC rc))
    
    (define nu-expected (result expected))
    (list sc cc client* nu-expected (~a msg ": " msg2)))

  
  (define scenarios-for-7 (scenario* 7 for-tests-7))
  (define scenarios-for-8 (scenario* 8 for-tests-8)) ;  #:quiet #false
  (define scenarios-for-9 (scenario* 9 for-tests-9))
  (define scenarios-for-A (scenario* 'A for-bonus-A))

  ;; - - - 
  (define [client-diverge-before-sending-name port#]
    (c:make-client-for-name-sender (λ (ip) (let L () (L))) port#))
  
  (define scenario-special-1
    (mixed-all-tiles-rev-inf-exn-dag2-A
     (server-client-scenario
      (~a "a client that connects but does not complete the registration")
      #:extras (list client-diverge-before-sending-name))
     "dummy one"
     "dummy two"))

  ;; - - -
  ;; this should be observationally equivalent to a player that goes infinite in the setup method

  (define name-cd  "sendnameloop")
  (define [client-diverge-after-sending-name port#]
    (c:make-client-for-name-sender (λ (ip) (send-message name-cd ip) (let L () (L))) port#))
  (define remote-player-for-client
    (make-remote-player name-cd (current-input-port) (current-output-port)))

  (define scenario-special-2
    (mixed-all-tiles-rev-inf-exn-dag ;; four players expected 
     (server-client-scenario 
      (~a "a client that connects but does not complete the registration")
      #:drop all-but-last
      #:update-state-players (λ (given) (append given [list remote-player-for-client]))
      #:expected (λ (x) (list (first x) (cons name-cd (reverse (rest (second x))))))
      #:extras (list client-diverge-after-sending-name))
     "dummy one"
     "dummy two")))

;                                                                        
;      ;;                                                                
;     ;           ;;;    ;;;             ;                    ;          
;     ;             ;      ;             ;                    ;          
;   ;;;;;  ;   ;    ;      ;           ;;;;;   ;;;    ;;;   ;;;;;   ;;;  
;     ;    ;   ;    ;      ;             ;    ;;  ;  ;   ;    ;    ;   ; 
;     ;    ;   ;    ;      ;             ;    ;   ;; ;        ;    ;     
;     ;    ;   ;    ;      ;             ;    ;;;;;;  ;;;     ;     ;;;  
;     ;    ;   ;    ;      ;             ;    ;          ;    ;        ; 
;     ;    ;   ;    ;      ;             ;    ;      ;   ;    ;    ;   ; 
;     ;     ;;;;     ;;     ;;           ;;;   ;;;;   ;;;     ;;;   ;;;  
;                                                                        
;                                                                        
;                                                                        

(module+ test ;; how to run scenarios as unit tests 
  #; {ServerClientScenario -> Void}
  ;; start a server; start regular clients then bad clients; test
  (define (run-server-client-scenario server-client-scenario)
    (match-define [list sc cc bad-clients expected msg] server-client-scenario)

    (run-server-client sc cc bad-clients)

    (check-equal? (run-server-client sc cc bad-clients) expected msg))
  
  (define (run-server-client sconfig cconfig bad-clients)
    (parameterize ([current-custodian (make-custodian)])
      (define client   (launch-clients cconfig bad-clients))
      (define result   (launch-server sconfig))
      (begin0
        result
        (sync client)
        (custodian-shutdown-all (current-custodian)))))

  #; {ServerConfiguration -> Result}
  (define (launch-server sconfig)
    (define quiet (dict-ref sconfig QUIET))
    (define err-out (if quiet (open-output-string) (current-error-port)))
    (parameterize ([current-error-port err-out])
      (server sconfig descending-by-age #:result values)))

  #; {ServerConfiguration [Listof Player] [Listof Player] -> Thread}
  (define (launch-clients cconfig bad-clients)
    (define port#   (dict-ref cconfig c:PORT))
    (define quiet   (dict-ref cconfig c:QUIET))
    (define err-out (if quiet (open-output-string) (current-error-port)))
    (thread
     (λ ()
       (parameterize ([current-error-port err-out])
         ;; for failuers before the threads are launched
         ;; `quiet` is passed along so that thrreads can quiet the proxy refs
         (define baddies (map (λ (f) (f port#)) bad-clients))
         (c:clients cconfig #:baddies baddies))))))

;; ---------------------------------------------------------------------------------------------------

(module+ test ;; running scenarios as unit tests
  7
  (for-each run-server-client-scenario scenarios-for-7)
  8
  (for-each run-server-client-scenario scenarios-for-8)
  9
  (for-each run-server-client-scenario scenarios-for-9)
  'A
  (for-each run-server-client-scenario scenarios-for-A))

(module+ test
  'special-2
  (run-server-client-scenario scenario-special-2)
  'special-1
  (run-server-client-scenario scenario-special-1))

