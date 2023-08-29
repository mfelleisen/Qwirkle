#lang racket

;; a server that signs up players over TCP and runs a game 

(require (submod (lib "Qwirkle/scribblings/qwirkle.scrbl") spec))

(define PORT            'port)
(define SERVER-TRIES    'server-tries)
(define SERVER-WAIT     'server-wait)
(define WAIT-FOR-SIGNUP 'wait-for-signup)
(define REF-SPEC        'referee-specific)
(define QUIET           'quiet)

(define options (list PORT SERVER-TRIES SERVER-WAIT WAIT-FOR-SIGNUP REF-SPEC QUIET))

;; ---------------------------------------------------------------------------------------------------
(provide
 ;; server options 
 PORT SERVER-TRIES SERVER-WAIT WAIT-FOR-SIGNUP REF-SPEC QUIET

 ascending-by-age
 descending-by-age
 
 (contract-out
  (DEFAULT-CONFIG (hash-carrier/c options))
  
  [server
   ;; returns the list of winners and cheaters/failures

   ;; get at least `MIN-PLAYERS while running at most `SERVER-TRIES` wait periods of `SERVER-WAIT`s
   ;; but as soon as `MAX-PLAYERS` have signed up, stop accepting more players and run the game
   ;; if at the end of a wait period `MIN-PLAYERS` are around, also launch the game and stop signups
   ;; otherwise, shut down.
   ;;   (a production server would have a "wait in line" queue for late comers; and it would restart.)
   ;; 
   ;; runs a referee on the players that signed up properly port# plus the house players (if any) 
   (->i ([confg    (hash-carrier/c options)])
        ([ordering (-> list? list?)]
         [plyrs    list?]
         #:result (return-results-or-void (-> list? any/c)))
        (result any/c))]))

#;
(module+ examples
  (provide

   set-verbose DEFAULT-CONFIG

   #; {[Listof [Cons RefPlayer Coordinate]] Natural String [#:quiet Boolean] -> Result}
   test-server-client-with

   #; {[Listof [Cons RefPlayer Coordinate]] RefState String [#:quiet Boolean]-> Result}
   test-server-client-plus

   #; {[Listof Player] RefereeConfiguration ServerConfiuration -> Result}
   run-server-client

   #; {[List [Listof Player] [Listof Player]] -> [List [Listof String] [Listof String]]}
   ;; from `main` for integration tests 
   referee-results->names

   #; {String -> RIP}
   make-badly-named-player))

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

(define DEFAULT-CONFIG
  (hash
   PORT        PORT0
   SERVER-WAIT    20
   WAIT-FOR-SIGNUP 2
   SERVER-TRIES    1
   ;; a lit of optional keyword arguments:
   ;; `REF-SPEC` is a keyword-dicionary that is turned into arguments to the referee
   ;; that way the server is parametric wrt to the referee's domain 
   REF-SPEC      '[]
   QUIET          #true))

(define (set-verbose c) (dict-set c QUIET #false))
(define (set-port c p) (dict-set c PORT p))

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

#;
(module+ test ;; timing
  
  #; {Port-Number (U False n:N) -> (U False [Listof 0]: (=/c (length) n))}
  #; (run-server-test p k)
  ;; runs the server on port p, waitig for m players, but receiving k
  (define (run-server-test port k #:delay (d #false) #:non-string (n #false))
    [define custodian (make-custodian)]
    [define result    (make-channel)]
    [define err-p     (open-output-string)]
    (define th
      (launch-server-with-clients (set-port DEFAULT-CONFIG port) custodian result err-p port k d n))
    (begin0
      (cond
        [(or (not k) (< k MIN-PLAYERS))
         (sync th)
         (get-output-string err-p)]
        [else (channel-get result)])
      (custodian-shutdown-all custodian)))

  #; {Configuration Custodian [Channel [Listof Any]] Output-port Port (U False N) (U False N) -> Void}
  (define (launch-server-with-clients config2 custodian result err-out port k d n)
    (parameterize ([test-run?          result]
                   [current-custodian  custodian]
                   [current-error-port err-out])
      (define th (thread (λ () (server config2 #:result values))))
      (sleep 1)
      (sign-up-fake-clients th port k d n)
      th))

  #;{Thread Port-Number (U False N) (U False N) (U False N) -> Void}
  (define (sign-up-fake-clients th port k d n)
    (unless (boolean? k)
      (for ([i k])
        (define-values (_ out) (tcp-connect LOCAL port))
        (when (and d (= d i)) (sleep (+ (dict-ref DEFAULT-CONFIG WAIT-FOR-SIGNUP) .1)))
        (if (and n (= n i)) (send-message 42 out) (send-message "a" out)))))
  
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

  (require (submod Qwirkle/Referee/ref-state examples))
  
  (define (test-server-client-with rplayers dont-include# msg #:quiet (quiet #true))
    (define for-state (drop rplayers dont-include#))
    (eprintf "testing ~a\n" msg)

    #;
    (test-server-client-plus rplayers (xstate padded-oli for-state) msg #:quiet quiet)
    ;; the state includes just those players that survive 

    (test-server-client-plus rplayers ref-starter-state msg #:quiet quiet))

  #; {[Listof Player] RefState String -> Void}
  (define ((test-server-client-plus msg  #:quiet (quiet #true)) rconfig0 players expected)

    ;; WARNING -----------------------------------------------------------------
    ;; reverse so that they sign up in the order in which they are in the state
    ;; for external integration tests, this must be moved to the `expected` part
    ;; WARNING -----------------------------------------------------------------

    (define rconfig (dict-set rconfig0 PER-TURN 4.8))
    (check-equal?
     (cond
       [quiet (run-server-client players rconfig)]
       [else
        (define rconfig+ (set-verbose rconfig))
        (define sconfig+ (set-verbose DEFAULT-CONFIG))
        (run-server-client players rconfig+ sconfig+)])
     expected
     msg))
  
  (define (run-server-client players (rconfig '[]) (sconfig DEFAULT-CONFIG))
    (define PORT# 45674)
    (parameterize ([current-custodian (make-custodian)])
      (define sconfig+ (adjust-server-configuration sconfig rconfig PORT#))
      (define quiet    (dict-ref sconfig+ QUIET #true))
      (define client   (launch-clients players PORT# quiet))
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
  (define (launch-clients players PORT# quiet)
    (define err-out (if quiet (open-output-string) (current-error-port)))
    (thread
     (λ ()
       (parameterize ([current-error-port err-out])
         (client players PORT# #:quiet quiet)))))

  #; {ServerConfiguration RefereeConfiguration -> ServerConfiguration}
  (define (adjust-server-configuration sconfig0 rconfig port#)
    (let* ([config sconfig0]
           [config (hash-set config PORT port#)]
           [config (hash-set config REF-SPEC rconfig)])
      config))

  #; {String -> RIP}
  (define (make-badly-named-player name)
    (define xbad-name (create-player name dag-strategy))
    (cons (create-player "irrelevant" 't-coord 'h-coord "navy" #:ep xbad-name) 'c-coord)))

(module+ test
  (require (submod Qwirkle/Referee/referee examples))

  (for ([t for-tests-7] [i (in-naturals)])
    (define k (test-server-client-plus (~a i) #:quiet #false))
    (t k 1 2)))

