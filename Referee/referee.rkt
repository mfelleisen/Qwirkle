#lang racket

;; referee: mediates between external players (local or remote) and the referee state, via safe xsend 
;; ---------------------------------------------------------------------------------------------------

(require (submod (file "../scribblings/qwirkle.scrbl") spec))
(require Qwirkle/Common/player-interface)
(require SwDev/Contracts/unique)
(require SwDev/Lib/hash-contract)

(define STATE0   'initial-state)
(define QUIET    'quiet)
(define PER-TURN 'time-per-turn)
(define OBSERVE  'observe)
(define SHOW     'observer-shows-for-s)
(define options  (list STATE0 QUIET OBSERVE SHOW PER-TURN))

(define unit-test-mode [make-parameter #true])

#; {Configuration [Listof Player] -> Boolean}
(define (matching-number config players)
  (let* ([state0  (dict-ref config STATE0)]
         [sop#    (and state0 (player-count state0))]
         [player# (length players)])
    (or (unit-test-mode)
        (and (= sop# player#) (<= MIN-PLAYERS sop# MAX-PLAYERS)))))

(provide
 ;; config options
 STATE0 QUIET OBSERVE SHOW PER-TURN
 
 (contract-out
  [create-config
   ;; create a default configuration from a referee state 
   (->* (state?) (#:observe any/c #:per-turn (and/c real? positive?)) (hash-carrier/c options))]
  
  [referee/config
   ;; runs a game from the given configuration, which contains a ref state plus the above options
   ;; The referee calls player methods via `xsend`, which
   ;; -- limits the time a method call can last (configured)
   ;; -- catches exn that the method call raises
   ;; When `xsend` notices either problem, the referee kicks the player out.
   ;; 
   ;; The referee's one-turn methods distinguishes five different results of an `xsend`:
   ;; (1) failed (as above) (2) PASS (3) LEGAL REPLACEMENT (4) LEGAL PLACEMENTS (5) ILLEGAL Requests
   ;; It throws out players when (1) or (5) happens. 
   ;;
   (->i ([config (hash-carrier/c options)] [players (listof player/c)])
        #:pre/name (players) "distince names" (distinct? (map (λ (p) (send p name)) players))
        #:pre/name (config players) "matching number of players" (matching-number config players)
        (r [list/c [listof string?] [listof string?]]))]))

(module+ examples
  (provide
   all-tests))

(module+ examples
  (provide
   textual-observer
   FLUSH

   plain-main
   dag-player*))

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

(require Qwirkle/Common/state-of-player)
(require Qwirkle/Referee/ref-state)

;; -----------------------------------------------------------------------------
(require Qwirkle/Lib/xsend)

(define-syntax (xsend+ stx)
  (syntax-case stx (failed)
    [(_ rp method arg ... ([failed f ...] [q a ...] ...))
     #'(match (xsend (sop-player rp) method arg ...)
         [(? failed? r) (report 'method rp (failed-value r)) f ...]
         [q a ...]
         ...)]))

#; {Symbol SoPlayer Failed -> Void}
(define (report m p f-msg)
  (define n (send (sop-player p) name))
  (eprintf "~a dropped out on ~a: ~v [time limit ~v]\n" n m f-msg per-turn))
;; -----------------------------------------------------------------------------

(module+ examples
  (require (submod Qwirkle/Common/map examples))
  (require (submod Qwirkle/Common/tiles examples))
  (require (submod Qwirkle/Player/mechanics json))
  (require (submod Qwirkle/Referee/ref-state json))
  
  (require Qwirkle/Common/map)
  (require Qwirkle/Common/tiles)
  (require Qwirkle/Player/mechanics)
  (require Qwirkle/Player/strategies)
  (require Qwirkle/Lib/check-message)

  (require SwDev/Lib/should-be-racket)
  (require SwDev/Testing/check-values)
  (require SwDev/Testing/testing)
  (require json)
  (require rackunit)
  (require (for-syntax syntax/parse)))

(module+ test
  (define cep current-error-port)

  (require (submod ".."))
  (require (submod ".." examples))
  (require (submod Qwirkle/Common/game-state examples))
  (require (submod Qwirkle/Referee/ref-state examples))
  (require Qwirkle/Player/mechanics)
  (require Qwirkle/Player/strategies)
  (require Qwirkle/Lib/check-message)
  (require SwDev/Testing/check-values)
  (require SwDev/Lib/should-be-racket)
  (require rackunit)
  (require (for-syntax syntax/parse)))


;                                            
;                           ;;               
;     ;;;                  ;      ;          
;    ;                     ;                 
;   ;       ;;;   ; ;;   ;;;;;  ;;;     ;;;; 
;   ;      ;   ;  ;;  ;    ;      ;    ;   ; 
;   ;      ;   ;  ;   ;    ;      ;    ;   ; 
;   ;      ;   ;  ;   ;    ;      ;    ;   ; 
;    ;     ;   ;  ;   ;    ;      ;    ;   ; 
;     ;;;   ;;;   ;   ;    ;    ;;;;;   ;;;; 
;                                          ; 
;                                       ;;;  
;                                            

(define per-turn 4.0)  ;; time limit per turn
(define (set-per x) (set! per-turn x)) ;; for the testing submod 
(define quiet    [make-parameter #false])
(define (set-with x) (set! with-obs x))
(define with-obs #false)
(define obs-show 1)

#; {State -> [Hashtable Options]}
(define (create-config j #:observe (o #false) #:per-turn (pt #f))
  (hash
   OBSERVE  o
   SHOW     1
   STATE0   j
   QUIET    #true
   PER-TURN (or pt per-turn)))

#; {[Hashtable options] -> (values State Observer)}
;; EFFECT installs the values of `c` globally
;; returns the content of the state field 
(define (install-config c players)
  (quiet (dict-ref c QUIET quiet))
  (set-per (dict-ref c PER-TURN per-turn))
  (set! obs-show (dict-ref c SHOW obs-show))
  (define state0 (dict-ref c STATE0))
  (values (set-ref-state-players state0 players) (dict-ref c OBSERVE with-obs)))
  

;                                                          
;                    ;;                                    
;                   ;                                      
;                   ;                                      
;    ;;;;   ;;;   ;;;;;   ;;;    ;;;;   ;;;    ;;;    ;;;  
;    ;;  ; ;;  ;    ;    ;;  ;   ;;  ; ;;  ;  ;;  ;  ;   ; 
;    ;     ;   ;;   ;    ;   ;;  ;     ;   ;; ;   ;; ;     
;    ;     ;;;;;;   ;    ;;;;;;  ;     ;;;;;; ;;;;;;  ;;;  
;    ;     ;        ;    ;       ;     ;      ;          ; 
;    ;     ;        ;    ;       ;     ;      ;      ;   ; 
;    ;      ;;;;    ;     ;;;;   ;      ;;;;   ;;;;   ;;;  
;                                                          
;                                                          
;

#; {type Result = [List [Listof String] [Listof String]]}
;; the first list contains the name of winning players, sorted in lexicographic (string<?) order
;; the second is the list of misbehaving players, in the order in which they dropped out 

(define DEFAULT-RESULT '[[] []]) 

#; {Configuration [Listof [Instanceof Player]] -> Result}
;; `lo-players` must list the player in the order in which they get into the state 
(define (referee/config c lo-players)
  (define-values (s o) (install-config c lo-players))
  (referee/state s #:with-obs o))

#; {State {#:with-obs (U False Observer)} -> Result}
;; This is the workhorse of REFEREES.
;; ASSUME the given state is not finished (at least one player, no winner)
(define (referee/state s0 #:with-obs (wo #false))
  [time-out-limit per-turn]
  (dynamic-wind (setup-observer wo) (referee/state-proper s0) (unset-observer wo)))

#; {State -> Result}
(define [(referee/state-proper s0)]
  (parameterize ([current-error-port (if [quiet] (open-output-string) (current-error-port))])
    (let*-values ({[s out]              (setup s0)}
                  {[winners+losers out] (if (false? s) (values DEFAULT-RESULT out) (rounds s out))}
                  ([winners0 losers0]   (apply values winners+losers))
                  {[true-winners out]   (inform-about-outcome winners0 #true out)}
                  {[_true-losers out]   (inform-about-outcome losers0 #false out)})
      (list (sort (extract-names true-winners) string<?)
            (reverse (extract-names out))))))

#; {State (U False Observer) -> Void}
;; turn with-obs into a function that consumes a state
(define [(setup-observer wo)]
  (set-with (or wo void)))

#; {(U False Observer) -> Void}
(define [(unset-observer wo)]
  (with-obs #false)
  (when wo (sleep obs-show))
  (set-with #false))

(module+ test
  #; {TC is pne of:
         (ref-test-case title:String className:StringExpr factory:id state:Expr
                        [win:ListofString out:ListOfString] ;; winners and losers, drop-outs 
                        MaybePlayers
                        MaybeQuiet
                        MaybeObserver)}
  
  #; {MaybePlayers  = ϵ || #:extra n:Id x:PlayerExpr}
  #; {MaybeQuiet    = ϵ || #:quiet x:BoolExpr}
  #; {MaybeObserver = ϵ || #:with-obs x:FunExpr μf.[State -> f]}

  ;; tests both referee/state and referee/config 
  (define-syntax (ref-test-case stx)
    (syntax-parse stx
      [(test-case msg:string name:string factory:id state:expr [win out]
                  (~optional (~seq #:extra Bname:id B:expr))
                  (~optional (~seq #:quiet Q:expr) #:defaults ([Q #'#false]))
                  (~optional (~seq #:with-obs O:expr)))
       #'(let ()
           (let* ([run (λ (x) (~? (referee/state x) (referee/state x #:with-obs O)))]
                  (P (create-player "A" dag-strategy #:bad (retrieve-factory name factory)))
                  [S (set-ref-state-players state [cons P (~? (list B) '[])])])
             [quiet #false]
             (check-equal? (check-message msg cep #px"dropped" (run S)) (list win out) msg))
           ;; --- to cover the default #:with-obs clause 
           (let* ([config (λ (s) (~? (create-config s #:observe O #:per-turn 1.1) (create-config s)))]
                  [C (dict-set (config state) QUIET Q)]
                  (P (create-player "A" dag-strategy #:bad (retrieve-factory name factory)))
                  [run (λ () (referee/config C (cons P (~? (list B) '[]))))])
             (check-equal?
              (if Q [run] (check-message msg cep #px"dropped" [run])) `(,win ,out) msg)))])))

(module+ test
  (ref-test-case "receiving new tiles fails"
                 "new-tiles" factory-table-7 (active-sop-hand state1 (list #s(tile clover red)))
                 ['[] '["A"]])

  (ref-test-case "receiving new tiles fails"
                 "new-tiles" factory-table-7 (active-sop-hand state1 (list #s(tile clover red)))
                 ['[] '["A"]]
                 #:quiet 'yes)

  (ref-test-case "setup fails"
                 "setup" factory-table-7 (active-sop-hand state1 (list #s(tile clover red)))
                 ['[] '["A"]])

  (ref-test-case "informing fails"
                 "win" factory-table-7 (active-sop-hand state1 (list #s(tile clover red)))
                 ['[] '["A"]])

  (ref-test-case "receiving new tiles fails; game goes on with one more player"
                 "new-tiles" factory-table-7
                 (active-sop-hand state1-with (list #s(tile clover red)))
                 ['["B"] '["A"]]
                 #:extra B (create-player "B" dag-strategy)))

(module+ examples
  ;; a primitive textual observer 
  (define seq* '[])
  (define FLUSH (gensym 'flush))
  (define (textual-observer s)
    (cond
      [(false? s) (reverse seq*)]
      [(eq? FLUSH s) (set! seq* '[])]
      [else
       (set! seq* (cons s seq*))
       textual-observer])))

(module+ test 
  (ref-test-case "receiving new tiles fails, cover observer"
                 "new-tiles" factory-table-7 (active-sop-hand state1 (list #s(tile clover red)))
                 ['[] '["A"]]
                 #:with-obs textual-observer)
  (check-equal? (length (textual-observer #false)) 1)
  (textual-observer FLUSH))

;                                     
;                                     
;                   ;                 
;                   ;                 
;    ;;;    ;;;   ;;;;;  ;   ;  ;;;;  
;   ;   ;  ;;  ;    ;    ;   ;  ;; ;; 
;   ;      ;   ;;   ;    ;   ;  ;   ; 
;    ;;;   ;;;;;;   ;    ;   ;  ;   ; 
;       ;  ;        ;    ;   ;  ;   ; 
;   ;   ;  ;        ;    ;   ;  ;; ;; 
;    ;;;    ;;;;    ;;;   ;;;;  ;;;;  
;                               ;     
;                               ;     
;                               ;     

(define TILES0# 6) ;; should be in the problem statement 

#; {State -> (values State [Listof SoPlayer])}
;; setup players with initial public view and their (private) tiles
(define (setup s0)
  #; {SoPlayer State [Listof SoPlayer] -> (values State [Listof SoPlayer])}
  (define (inform p s0 out)
    (define-values [handouts s] (state-handouts s0 #false))
    (xsend+ p setup (ref-state-to-info-state s) handouts
            [[failed (values (state-kick s #:from-active #true)  (cons p out))]
             [_      (values (state-rotate s) out)]]))
  ;; -- in --
  (match-define [list opt-state out++] (fold-players inform s0 '[]))
  (values opt-state out++))

(module+ test

  #; {TC is pne of:
         (setup-test-case title:String className:StringExpr factory:id state:Expr
                          [newState:Expr out:[Listof SoPlayer]] ;; winners and losers, drop-outs 
                          MaybeDropped
                          MaybePlayers)}
  #; {MaybeDropped  = ϵ || #:dropped px:String}
  
  ;; tests whether setup produces the expected newState and drops players that misbehave  
  (define-syntax (setup-test-case stx)
    (syntax-parse stx
      [(test-case title:string name:string factory:id state:expr
                  [new-state dropped-out]
                  (~optional (~seq #:dropped  e:string))
                  (~seq #:extra Bname:id Bplayer:expr))
       #:with S (datum->syntax #'test-case 'S #'test-case #'test-case)
       #:with A (datum->syntax #'test-case 'A #'test-case #'test-case)
       #'(let* ((A (create-player (~a "A-" title) dag-strategy #:bad (retrieve-factory name factory)))
                [Bname Bplayer]
                [S (set-ref-state-players state [list A Bname])])
           (~?
            (define-values [S++ out] (check-message title cep (pregexp e) (setup S)))
            (define-values [S++ out] [setup S]))
           (check-values (values S++ (map sop-player out)) new-state dropped-out title))])))

(module+ test
  (setup-test-case "setup works fine" "good" factory-base ref-starter-state [S '[]]
                   #:extra B (create-player "B" dag-strategy))

  (setup-test-case "players drops out during setup" "setup" factory-table-7 ref-starter-state
                   [(state-kick S #:from-active #t) `(,A)]
                   #:dropped "dropped"
                   #:extra C (create-player "C" dag-strategy)))

;                                                                 
;                                                                 
;                                                                 
;                                                                 
;    ;;;;  ;   ;  ; ;;           ;;;;  ;;;;  ;;;;;;   ;;;    ;;;  
;    ;;  ; ;   ;  ;;  ;         ;;  ;      ; ;  ;  ; ;;  ;  ;   ; 
;    ;     ;   ;  ;   ;         ;   ;      ; ;  ;  ; ;   ;; ;     
;    ;     ;   ;  ;   ;         ;   ;   ;;;; ;  ;  ; ;;;;;;  ;;;  
;    ;     ;   ;  ;   ;         ;   ;  ;   ; ;  ;  ; ;          ; 
;    ;     ;   ;  ;   ;         ;; ;;  ;   ; ;  ;  ; ;      ;   ; 
;    ;      ;;;;  ;   ;          ;;;;   ;;;; ;  ;  ;  ;;;;   ;;;  
;                                   ;                             
;                                ;  ;                             
;                                 ;;                              
 
#; {State [Listof SoPlayer] -> (values [Listof [Listof SoPlayer]] [Listof SoPlayer])}
;; determine winners and kicked players 
(define (rounds s0 out0)
  (let rounds ([s s0] [out out0])
    (define-values (s+ game-over? out+) (one-round s out))
    (cond
      [game-over? (with-obs s+)
                  (define w+l (if (false? s+) DEFAULT-RESULT (determine-winners s+)))
                  (values w+l out+)]
      [else       (rounds s+ out+)])))

(module+ test
  #; {(*-rounds-test-case title:String className:StringExpr factory:id state:Expr
                          [[winners losers] drop-outs]
                          MaybePlayers)}
  
  ;; runs one round, binds the manufatured player to A, and then checks that the game 
  ;; terminates with the specified winners (classified survivors) and drop-outs
  (define-syntax (*-rounds-test-case stx)
    (syntax-parse stx
      [(test-case title:string n:string factory:id state:expr
                  [[winners losers] out]
                  (~optional (~seq #:extra Bname:id B:expr)))
       #:with A (datum->syntax #'test-case 'A #'test-case #'test-case)
       #'(let (~? ([Bname B]) ())
           (let* ([F (retrieve-factory n factory)]
                  [T state]
                  [C (~? (list B) '[])])
             (let* ([L (~a "test for " title)]
                    (A (create-player (~a "A-" title) dag-strategy #:bad F))
                    [_ (send A setup (ref-state-to-info-state T) '())]
                    [S (set-ref-state-players T [cons A C])])
               (set-with void)
               (define-values [W O] (check-message title cep #px"dropped" (rounds S '[])))
               (define awinners (map (λ (x) (send (sop-player x) name)) (first W)))
               (define alosers (map sop-player (second W)))
               (check-equal? `[,awinners ,alosers] `[,winners ,losers] (~a "winners " L))
               (check-equal? (map sop-player O) out (~a "drop outs " L))
               (set-with #false))))])))

(module+ test
  (*-rounds-test-case "receiving new tiles fails; game goes on with one more player"
                      "new-tiles" factory-table-7
                      (active-sop-hand state1-with (list #s(tile clover red)))
                      ;; this seems to trip over object equality, wach out 
                      [[`["B"] `[]] `[,A]]
                      #:extra B (create-player "B" dag-strategy))

  (*-rounds-test-case "receiving new tiles fails"
                      "new-tiles" factory-table-7
                      (active-sop-hand state1 (list #s(tile clover red)))
                      [[`[] `[]] `(,A)]))

;                                                   
;                                                 ; 
;     ;;                                          ; 
;    ; ;                                          ; 
;      ;           ;;;;   ;;;   ;   ;  ; ;;    ;;;; 
;      ;           ;;  ; ;; ;;  ;   ;  ;;  ;  ;; ;; 
;      ;           ;     ;   ;  ;   ;  ;   ;  ;   ; 
;      ;           ;     ;   ;  ;   ;  ;   ;  ;   ; 
;      ;           ;     ;   ;  ;   ;  ;   ;  ;   ; 
;      ;           ;     ;; ;;  ;   ;  ;   ;  ;; ;; 
;    ;;;;;         ;      ;;;    ;;;;  ;   ;   ;;;; 
;                                                   
;                                                   
;                                                   

#; {type OneRound = (values State end:Boolean [Listof SoPlayer])}
#; {type StopGame = [State Boolean [Listof SoPlayer] -> Empty]}
#; {(values s game-over all-passed kicked)}
;; s          : the final state of the round,
;; game-over  : does s satisfy `(finished? s)` because one player is done
;; all-passed : did all players pass?
;; kicked     : all misbehaving players since the beginning of the game

#; {State [Listof SoPlayer] -> OneRound}
(define (one-round s0 out0)
  (let/ec stop #; [OneRound -> Empty]
    (define [normal s out] (list s (unbox pass*) out))
    (define pass* (box #true))
    (match-define [list s++ end? out++] (fold-players (one-turn stop pass*) s0 out0 #:return normal))
    (values s++ end? out++)))
  
(module+ test

  #; (1-round-test-case title:String className:StringExpr factory:id state:Expr
                        [MaybeEnd MaybeState MaybeDropped]
                        MaybePlayers)
  #; {MaybeEnd     = ϵ || #:end? end:id}
  #; {MaybeState   = ϵ || #:state? s #;symbol}
  #; {MaybeDropped = ϵ || #:dropped? d #;symbol}
  
  ;; runs one round with the one manufacture player, plus one optional extra,
  ;; to yield the next state (S++), the end-of-game statis (end), and the drop-`out`s 
  (define-syntax (1-round-test-case stx)
    (syntax-parse stx
      [(test-case title:string name:string factory:id state:expr
                  [(~optional (~seq #:end? en #;symbol) #:defaults ([en #'#true]))
                   (~optional (~seq #:state? ok #;symbol) #:defaults ([ok #'#true]))
                   (~optional (~seq #:dropped? dr #;symbol) #:defaults ([dr #'#false]))]
                  (~optional (~seq #:extra Bname:id B:expr)))
       #'(let (~? ([Bname B]) ())
           (let ([F (retrieve-factory name factory)]
                 [T state]
                 [C (~? (list Bname) '[])])
             (let* ([W (~a " test for " title)]
                    (P (create-player (~a "A-" title) dag-strategy #:bad F))
                    [_ (send P setup (ref-state-to-info-state T) '())]
                    [S (set-ref-state-players T [cons P C])])
               (set-with void)
               (define-values [S++ end out]
                 (if dr
                     (check-message W cep #px"dropped out" (one-round S '[]))
                     [one-round S '[]]))
               (check-equal? end end W)
               (if ok (check-true (state? S++) W) (check-false S++ W))
               (if dr
                   (check-equal? (map sop-player out) `[,P] (~a "A drop out" W))
                   (check-equal? out '[] (~a "no:" W)))
               (set-with #false))))])))

(module+ test
  (1-round-test-case "receiving new tiles fails"
                     "new-tiles" factory-table-7
                     (active-sop-hand state1 (list #s(tile clover red)))
                     [#:end? #true #:state? #false #:dropped? 'yes!])

  (1-round-test-case "receiving new tiles fails; game goes on with one more player"
                     "new-tiles" factory-table-7
                     (active-sop-hand state1-with (list #s(tile clover red)))
                     [#:end? #false #:state? #true #:dropped? 'yes!]
                     #:extra B (create-player "B" dag-strategy))

  (1-round-test-case "player wins on take turn"
                     "new-tiles" factory-table-7
                     state1
                     (#:end? #true #:state? #true)))

;                                            
;                                            
;    ;;            ;                         
;   ; ;            ;                         
;     ;          ;;;;;   ;   ;   ;;;;  ; ;;  
;     ;            ;     ;   ;   ;  ;  ;;  ; 
;     ;            ;     ;   ;   ;     ;   ; 
;     ;            ;     ;   ;   ;     ;   ; 
;     ;            ;     ;   ;   ;     ;   ; 
;   ;;;;;          ;;;    ;;;;   ;     ;   ; 
;                                            
;                                            
;                                            

#; {type OneTurn = (values State [Listof SoPlayer])}
#; (value s out)
;; s   : the state after one turn
;; out : the list of misbehaving players at the end of the turn

#; {StopGame [Box Boolean] -> SoPlayer State [Listof SoPlayer] -> OneTurn}
(define ((one-turn ap-ends-game all-passed) ap s out)
  (with-obs s)
  (xsend+ ap take-turn (ref-state-to-info-state s)
          [[failed ;; communication 
            (values (state-kick s) (cons ap out))] 
           [(legal-pass)
            (values (state-rotate s) out)]
           [(legal-placement s s+ tiles-placed)
            (player-not-passed! all-passed)
            (cond
              [(active-sop-finished? s+ #;"s+'s ap no longer owns the placed tiles") 
               (ap-ends-game s+ #true out)]
              [else
               (define-values [handouts s++] (state-handouts s+ (length tiles-placed)))
               (hand-tiles-now ap s++ handouts out ap-ends-game)])]
           [(legal-re-placement s s+ ap-s-tiles)
            (define-values [handouts s++] (state-handouts s+ (length ap-s-tiles)))
            (hand-tiles-now ap s++ handouts out ap-ends-game)]
           [_illegal-request_
            (report 'take-turn ap "illegal placement or re-placement request")
            (values (state-kick s) (cons ap out))]]))

#; {SoPlayer State [Listof Tile] [Listof SoPlayer] -> OneTurn}
(define (hand-tiles-now ap s+ handouts out ap-ends-game)
  (xsend+ ap new-tiles handouts
          [[failed
            (define s++ (state-kick s+))
            (if s++ (values s++ (cons ap out)) (ap-ends-game #false #true (cons ap out)))]
           [_ (values (state-rotate (active-sop-hand s+ handouts)) out)]]))

;; auxiliaries for readability only 
(define (player-not-passed! b) (set-box! b #false) #false)

(module+ test
  #; {(test-case title:String className:String factory:id state:Expr
                 [MaybeEnd MaybeState MaybePassed MaybeDropped]
                 MaybePlayer)}
  #; {MaybePassed = ϵ || #:passed? d #;symbol}

  ;; runs one turn with the one manufacture player (A), plus one optional named extra,
  ;; and then tests whether the player ended the game (#:end?), acted (#:passed?),
  ;; dropped out (#:dropped), and/or whether the state is "terminal" (#false)
  (define-syntax (1-turn-test-case stx)
    (syntax-parse stx
      [(test-case title:string name:string factory:id state:expr
                  [(~optional (~seq #:end? end:id))
                   (~optional (~seq #:state? ok #;symbol) #:defaults ([ok #'#true]))
                   (~optional (~seq #:passed? ps #;symbol) #:defaults ([ps #'#false]))
                   (~optional (~seq #:dropped? dr #;symbol) #:defaults ([dr #'#false]))
                   checks ...]
                  (~optional (~seq #:extra Bname:id B:expr)))
       #'(let* ([F (retrieve-factory name factory)]
                [T state]
                [C (~? (list B) '[])]
                [fake-k (λ (s end? out) (values s end? out))]
                [passed [box #true]]
                [run (one-turn #;"fake continuation:" fake-k passed)])
           (let* ([W title]
                  (P (create-player (~a "A-> " W) dag-strategy #:bad F))
                  [_ (send P setup (ref-state-to-info-state T) '())]
                  [S (set-ref-state-players T [cons P C])]
                  [A (active-player S)])
             (set-with void)
             (define-values (~? [S++ end out] [S++ out])
               (if dr
                   (check-message W cep #px"dropped out" (run A S '[]))
                   [run A S '[]]))
             (~? (check-true end W) (void))
             (if ok (check-true (state? S++) W) (check-false S++ W))
             (if ps (check-true (unbox passed) W) (check-false (unbox passed) W))
             (if dr
                 (check-equal? (map sop-player out) `[,P] (~a "A drop out" W))
                 (check-equal? out '[] (~a "no:" W)))
             (set-with #false)
             (set-box! passed #true #;"restore old value")))])))

(module+ test
  (1-turn-test-case "player passes due to lack of placable tiles: ref-starter-state"
                    "good" factory-base ref-starter-state
                    [#:passed? 'yes!]
                    #:extra B (create-player "B" dag-strategy))
  
  (1-turn-test-case "player requests legal placement of tiles: state0; no more passing"
                    "good" factory-base state0 
                    [])

  (1-turn-test-case "player passes"
                    "good" factory-base bad-state
                    [#:passed? 'yes!])

  (1-turn-test-case "correctly shaped, but illegal placement request; state is #false in this case"
                    "tile-not-owned" factory-table-10 bad-state
                    [#:state? #false #:passed? 'yes! #:dropped? 'yes!])

  (1-turn-test-case "cover illegal request; state is #false in this case because there's 1 player"
                    "take-turn" factory-table-7 bad-state
                    [#:state? #false #:passed? 'yes! #:dropped? 'yes!])

  (1-turn-test-case "receiving new tiles fails; it's the last player so game's up"
                    "new-tiles" factory-table-7 (active-sop-hand state1 (list #s(tile clover red)))
                    [#:end? end? #:state? #false #:dropped? 'yes!])

  (1-turn-test-case "receiving new tiles fails; game goes on with one more player"
                    "new-tiles" factory-table-7
                    (active-sop-hand state1-with (list #s(tile clover red)))
                    [#:dropped? 'yes!]
                    #:extra B (create-player "B" dag-strategy))

  (1-turn-test-case "player wins on take turn"
                    "new-tiles" factory-table-7
                    state1
                    (#:end? end?)))

;                                                                                                    
;                    ;;                                                                              
;      ;            ;                                          ;                                     
;                   ;                                                                                
;    ;;;   ; ;;   ;;;;;   ;;;    ;;;; ;;;;;;        ;     ;  ;;;   ; ;;   ; ;;    ;;;    ;;;;   ;;;  
;      ;   ;;  ;    ;    ;; ;;   ;;  ;;  ;  ;       ;     ;    ;   ;;  ;  ;;  ;  ;;  ;   ;;  ; ;   ; 
;      ;   ;   ;    ;    ;   ;   ;    ;  ;  ;        ; ; ;     ;   ;   ;  ;   ;  ;   ;;  ;     ;     
;      ;   ;   ;    ;    ;   ;   ;    ;  ;  ;        ; ; ;     ;   ;   ;  ;   ;  ;;;;;;  ;      ;;;  
;      ;   ;   ;    ;    ;   ;   ;    ;  ;  ;        ;; ;;     ;   ;   ;  ;   ;  ;       ;         ; 
;      ;   ;   ;    ;    ;; ;;   ;    ;  ;  ;        ;; ;;     ;   ;   ;  ;   ;  ;       ;     ;   ; 
;    ;;;;; ;   ;    ;     ;;;    ;    ;  ;  ;         ; ;    ;;;;; ;   ;  ;   ;   ;;;;   ;      ;;;  
;                                                                                                    
;                                                                                                    
;                                                                                                    

#; {[Listof SoPlayer] Boolean [Listof SoPlayer] -> (values [Listof SoPlayer] [Listof SoPlayer])}
;; iterate over the given list of players and send them `msg`; collect players that fail 
(define (inform-about-outcome lop msg out0)
  (for/fold ([survived '()] [out out0] #:result (values (reverse survived) (reverse out))) ([p lop])
    (xsend+ p win msg
            [[failed (values survived (cons p out))]
             [_      (values (cons p survived) out)]])))

;                                                          
;                                                          
;                                                ;         
;                                                          
;    ;;;    ;;;    ;;;   ; ;;   ;;;;    ;;;;   ;;;    ;;;  
;   ;   ;  ;;  ;  ;;  ;  ;;  ;      ;   ;;  ;    ;   ;; ;; 
;   ;      ;      ;   ;; ;   ;      ;   ;        ;   ;   ; 
;    ;;;   ;      ;;;;;; ;   ;   ;;;;   ;        ;   ;   ; 
;       ;  ;      ;      ;   ;  ;   ;   ;        ;   ;   ; 
;   ;   ;  ;;     ;      ;   ;  ;   ;   ;        ;   ;; ;; 
;    ;;;    ;;;;   ;;;;  ;   ;   ;;;;   ;      ;;;;;  ;;;  
;                                                          
;                                                          
;                                                          

(module+ examples
  
  (define dag-player*
    (let* ([normal (retrieve-factory "good" factory-base)]
           [A (create-player "A" dag-strategy #:bad normal)]
           [B (create-player "B" dag-strategy #:bad normal)]
           [C (create-player "C" dag-strategy #:bad normal)]
           [D (create-player "D" dag-strategy #:bad normal)])
      (list A B C D)))

  (provide for-students)

  (define for-students '[])
  (define for-tests    '[])
  (define for-bonus    '[])
  
  (define [all-tests] (append for-students for-tests for-bonus))
  
  (define-syntax (integration-test stx)
    (syntax-parse stx
      [(_ name:id
          #:desc description:string
          #:player-names [player-names:str ...]
          #:player-tiles player-tiles
          #:externals    externals
          #:ref-tiles    ref-tiles
          #:ref-map      map0
          #:expected     [[winners:str ...] [drop-outs:str ...]]
          #:kind         kind:id
          (~optional (~seq #:quiet quiet) #:defaults ([quiet #'#true]))
          (~optional (~seq #:show show) #:defaults ([show #'#false])))
       #'(begin
           [define L       description]
           [define specs   (map list player-tiles (list player-names ...))]
           [define state   (create-ref-state map0 specs #:tiles0 ref-tiles)]
           [define config  (dict-set (create-config state #:observe textual-observer) QUIET quiet)]
           [define expect  `[,(list winners ...) ,(list drop-outs ...)]]
           ;; the Racket integration test

           #; {case-> [-> Void] [(-> Void) -> Void]}
           (define name
             (case-lambda
               [() [name-plain]]
               [(main expect) [name-jsexpr main expect]]))

           #; {-> Void}
           (define [name-plain]
             (parameterize ([unit-test-mode #false])
               (check-equal? (referee/config config externals) expect L)
               (when show
                 (for-each (compose pretty-print render-ref-state) (textual-observer #false)))
               (textual-observer FLUSH)))

           #; {[ -> Void] -> Void}
           (define [name-jsexpr main expect]
             (define jsexpr-inputs [list (state->jsexpr state) (map player->jsexpr externals)])
             (r-check-equal? main jsexpr-inputs `[,expect] L))

           (set! kind (cons name kind)))]))

  (define (plain-main . x)
    (write-json `[["A"] []] #:indent 2)
    (newline)))
;; ---------------------------------------------------------------------------------------------------

(module+ examples
  (integration-test
   normal-short
   #:desc "two normal players, 1 turn"
   #:player-names ["A" "B"]
   #:player-tiles `[,tiles1 ,tiles1]
   #:externals    (take dag-player* 2)
   #:ref-tiles    tiles0
   #:ref-map      map0
   #:expected     [["A"] []]
   #:kind         for-students)

  (integration-test
   normal-medium
   #:desc "four normal players, several turns"
   #:player-names ["A" "B" "C" "D"]
   #:player-tiles (list starter-tile* 1starter-tile* 2starter-tile* 3starter-tile*)
   #:externals    (take dag-player* 4)
   #:ref-tiles    starter-tile*
   #:ref-map      (start-map #s(tile clover yellow))
   #:expected     [["B"] []]
   #:kind         for-students)

  (integration-test
   normal-medium2
   #:desc "four normal players, several turns"
   #:player-names ["A" "B" "C" "D"]
   #:player-tiles (list starter-tile* 1starter-tile* 2starter-tile* 3starter-tile*)
   #:externals    (take dag-player* 4)
   #:ref-tiles    ALL-SHAPE-COLOR-COMBOS
   #:ref-map      (start-map #s(tile clover yellow))
   #:expected     [["D"] []]
   #:kind         for-students
   ; #:quiet #false
   ; #:show 'yes
   )

  (define bad-7 (retrieve-factory "setup" factory-table-7))
  (integration-test
   bad-setup 
   #:desc "one normal player, one drop out: 1 turn"
   #:player-names ["A" "B"]
   #:player-tiles (list tiles1 tiles1)
   #:externals    (append (take dag-player* 1) `[,(create-player "B" dag-strategy #:bad bad-7)])
   #:ref-tiles    starter-tile*
   #:ref-map      map0
   #:expected     [["A"] ["B"]]
   #:kind         for-students)
  )

;                                                                        
;                                                                        
;      ;            ;                    ;                    ;          
;                   ;                    ;                    ;          
;    ;;;   ; ;;   ;;;;;                ;;;;;   ;;;    ;;;   ;;;;;   ;;;  
;      ;   ;;  ;    ;                    ;    ;;  ;  ;   ;    ;    ;   ; 
;      ;   ;   ;    ;                    ;    ;   ;; ;        ;    ;     
;      ;   ;   ;    ;                    ;    ;;;;;;  ;;;     ;     ;;;  
;      ;   ;   ;    ;                    ;    ;          ;    ;        ; 
;      ;   ;   ;    ;     ;;             ;    ;      ;   ;    ;    ;   ; 
;    ;;;;; ;   ;    ;;;   ;;             ;;;   ;;;;   ;;;     ;;;   ;;;  
;                                                                        
;                                                                        
;                                                                        

(module+ test ;; run all integration tests
  '---------------------------------------------------------------------------------------------------

  (for-each (λ (test) [test]) [all-tests])

  (check-equal? (length [all-tests]) 4 "make sure all tests are recordded")

  (define expected  #; "because this succeeds, not because it's correct"  `{["A"] []})
  (for-each (λ (test) [test plain-main expected]) for-students))