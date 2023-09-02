#lang racket

;; referee: mediates between external players (local or remote) and the referee state, via safe xsend 
;; ---------------------------------------------------------------------------------------------------

(require (submod (lib "Qwirkle/scribblings/qwirkle.scrbl") spec))
(require Qwirkle/Common/player-interface)
(require SwDev/Contracts/unique)
(require SwDev/Lib/hash-contract)

(define STATE0   'initial-state)
(define QUIET    'quiet)
(define PER-TURN 'time-per-turn)
(define OBSERVE  'observe)
(define SHOW     'observer-shows-for-s)
(define QBO      'q-bonus)
(define FBO      'finish-bonus)
(define options  (list STATE0 QUIET OBSERVE SHOW PER-TURN QBO FBO))

(define unit-test-mode [make-parameter #false])

#; {Configuration [Listof Player] -> Boolean}
(define (matching-number config players)
  (let* ([state0  (dict-ref config STATE0)]
         [sop#    (and state0 (player-count state0))]
         [player# (length players)])
    (or (unit-test-mode)
        (and (= sop# player#) (<= MIN-PLAYERS sop# MAX-PLAYERS)))))

(define config/c (hash-carrier/c options))

(provide
 #; {type Configuration = [Hashtable Options]}
 ;; config options
 STATE0 QUIET OBSERVE SHOW PER-TURN
 
 (contract-out
  [create-config
   ;; create a default configuration from a referee state 
   (->* (state?) (#:observe any/c #:per-turn (and/c real? positive?)) config/c)]

  [set-bonus
   #; (set-bonus c Q-BONUS FINISH-BONUS)
   (-> config/c natural? natural? config/c)]
  
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
        #:pre/name (players) "players must have distince names"
        (distinct? (map (λ (p) (send p name)) players))
        #:pre/name (config players) "matching number of players"
        (matching-number config players)
        (r [list/c [listof string?] [listof string?]]))]))

(module+ examples
  (provide
   for-students-7 for-tests-7
   for-students-9 for-tests-9
   for-bonus-A

   #; {[Listof ->]}
   all-tests))

(module+ examples
  (provide
   textual-observer
   FLUSH))

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
  (require (submod ".."))
  (require (submod Qwirkle/Common/map examples))
  (require (submod Qwirkle/Common/tiles examples))
  (require (submod Qwirkle/Player/mechanics json))
  (require (submod Qwirkle/Referee/ref-state json))
  
  (require Qwirkle/Common/map)
  (require Qwirkle/Common/tiles)
  (require Qwirkle/Player/mechanics)
  (require Qwirkle/Player/strategies)
  (require Qwirkle/Lib/check-message)
  (require Qwirkle/Lib/fixed-perm)

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
  (require json)
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

(define per-turn 0.1)  ;; time limit per turn
(define (set-per x) (set! per-turn x)) ;; for the testing submod 
(define quiet    [make-parameter #false])
(define (set-with x) (set! with-obs x))
(define with-obs #false)
(define obs-show 1)

#; {State -> Configuration}
(define (create-config j #:observe (o #false) #:per-turn (pt #f))
  (hash
   OBSERVE  o
   SHOW     1
   STATE0   j
   QUIET    #true
   QBO      Q-BONUS-7
   FBO      FINISH-BONUS-7
   PER-TURN (or pt per-turn)))

#; {Configuration Natural Natural -> Configuration}
(define (set-bonus config QB FB)
  (dict-set (dict-set config FBO FB) QBO QB))

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
  (define QB (dict-ref c QBO Q-BONUS-7))
  (define FB (dict-ref c FBO FINISH-BONUS-7))
  (referee/state s #:with-obs o #:QB QB #:FB FB))

#; {State {#:with-obs (U False Observer)} -> Result}
;; This is the workhorse of REFEREES.
;; ASSUME the given state is not finished (at least one player, no winner)
(define (referee/state s0 #:with-obs (wo #false) #:QB (QB Q-BONUS-7) #:FB (FB FINISH-BONUS-7))
  [time-out-limit per-turn]
  (dynamic-wind (setup-observer wo) (referee/state-proper s0 QB FB) (unset-observer wo)))

#; {State -> Result}
(define [(referee/state-proper s0 QB FB)]
  (parameterize ([current-error-port (if [quiet] (open-output-string) (current-error-port))]
                 [Q-BONUS            QB]
                 [FINISH-BONUS       FB])
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
           (parameterize ([unit-test-mode #true])
             (let* ([run (λ (x) (~? (referee/state x) (referee/state x #:with-obs O)))]
                    (P (create-player "A" dag-strategy #:bad (retrieve-factory name factory)))
                    [S (set-ref-state-players state [cons P (~? (list B) '[])])])
               [quiet #false]
               (check-equal? (check-message msg cep #px"dropped" (run S)) (list win out) msg))
             ;; --- to cover the default #:with-obs clause 
             (let* ([config (λ (s) (~? (create-config s #:observe O) (create-config s)))]
                    [C (dict-set (config state) QUIET Q)]
                    (P (create-player "A" dag-strategy #:bad (retrieve-factory name factory)))
                    [run (λ () (referee/config C (cons P (~? (list B) '[]))))])
               (check-equal?
                (if Q [run] (check-message msg cep #px"dropped" [run])) `(,win ,out) msg))))])))

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
      [(test-case title:string n:string factory:id state:expr
                  [new-state dropped-out]
                  (~optional (~seq #:dropped  e:string))
                  (~seq #:extra Bname:id Bplayer:expr))
       #:with S (datum->syntax #'test-case 'S #'test-case #'test-case)
       #:with A (datum->syntax #'test-case 'A #'test-case #'test-case)
       #'(parameterize ([unit-test-mode #true])
           (let* ((A (create-player (~a "A-" title) dag-strategy #:bad (retrieve-factory n factory)))
                  [Bname Bplayer]
                  [S (set-ref-state-players state [list A Bname])])
             (~?
              (define-values [S++ out] (check-message title cep (pregexp e) (setup S)))
              (define-values [S++ out] [setup S]))
             (check-values (values S++ (map sop-player out)) new-state dropped-out title)))])))

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
       #'(parameterize ([unit-test-mode #true])
           (let (~? ([Bname B]) ())
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
                 (set-with #false)))))])))

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
       #'(parameterize ([unit-test-mode #true])
           (let (~? ([Bname B]) ())
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
                       (dev/null [one-round S '[]])))
                 (check-equal? end end W)
                 (if ok (check-true (state? S++) W) (check-false S++ W))
                 (if dr
                     (check-equal? (map sop-player out) `[,P] (~a "A drop out" W))
                     (check-equal? out '[] (~a "no:" W)))
                 (set-with #false)))))])))

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
                   (~optional (~seq #:dropped? dr #;symbol) #:defaults ([dr #'#false]))]
                  (~optional (~seq #:extra Bname:id B:expr)))
       #'(parameterize ([unit-test-mode #true])
           (let* ([F (retrieve-factory name factory)]
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
                     (dev/null [run A S '[]])))
               (~? (check-true end W) (void))
               (if ok (check-true (state? S++) W) (check-false S++ W))
               (if ps (check-true (unbox passed) W) (check-false (unbox passed) W))
               (if dr
                   (check-equal? (map sop-player out) `[,P] (~a "A drop out" W))
                   (check-equal? out '[] (~a "no:" W)))
               (set-with #false)
               (set-box! passed #true #;"restore old value"))))])))

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
  (for/fold ([survived '()] [out out0] #:result (values survived out)) ([p lop])
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

  (provide dag-player* ldasg-player* exn-player* inf-player*)

  (define dag-player*
    (let* ([f (retrieve-factory "good" factory-base)]
           [A (create-player "A" dag-strategy #:bad f)]
           [B (create-player "B" dag-strategy #:bad f)]
           [C (create-player "C" dag-strategy #:bad f)]
           [D (create-player "D" dag-strategy #:bad f)])
      (list A B C D)))

  (define ldasg-player*
    (let* ([f (retrieve-factory "good" factory-base)]
           [A (create-player "E" ldasg-strategy #:bad f)]
           [B (create-player "F" ldasg-strategy #:bad f)]
           [C (create-player "G" ldasg-strategy #:bad f)]
           [D (create-player "H" ldasg-strategy #:bad f)])
      (list A B C D)))

  (define (from-7 name) (retrieve-factory name factory-table-7))
  (define exn-player*
    (let* ([A (create-player "xnX" ldasg-strategy #:bad (from-7 "setup"))]
           [B (create-player "xnY" ldasg-strategy #:bad (from-7 "take-turn" ))]
           [C (create-player "xnZ" ldasg-strategy #:bad (from-7 "new-tiles" ))]
           [D (create-player "xnW" ldasg-strategy #:bad (from-7 "win" ))])
      (list A B C D)))

  (define (from-8 name) (retrieve-factory name factory-table-8))
  (define inf-player*
    (let* ([A (create-player "infK"  ldasg-strategy #:bad (from-8 "setup-1"))]
           [B (create-player "infL2" ldasg-strategy #:bad (from-8 "take-turn-2"))]
           [C (create-player "infL5" ldasg-strategy #:bad (from-8 "take-turn-4"))]
           [D (create-player "infM7" ldasg-strategy #:bad (from-8 "new-tiles-7"))]
           [E (create-player "infM3" ldasg-strategy #:bad (from-8 "new-tiles-3"))]
           [F (create-player "infO"  ldasg-strategy #:bad (from-8 "win-1"))])
      (list A B C D E F)))

  (define badly-named-player*
    (let* ([f (retrieve-factory "good" factory-base)]
           [A (create-player "ouch ouch" dag-strategy #:bad f)]
           [B (create-player "ouchouchouchouchouchouchouchouchouch" dag-strategy #:bad f)])
      (list A B)))
    
  (define for-students-7 '[])
  (define for-tests-7    '[])

  (define for-students-9 '[])
  (define for-tests-9    '[])
  
  (define for-bonus-A    '[])
  
  (define [all-tests]
    (append for-students-7 for-tests-7 for-students-9 for-tests-9
            for-bonus-A))

  ;; this could be a procedure if (1) the for-*:id were bound to boxed,
  ;; (2) I add a ` to each `#:expected`argument, & (3) always write (define name (integration-test ..)
  (define-syntax (define-integration-test stx)
    (syntax-parse stx
      [(_ name:id
          #:desc         description:string
          #:player-tiles player-tiles
          #:externals    externals
          #:ref-tiles    tiles0
          #:ref-map      map0
          #:expected     [[winners:str ...] [drop-outs:str ...]]
          (~optional (~seq #:q-bonus qb) #:defaults ([qb #'[Q-BONUS]]))
          (~optional (~seq #:finish-bonus fb) #:defaults ([fb #'[FINISH-BONUS]]))
          (~optional     (~seq #:extras xtras) #:defaults ([xtras #''[]]))
          #:kind         kind:id
          (~optional     (~seq #:quiet quiet) #:defaults ([quiet #'#true]))
          (~optional     (~seq #:show show)   #:defaults ([show #'#false])))
       #'(begin
           (define name
             (let ([descr  (~a 'name " " description)]
                   [expect [list (list winners ...) (list drop-outs ...)]])
               (setup-tsts descr player-tiles externals xtras tiles0 map0 expect quiet show qb fb)))
           (set! kind (cons name kind)))]))

  (define (setup-tsts L player-tiles externals xtras tiles0 map0 expect quiet show QB FB)
    [define specs   (map list player-tiles (take '["xnX" "xnY" "xnZ" "xnW"] (length player-tiles)))]
    [define state   (create-ref-state map0 specs #:tiles0 tiles0)]
    [define config
      (set-bonus (dict-set (create-config state #:observe textual-observer) QUIET quiet) QB FB)]
    
    ;; the Racket integration test

    #; {case-> [-> Void] [(-> Void) -> Void]}
    (define name
      (case-lambda
        [() [name-plain]]
        ;; the next two clauses are for running the JSON integration tests 
        [(main) (name-jsexpr main)]
        [(main expect) [name-jsexpr main expect]]
        ;; this clause exists to running tthe server/client setup
        ;; `xtras` are players that misbehave in some way before the game starts 
        [(main _server _client) (main config (append xtras externals) expect)]))

    #; {-> Void}
    (define [name-plain]
      (parameterize ([unit-test-mode #false])
        (check-equal? (referee/config config externals) expect L)
        (when show
          (for-each (compose pretty-print render-ref-state) (textual-observer #false)))
        (textual-observer FLUSH)))

    #; {[ -> Void] -> Void}
    (define [name-jsexpr main (expect expect)]
      (define jsexpr-inputs [list (state->jsexpr state) (player*->jsexpr externals)])
      (parameterize ([unit-test-mode #false])
        (r-check-equal? main jsexpr-inputs `[,expect] L)))
  
    name))
;; ---------------------------------------------------------------------------------------------------

(module+ examples ;; for milestone 7 ;; ASSUME the bonus parameters are set to the -7 values
  (define-integration-test dag-only-short
    #:desc "two dag players, 1 turn"
    #:player-tiles `[,tiles1 ,tiles1]
    #:externals    (take dag-player* 2)
    #:ref-tiles    tiles0
    #:ref-map      map0
    #:expected     [["A"] []]
    #:kind         for-students-7)

  (define-integration-test dag-only-short-A
    #:desc "two dag players, 1 turn"
    #:player-tiles `[,tiles1 ,tiles1]
    #:externals    (take dag-player* 2)
    #:ref-tiles    tiles0
    #:ref-map      map0
    #:expected     [["A"] []]
    #:extras       badly-named-player*
    #:kind         for-bonus-A)

  (define-integration-test bad-setup 
    #:desc "one dag player, one drop out: 1 turn"
    #:player-tiles (list tiles1 tiles1)
    #:externals    (append (take dag-player* 1) `[,(first exn-player*)])
    #:ref-tiles    starter-tile*
    #:ref-map      map0
    #:expected     [["A"] ["xnX"]]
    #:kind         for-students-7)

  (define-integration-test dag-only-medium
    #:desc "four dag players, several turns"
    #:player-tiles (list starter-tile* 1starter-tile* 2starter-tile* 3starter-tile*)
    #:externals    (take dag-player* 4)
    #:ref-tiles    starter-tile*
    #:ref-map      (start-map #s(tile clover yellow))
    #:expected     [["B"] []]
    #:kind         for-students-7)

  (define-integration-test
    mixed-medium2
    #:desc "two dag players, two ldasg player, many turns"
    #:player-tiles (list starter-tile* 1starter-tile* 2starter-tile* 3starter-tile*)
    #:externals    (append (take dag-player* 2) (take ldasg-player* 2))
    #:ref-tiles    ALL-SHAPE-COLOR-COMBOS
    #:ref-map      (start-map #s(tile clover yellow))
    #:expected     [["F"] []]
    #:kind         for-tests-7)

  (define-integration-test dag-only-medium2
    #:desc "four dag players, many turns"
    #:player-tiles (list starter-tile* 1starter-tile* 2starter-tile* 3starter-tile*)
    #:externals    (take dag-player* 4)
    #:ref-tiles    ALL-SHAPE-COLOR-COMBOS
    #:ref-map      (start-map #s(tile clover yellow))
    #:expected     [["D"] []]
    #:kind         for-tests-7)
  
  (define-integration-test mixed-medium2-rev-players
    #:desc "two dag players, two ldasg player; revversed players: does the player order matter"
    #:player-tiles (list starter-tile* 1starter-tile* 2starter-tile* 3starter-tile*)
    #:externals    (reverse (append (take dag-player* 2) (take ldasg-player* 2)))
    #:ref-tiles    ALL-SHAPE-COLOR-COMBOS
    #:ref-map      (start-map #s(tile clover yellow))
    #:expected     [["E" "F"] []]
    #:q-bonus      Q-BONUS-7
    #:finish-bonus FINISH-BONUS-7
    #:kind         for-tests-7)

  (define-integration-test mixed-medium2-rev-players-bonus-delta
    #:desc "two dag players, two ldasg player; revversed players: does the player order matter ΔBONUS"
    #:player-tiles (list starter-tile* 1starter-tile* 2starter-tile* 3starter-tile*)
    #:externals    (reverse (append (take dag-player* 2) (take ldasg-player* 2)))
    #:ref-tiles    ALL-SHAPE-COLOR-COMBOS
    #:ref-map      (start-map #s(tile clover yellow))
    #:expected     [["F"] []]
    #:q-bonus      Q-BONUS-8 
    #:finish-bonus FINISH-BONUS-8
    #:kind         for-tests-9)

  (define-integration-test mixed-medium2-rev-tiles 
    #:desc "two dag players, two ldasg player; revversed tiles: does the tile order matter"
    #:player-tiles (list starter-tile* 1starter-tile* 2starter-tile* 3starter-tile*)
    #:externals    (append (take dag-player* 2) (take ldasg-player* 2))
    #:ref-tiles    (reverse ALL-SHAPE-COLOR-COMBOS)
    #:ref-map      (start-map #s(tile clover yellow))
    #:expected     [["B"] []]
    #:kind         for-tests-7)

  (define-integration-test mixed-medium2-rev-both
    #:desc "two dag players, two ldasg player; revversed tiles and players: does the order matter"
    #:player-tiles (list starter-tile* 1starter-tile* 2starter-tile* 3starter-tile*)
    #:externals    (reverse (append (take dag-player* 2) (take ldasg-player* 2)))
    #:ref-tiles    (reverse ALL-SHAPE-COLOR-COMBOS)
    #:ref-map      (start-map #s(tile clover yellow))
    #:expected     [["E"] []]
    #:kind         for-tests-7)
  
  (define-integration-test bad-all
    #:desc "all drop out, in the epxected order"
    #:player-tiles (list 2starter-tile* 3starter-tile* starter-tile* 3starter-tile*)
    #:externals    exn-player*
    #:ref-tiles    (reverse ALL-SHAPE-COLOR-COMBOS)
    #:ref-map      map0
    #:expected     [[] ["xnX" "xnY" "xnZ" "xnW"]]
    #:kind         for-tests-7)

  (define-integration-test bad-all-1
    #:desc "surprise: one survuves"
    #:player-tiles (list tiles1 tiles1 tiles1 tiles1)
    #:externals    exn-player*
    #:ref-tiles    (reverse ALL-SHAPE-COLOR-COMBOS)
    #:ref-map      map0
    #:expected     [["xnZ"] ["xnX" "xnY" "xnW"]]
    #:kind         for-tests-7)

  (define-integration-test bad-all-tiles-bad-players
    #:desc "surprise: one survuves"
    #:player-tiles (list tiles1 tiles1 tiles1 tiles1)
    #:externals    exn-player*
    #:ref-tiles    ALL-TILES
    #:ref-map      map0
    #:expected     [["xnZ"] ["xnX" "xnY" "xnW"]]
    #:kind         for-tests-7)
  
  (define-integration-test mixed-all-tiles
    #:desc "two dag players, two ldasg player; revversed tiles and players: does the order matter"
    #:player-tiles (list starter-tile* 1starter-tile* 2starter-tile* 3starter-tile*)
    #:externals    (reverse (append (take dag-player* 2) (take ldasg-player* 2)))
    #:ref-tiles    ALL-TILES
    #:ref-map      (start-map #s(tile clover yellow))
    #:expected     [["A"] []]
    #:kind         for-tests-7)

  (define-integration-test mixed-all-tiles-perm
    #:desc "two dag players, two ldasg player; permuted tiles and players: does the order matter"
    #:player-tiles (list starter-tile* 1starter-tile* 2starter-tile* 3starter-tile*)
    #:externals    (append (take dag-player* 2) (take ldasg-player* 2))
    #:ref-tiles    ALL-TILES-PERM
    #:ref-map      (start-map #s(tile clover yellow))
    #:expected     [["B"] []]
    #:kind         for-tests-7))

;; ---------------------------------------------------------------------------------------------------
(module+ examples ;; for milestone 8 ; more 8 above
  
  (define-integration-test bad-all-tiles-bad-players-inf
    #:desc "surprise: one survuves because it can finish the game with the first take-turn"
    #:player-tiles (list tiles1 tiles1 tiles1 tiles1)
    #:externals    (take inf-player* MAX-PLAYERS)
    #:ref-tiles    ALL-TILES
    #:ref-map      map0
    #:expected     [["infL2"] ["infK"]]
    #:q-bonus      Q-BONUS-8 
    #:finish-bonus FINISH-BONUS-8
    #:kind         for-students-9)

  (define-integration-test bad-all-tiles-bad-players-inf-lsdag
    #:desc "the inf-winner drops out; nobody else wins"
    #:player-tiles (list starter-tile* 1starter-tile* 2starter-tile* 3starter-tile*)
    #:externals    (append (take (reverse inf-player*) MIN-PLAYERS) (take ldasg-player* MIN-PLAYERS))
    #:ref-tiles    ALL-TILES
    #:ref-map      map0
    #:expected     [[] ["infM3" "infO"]]
    #:q-bonus      Q-BONUS-8 
    #:finish-bonus FINISH-BONUS-8
    #:kind         for-students-9)

  (define-integration-test bad-all-tiles-bad-players-8-lsdag
    #:desc "reminder: plain tests are fair game (ldasg)"
    #:player-tiles (list starter-tile* 1starter-tile* 2starter-tile* 3starter-tile*)
    #:externals    (take ldasg-player* MAX-PLAYERS)
    #:ref-tiles    ALL-TILES
    #:ref-map      map0
    #:expected     [["G"] []]
    #:q-bonus      Q-BONUS-8 
    #:finish-bonus FINISH-BONUS-8
    #:kind         for-students-9)

  (define-integration-test bad-all-tiles-bad-players-8-lsdag-A
    #:desc "reminder: plain tests are fair game (ldasg), plus extras"
    #:player-tiles (list starter-tile* 1starter-tile* 2starter-tile* 3starter-tile*)
    #:externals    (take ldasg-player* MAX-PLAYERS)
    #:ref-tiles    ALL-TILES
    #:ref-map      map0
    #:expected     [["G"] []]
    #:q-bonus      Q-BONUS-8 
    #:finish-bonus FINISH-BONUS-8
    #:extras       badly-named-player*
    #:kind         for-bonus-A)
  
  (define-integration-test bad-all-tiles-ldasg-player*
    #:desc "reminder: lsdag players only, all get the same tiles but different from preceding test"
    #:player-tiles (list tiles1 tiles1 tiles1 tiles1)
    #:externals    ldasg-player*
    #:ref-tiles    ALL-TILES
    #:ref-map      map0
    #:expected     [["E"] []]
    #:q-bonus      Q-BONUS-8 
    #:finish-bonus FINISH-BONUS-8
    #:kind         for-tests-9)
  
  (define-integration-test bad-all-tiles-dag-player*
    #:desc "reminder: plain tests are fair game (dag)"
    #:player-tiles (list tiles1 tiles1 tiles1 tiles1)
    #:externals    dag-player*
    #:ref-tiles    ALL-TILES
    #:ref-map      map0
    #:expected     [["A"] []]
    #:q-bonus      Q-BONUS-8 
    #:finish-bonus FINISH-BONUS-8
    #:kind         for-tests-9)

  (define-integration-test mixed-all-tiles-rev
    #:desc "two dag players, two ldasg player; permuted reversed tiles,rev players: order matters?"
    #:player-tiles (list starter-tile* 1starter-tile* 2starter-tile* 3starter-tile*)
    #:externals    (reverse (append (take dag-player* 2) (take ldasg-player* 2)))
    #:ref-tiles    (pick-fixed-permutation (reverse ALL-TILES))
    #:ref-map      (start-map #s(tile clover yellow))
    #:expected     [["B"] []]
    #:q-bonus      Q-BONUS-8 
    #:finish-bonus FINISH-BONUS-8
    #:kind         for-tests-9)

  (define-integration-test mixed-all-tiles-rev-inf1
    #:desc "two inf players, two ldasg player; permuted reversed tiles,rev players: order matters?"
    #:player-tiles (list starter-tile* 1starter-tile* 2starter-tile* 3starter-tile*)
    #:externals    (reverse (append (take inf-player* 2) (take ldasg-player* 2)))
    #:ref-tiles    (pick-fixed-permutation (reverse ALL-TILES))
    #:ref-map      (start-map #s(tile clover yellow))
    #:expected     [["E"] ["infK" "infL2"]]
    #:q-bonus      Q-BONUS-8 
    #:finish-bonus FINISH-BONUS-8
    #:kind         for-tests-9)

  (define-integration-test mixed-all-tiles-rev-inf2
    #:desc "three inf players from back, one lsdag"
    #:player-tiles (list starter-tile* 1starter-tile* 2starter-tile* 3starter-tile*)
    #:externals    (append (take (reverse inf-player*) 3) (take ldasg-player* 1))
    #:ref-tiles    (pick-fixed-permutation (reverse ALL-TILES))
    #:ref-map      (start-map #s(tile clover yellow))
    #:expected     [[] ["infM3" "infM7" "infO"]]
    #:q-bonus      Q-BONUS-8 
    #:finish-bonus FINISH-BONUS-8
    #:kind         for-tests-9)

  (define-integration-test mixed-all-tiles-rev-inf3
    #:desc "all inf players with permutation tiles"
    #:player-tiles (list starter-tile* 1starter-tile* 2starter-tile* 3starter-tile*)
    #:externals    (take (reverse inf-player*) MAX-PLAYERS)
    #:ref-tiles    (pick-fixed-permutation (reverse ALL-TILES))
    #:ref-map      (start-map #s(tile clover yellow))
    #:expected     [[] ["infM3" "infL5" "infM7" "infO"]]
    #:q-bonus      Q-BONUS-8 
    #:finish-bonus FINISH-BONUS-8
    #:kind         for-tests-9)

  (define the-exn (list-ref exn-player* 3))
  (define-integration-test mixed-all-tiles-rev-inf-exn
    #:desc "an exn player with all inf players"
    #:player-tiles (list starter-tile* 1starter-tile* 2starter-tile* 3starter-tile*)
    #:externals    (cons the-exn (take (reverse inf-player*) (- MAX-PLAYERS 1)))
    #:ref-tiles    (pick-fixed-permutation (reverse ALL-TILES))
    #:ref-map      (start-map #s(tile clover yellow))
    #:expected     [[] ["infM3" "infM7" "xnW" "infO"]]
    #:q-bonus      Q-BONUS-8 
    #:finish-bonus FINISH-BONUS-8
    #:kind         for-tests-9)

  (define-integration-test mixed-all-tiles-rev-inf-exn-dag
    #:desc "one dag, one exn, two infs"
    #:player-tiles (list starter-tile* 1starter-tile* 2starter-tile* 3starter-tile*)
    #:externals    (list* (first dag-player*) the-exn (take (reverse inf-player*) (- MAX-PLAYERS 2)))
    #:ref-tiles    (pick-fixed-permutation (reverse ALL-TILES))
    #:ref-map      (start-map #s(tile clover yellow))
    #:expected     [["A"] ["infM3" "xnW" "infO"]]
    #:q-bonus      Q-BONUS-8 
    #:finish-bonus FINISH-BONUS-8
    #:kind         for-tests-9)

  (define t-mixed-all-tiles-rev-inf-exn-dag2 (pick-fixed-permutation (reverse ALL-TILES)))
  (define-integration-test mixed-all-tiles-rev-inf-exn-dag2
    #:desc "3 players: one dag"
    #:player-tiles (list 1starter-tile* 2starter-tile* 3starter-tile*)
    #:externals    (list (first dag-player*) the-exn (second inf-player*))
    #:ref-tiles    t-mixed-all-tiles-rev-inf-exn-dag2
    #:ref-map      (start-map #s(tile clover yellow))
    #:expected     [["A"] ["infL2" "xnW"]]
    #:q-bonus      Q-BONUS-8 
    #:finish-bonus FINISH-BONUS-8
    #:kind         for-tests-9)

  (provide mixed-all-tiles-rev-inf-exn-dag2-A)

  (define t-mixed-all-tiles-rev-inf-exn-dag2-A (pick-fixed-permutation (reverse ALL-TILES)))
  (define-integration-test mixed-all-tiles-rev-inf-exn-dag2-A
    #:desc "3 players: one dag"
    #:player-tiles (list 1starter-tile* 2starter-tile* 3starter-tile*)
    #:externals    (list (first dag-player*) the-exn (second inf-player*))
    #:ref-tiles    t-mixed-all-tiles-rev-inf-exn-dag2-A
    #:ref-map      (start-map #s(tile clover yellow))
    #:expected     [[] ["infL2" "xnW"]]
    #:q-bonus      Q-BONUS-8 
    #:finish-bonus FINISH-BONUS-8
    #:kind         for-bonus-A))

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
  (for-each (λ (test) [test]) [all-tests])

  (check-equal? (length [all-tests]) (+ 10 3 10 3 3) "make sure all tests are recordded")
  (check-equal? (length for-students-7) 3 "7: students get three tests")
  (check-equal? (length for-students-9) 3 "8: students get three tests -- expected to fail")
  (check-equal? (length for-tests-7) 10 "7: we run students' code on ten tests")
  (check-equal? (length for-tests-9) 10 "8: we run students' code on ten tests")

  ;; this test just ensures that the jsexpr entry point is run in a dummy way
  (define (plain-main-jsexpr . x) (write-json `[["A"] []] #:indent 2) (newline))
  (define expected  #; "because this succeeds, not because it's correct"  `{["A"] []})
  (for-each (λ (test) [test plain-main-jsexpr expected]) for-students-7)

  ;; this test just ensures that the server/client entry point works 
  (for-each (λ (test) (test list 1 2)) for-students-9))