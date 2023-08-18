#lang racket

;; referee: mediates between external players (local or remote) and the game state, via safe xsend 
;; ---------------------------------------------------------------------------------------------------

(define results/c [list/c [listof string?] [listof string?]])

(define STATE0   'initial-state)
(define QUIET    'quiet)
(define PER-TURN 'time-per-turn)
(define OBSERVE  'observe)
(define SHOW     'observer-shows-for-s)
(define options  (list STATE0 QUIET OBSERVE SHOW PER-TURN))

(provide
 MAX-PLAYERS 
 MIN-PLAYERS

 PER-TURN 

 (contract-out
  [create-config
   ;; create a default configuration from a referee state 
   (->* (state?) (#:observe any/c #:per-turn (and/c real? positive?)) (hash-carrier/c options))]
  
  [referee/config
   ;; use a configuration of a referee state,
   ;; plus possibly options to set the time limit per turn 
   (->i ([c (hash-carrier/c options)] [players (listof player/c)])
        #:pre/name (players) "distince names" (distinct? (map (λ (p) (send p name)) players))
        (r results/c))]
  
  [referee/state
   (->* (state?) (#:with-obs any/c) results/c)]))

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

;; should not use this: 
(require Qwirkle/Common/game-state)


(require Qwirkle/Common/map)
(require Qwirkle/Common/state-of-player)
(require Qwirkle/Common/player-interface)
(require Qwirkle/Player/strategies)
(require Qwirkle/Referee/ref-state)
(require SwDev/Contracts/unique)
(require SwDev/Lib/hash-contract)

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
  (require (prefix-in mech: Qwirkle/Player/mechanics)))

(module+ test
  (define cep current-error-port)

  (require (submod ".."))
  (require (submod ".." examples))
  (require (submod Qwirkle/Common/game-state examples))
  (require (submod Qwirkle/Referee/ref-state examples))
  (require (submod Qwirkle/Common/map examples))
  (require (submod Qwirkle/Common/tiles examples))
  (require Qwirkle/Player/mechanics)
  (require Qwirkle/Common/coordinates)
  (require Qwirkle/Lib/check-message)
  (require SwDev/Testing/check-values)
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

(define MAX-PLAYERS 4)
(define MIN-PLAYERS 2)

(define per-turn 4.0)  ;; time limit per turn
(define (set-per x) (set! per-turn x)) ;; for the testing submod 
(define quiet    [make-parameter #false])
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
;; the first list contains the name of winning players, the second is the list of misbehaving players

#; {[Listof [Instanceof Player]] [Hashtable options] -> Result}
;; `lo-players` must list the player in the order in which they get into the state 
(define (referee/config c lo-players)
  (define-values (s o) (install-config c lo-players))
  (referee/state s #:with-obs o))

#; {State {#:with-obs (U False Observer)} -> Result}
;; This is the workhorse of REFEREES.
;; ASSUME the given state is not finished (at least one player, no winner)
(define (referee/state s0 #:with-obs (wo #false))
  [time-out-limit per-turn]
  (dynamic-wind (setup-observer s0 wo) (referee/state-proper s0) (unset-observer wo)))

#; {State -> Result}
(define [(referee/state-proper s0)]
  (parameterize ([current-error-port (if [quiet] (open-output-string) (current-error-port))])
    (let*-values ({[state out]          (setup s0)}
                  {[winners+losers out] (if (false? state) (values '[[] []] out) (rounds state out))}
                  ([winners0 losers0]   (apply values winners+losers))
                  {[true-winners out]   (inform-about-outcome winners0 #true out)}
                  {[_true-losers out]   (inform-about-outcome losers0 #false out)})
      (list (extract-names true-winners) (extract-names out)))))

#; {State (U False Observer) -> Void}
;; turn with-obs into a function that consumes a state
(define [(setup-observer s0 wo)]
  (define is (or with-obs wo))
  (set! with-obs (if is (is s0) void)))

#; {(U False Observer) -> Void}
(define [(unset-observer wo)]
  (when wo (sleep obs-show))
  (with-obs #false)
  (set! with-obs #false))

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
                  (P (create-player "A" dag-strategy #:bad (retrieve-factory name factory)))
                  [C (dict-set (config state) QUIET Q)]
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

(module+ test 

  ;; a primitive textual observer 
  (define seq* '[])
  (define (textual-observer s)
    (cond
      [(false? s) seq*]
      [else (set! seq* (cons s seq*))
            textual-observer]))

  (ref-test-case "receiving new tiles fails, cover observer"
                 "new-tiles" factory-table-7 (active-sop-hand state1 (list #s(tile clover red)))
                 ['[] '["A"]]
                 #:with-obs textual-observer)
  (check-equal? (length (textual-observer #false)) 1))
    

;; TODO:
;; DEFAULT RESULT 
;; -- state-kick and state-take-back seem to have overlapping functionality : CHECK
;; -- no state-players

;; -- match/struct* and struct-copy 


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
      [game-over? (values (determine-winners s+) out+)]
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
                    [S (set-ref-state-players T [cons A C])]
                    [players (state-players S)])
               (define-values [W O] (check-message title cep #px"dropped" (rounds S '[])))
               (define awinners (map (λ (x) (send (sop-player x) name)) (first W)))
               (define alosers (map sop-player (second W)))
               (check-equal? `[,awinners ,alosers] `[,winners ,losers] (~a "winners " L))
               (check-equal? (map sop-player O) out (~a "drop outs " L)))))])))

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
               (define-values [S++ end out]
                 (if dr (check-message W cep #px"dropped out" (one-round S '[])) [one-round S '[]]))
               (check-equal? end end W)
               (if ok (check-true (state? S++) W) (check-false S++ W))
               (if dr
                   (check-equal? out `[,(first (state-players S))] (~a "A drop out" W))
                   (check-equal? out '[] (~a "no:" W))))))])))

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
  (xsend+ ap take-turn (ref-state-to-info-state s)
          [[failed
            (values (state-kick s) (cons ap out))] 
           [(? (curry equal? PASS))
            (values (state-rotate s) out)]
           [(legal-placement s s+ tiles-placed)
            (player-not-passed! all-passed)
            (cond
              [(active-sop-finished? s+ '[]) (ap-ends-game s+ #true out)]
              [else (define-values [handouts s++] (state-handouts s+ (length tiles-placed)))
                    (hand-tiles-now ap s++ handouts out ap-ends-game)])]
           [(legal-re-placement s s+ ap-s-tiles)
            (define-values [handouts s++] (state-handouts s+ (length ap-s-tiles)))
            (define-values [s+++ out++] (hand-tiles-now ap s++ handouts out ap-ends-game))
            (values (state-take-back s+++ ap-s-tiles) out++)]
           [_illegal-request_
            (report 'take-turn ap "illegal placement or re-placement request")
            (values (state-kick s) (cons ap out))]]))

#; {SoPlayer State [Listof Tile] [Listof SoPlayer] -> OneTurn}
(define (hand-tiles-now ap s+ handouts out ap-ends-game)
  (xsend+ ap new-tiles handouts
          [[failed
            (define s++ (state-kick (state-take-back s+ handouts)))
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
                  [A (first (state-players S))])
             (define-values (~? [S++ end out] [S++ out])
               (if dr (check-message W cep #px"dropped out" (run A S '[])) [run A S '[]]))
             (~? (check-true end W) (void))
             (if ok (check-true (state? S++) W) (check-false S++ W))
             (if ps (check-true (unbox passed) W) (check-false (unbox passed) W))
             (if dr (check-equal? out `[,A] (~a "A drop out" W)) (check-equal? out '[] (~a "no:" W))) 
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
;                                      ;;;                 
;                                        ;                 
;    ;;;   ;   ;  ;;;;  ;;;;;;  ;;;;     ;     ;;;    ;;;  
;   ;;  ;   ; ;       ; ;  ;  ; ;; ;;    ;    ;;  ;  ;   ; 
;   ;   ;;  ;;;       ; ;  ;  ; ;   ;    ;    ;   ;; ;     
;   ;;;;;;   ;     ;;;; ;  ;  ; ;   ;    ;    ;;;;;;  ;;;  
;   ;       ;;;   ;   ; ;  ;  ; ;   ;    ;    ;          ; 
;   ;       ; ;   ;   ; ;  ;  ; ;; ;;    ;    ;      ;   ; 
;    ;;;;  ;   ;   ;;;; ;  ;  ; ;;;;      ;;   ;;;;   ;;;  
;                               ;                          
;                               ;                          
;                               ;                          

#;
(module+ examples
  (provide
   #; {[Listof (cons SoPlayer Coordinate)]
       [Listof (cons SoPlayer Coordinate)] ->
       {List [Listof [Instanceof Player]] [Listof [Instanceof Player]]}}
   onT

   #; {State
       ->
       {List [Listof IPlayer] [Listof [List Color Coordinate Coordinate Coordinate]] Board Tile Undo}}
   ; state->pieces

   #; {Board [Listof Player] -> State}
   xstate
   
   ;; player lists 
   Euclid** Riemann** Riemann*2 1e-r** 1r-e** b1e-r** 11e-r**

   ;; bad players 
   Bsetup Btt Bwin Bwin2
   1setup 1tt 1win 1win2
   2setup 3tt

   ;; cheating players
   Z cheaters

   ;; winning players
   Radam Eadam Ecarl Rbob Ebob Rcarl Adam-R Adam-E Oli Eve Fae pass-ω pass-1)

  ;; ------------------------------------------------------------------------------------------------
  (define (onT w* outs)
    (list 
     (map (λ (A) (sop-player ('target-switch (car A) ))) w*)
     (map (compose sop-player car) outs)))

  ;; players 
  (define (mk-plr name target color home current post-gen-strat #:factory (f #false))
    (define ep ((or f mech:create-player) name post-gen-strat))
    (define ip ('create-player '_ target home color #:ep ep))
    (cons ip current))

  (define Z
    (let ([the-place (coordinate 9 9)])
      (mk-plr "undo" the-place "ABCDEF" the-place the-place dag-strategy
              #:factory (mech:retrieve-factory "undo" mech:factory-table-10))))
  
  (define cheaters
    (let ()
      ;; assume 11 x 12 baord-0
      (define lonames
        ;;  Name in Factory                  Target             Home              Current
        ;; ----------------------------------------------------------------------------------------
        `[["unreachable"     ,[coordinate 1 1] ,[coordinate 1 1] ,[coordinate 1 1]]
          ["undo"            ,[coordinate 1 1] ,[coordinate 3 1] ,[coordinate 1 1]]
          ["coordinate"      ,[coordinate 1 1] ,[coordinate 5 1] ,[coordinate 1 1]]
          ["row"             ,[coordinate 1 1] ,[coordinate 7 1] ,[coordinate 1 1]]
          ["column"          ,[coordinate 1 1] ,[coordinate 9 1] ,[coordinate 1 1]]
          ["move"            ,[coordinate 1 1] ,[coordinate 1 9] ,[coordinate 1 1]]])

      (for/list ([name+coordinates lonames] [c '[A B C D E F G]])
        (define name (first name+coordinates))
        [define fact (mech:retrieve-factory name mech:factory-table-10)]
        (match-define [list target home current] (rest name+coordinates))
        [mk-plr name target (~a c "FFFCC") home current dag-strategy #:factory fact])))

  #; {String String Sirng FactoryTable -> (values Bsetup Bwin Bwin2 Btt)}
  (define (mk-Bs setup tt win factory-table)
    (define Bsetup
      [mk-plr "ZENAs" (coordinate 5 5) "FFFFCC" (coordinate 5 1) (coordinate 1 3) 
              #:factory (mech:retrieve-factory setup factory-table)])

    (define Bwin
      [mk-plr "XIAw" (coordinate 5 3) "AAFFCC" (coordinate 3 3) (coordinate 3 3) 
              #:factory (mech:retrieve-factory win factory-table)])

    (define Bwin2
      (let ([fact (mech:retrieve-factory win factory-table)])
        (map (λ (x t h c) [mk-plr (~a "Win" x) t (~a x "FFFCC") h c #:factory fact])
             '("A" "B" "C")
             [list [coordinate 3 3] [coordinate 1 3] [coordinate 5 1]]
             [list [coordinate 1 1] [coordinate 3 5] [coordinate 3 1]]
             [list [coordinate 0 1] [coordinate 0 2] [coordinate 0 3]])))
  
    (define Btt
      [mk-plr "YALAtt" (coordinate 5 5) "FFFFCC" (coordinate 5 1) (coordinate 1 3) 
              #:factory (mech:retrieve-factory tt factory-table)])

    (values Bsetup Bwin Bwin2 Btt))

  (define-values (Bsetup Bwin Bwin2 Btt) (mk-Bs "setUp" "takeTurn" "win" mech:factory-table-7))
  (define-values (1setup 1win 1win2 1tt) (mk-Bs "setUp-1" "takeTurn-1" "win-1" mech:factory-table-8))
  (define-values (2setup 2win 2win2 3tt) (mk-Bs "setUp-2" "takeTurn-3" "win-2" mech:factory-table-8))

  (define Eve (mk-plr "eve" (coordinate 1 1) "orange" (coordinate 9 9) [coordinate 9 9]))
  (define Fae (mk-plr "fae" (coordinate 9 9) "blue"   (coordinate 1 1) [coordinate 1 1]))

  (define Oli (mk-plr "oli" (coordinate 3 1) "orange" (coordinate 3 1) [coordinate 3 2]))
  
  (define Eadam (mk-plr "eadam" (coordinate 3 3) "blue"  (coordinate 1 1) (coordinate 0 1)))
  (define Ebob  (mk-plr "ebob"  (coordinate 1 3) "red"   (coordinate 3 5) (coordinate 0 2)))
  (define Ecarl (mk-plr "ecarl" (coordinate 5 1) "green" (coordinate 5 3) (coordinate 0 3)))
  
  (define Radam (mk-plr "radam" (coordinate 3 3) "pink"   (coordinate 1 1) (coordinate 0 1)))
  (define Rbob  (mk-plr "rbob"  (coordinate 1 3) "purple" (coordinate 3 5) (coordinate 0 2)))
  (define Rcarl (mk-plr "rcarl" (coordinate 5 1) "yellow" (coordinate 5 3) (coordinate 0 3)))
  (define Rdale (mk-plr "rdale" (coordinate 5 1) "FFFFFC"  (coordinate 5 5) (coordinate 0 3)))
  (define Remil (mk-plr "remil" (coordinate 5 1) "black"  (coordinate 3 3) (coordinate 0 3)))

  (define Adam-R (mk-plr "Radam" (coordinate 5 1) "black"  (coordinate 1 1) (coordinate 3 3)))
  (define Adam-E (mk-plr "Edam"  (coordinate 5 3) "white"  (coordinate 3 3) (coordinate 3 3)))

  (define pass-ω
    (mk-plr "Rpass" (coordinate 3 5) "FFAABB" (coordinate 3 5) (coordinate 3 5) 
            #:factory (λ _ (new 'mech:pass-player% [my-name "Rpass"]))))
  (define pass-1
    (mk-plr "Epass" (coordinate 3 5) "BBFFAA" (coordinate 5 3) (coordinate 3 5) 
            #:factory (λ _ (new 'mech:pass-once-player% [my-name "Epass"]))))
  
  ;; player lists 
  (define Euclid**  (list Eadam Ebob Ecarl))
  (define Riemann** (list Radam Rbob Rcarl))
  (define Riemann*2 (list Remil Rdale Rcarl))
  (define 1e-r**    (list* Adam-E Adam-R (rest Riemann**)))
  (define b1e-r**   (list* Bwin Adam-R (rest Bwin2)))
  (define 11e-r**   (list* 1win Adam-R (rest 1win2)))
  (define 1r-e**    (list* Adam-E Adam-R (rest Euclid**)))
  
  ;; states 
  (define [xstate rb players (goals '())]
    ('cstate players #f #:goals goals)))

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

#;
(module+ test
  (define (cnr x) (sop-player (car x)))
  (define (cnr-name x) (send x name)))

; this is only needed for illustrating how Milestone #7 could be enforced and tested
#;
(module+ test
  (check-true ('distinct-homes [xstate 'padded-1 Riemann**]))
  (check-false (distinct-homes [xstate 'padded-1 (append Riemann** Euclid**)])))

#; 
(module+ test ;; very basic tests
  '------basic-----
  (define [olis p (q Oli)] (cons q (append p Euclid**)))
  (check-equal? (referee/state (xstate padded-oli [olis '[]])) [onT `(,Oli) '[]] "Oli plain")
  '-------
  (define allout-r `([] (,(sop-player (car Bsetup)))))
  (check-equal? (referee/state (xstate padded-oli [list Bsetup])) allout-r "--")

  (check-equal? (referee/state [xstate padded-2 Riemann**]) (onT `[,Rcarl] '[]) "Riemann 2")
  (check-equal? (referee/state [xstate padded-3 1e-r**]) (onT `[,Adam-E] '[]) "*padded 3w"))

#; 
(module+ test ;; ForStudents/ :: baddies tests
  (check-equal? (referee/state (xstate padded-oli [olis `[,Bsetup]]))
                `[[,(cnr Oli)] [,(cnr Bsetup)]])
  (check-equal? (referee/state (xstate padded-oli [olis `[,1setup]]))
                `[[,(cnr Oli)] [,(cnr 1setup)]])
  (check-equal? (referee/state (xstate padded-oli [olis `[,2setup]]))
                `[[,(cnr Oli)] [,(cnr 2setup)]] "for 8 students")
  (check-equal? (referee/state (xstate padded-oli [olis '[] Btt])) (onT `[,Ebob] `[, Btt]))
  (check-equal? (referee/state (xstate padded-oli [olis '[] 1tt])) (onT `[,Ebob] `[, 1tt]))
  (check-equal? (referee/state (xstate padded-oli [olis '[] 3tt])) (onT `[,Ecarl] `[,3tt])
                "for 8 Tests"))

#;
(module+ test
  ;; one player passes once, the other player always; both are at the same distance and should win
  (define p1-palways (list pass-ω pass-1))
  (define p1-state   (xstate padded-oli p1-palways))
  
  (for ([p (map cnr p1-palways)])
    (check-equal? (send p take-turn p1-state) PASS (~a (cnr-name p) " passes")))
  (check-equal? (score-game p1-state) `[,(map car p1-palways) []] "both pass, score!")
  (check-equal? (referee/state p1-state) `[,(map cnr p1-palways) []] "pass-pass")
  
  (define pass-r `[(,(cnr Adam-R)) []])
  (define p1-play (list Adam-R pass-ω))
  (define palways-state (xstate padded-oli p1-play))
  (check-equal? (referee/state palways-state) pass-r "one always passes; sits on home=goal=current"))

#;
(module+ test ;; Tests/ :: runs out of rounds but no tie yet
  ;; This test takes about an hour: 
  ;; 3069.521u 20.850s 52:13.97 98.5%	0+0k 0+0io 15pf+0w
  ;; after caching, all 23 tests passed in
  ;; 105.837u 1.837s 1:50.90 97.0%	0+0k 0+0io 12pf+0w
  'stress
  [set-per 5.0] ;; needed for the first few drracket interactuons 
  (define state-0 (cstate board-0 [tile-ew] [list Eve Fae] #false))
  (check-equal? (referee/state state-0) (reverse (onT '[] `[,Eve])) "Eve vs Fae")

  'stress-2
  (define state-0c (cstate board-0 [tile-ew] [list Eve Fae Z] #false))
  (check-equal? (referee/state state-0c) (reverse (onT `[,Z] `[,Eve])) "Eve vs Fae"))

#; 
(module+ test ;; Tests/ ;; plain tests
  (check-equal? (referee/state (xstate padded-oli (cons Oli Riemann**))) [onT `(,Oli) '[]] "")
  (check-equal? (referee/state [xstate padded-2 Euclid**]) (onT `[,Eadam] '[]) "*Euclid 2")
  (check-equal? (referee/state [xstate padded-1 1e-r**]) (onT `[,Adam-E] '()) "*21")
  (check-equal? (referee/state [xstate padded-3 1r-e**]) (onT `[,Adam-E] '[]) "*pad3"))

#; 
(module+ test ;; Tests/ :: baddies tests for 7
  (define (result Bwin Bwin2 (at-end '()))
    (let* ([result (onT (if Bwin `[,Bwin] '[]) (reverse (rest Bwin2)))]
           [result (append (first result) (second result))]
           [result (list '[] (append result at-end))])
      (referee-results->names result)))
  (check-equal? (referee-results->names (referee/state [xstate padded-1 b1e-r**]))
                (result Bwin Bwin2)
                "2 lose")
  (check-equal? (referee-results->names (referee/state [xstate padded-1 11e-r**]))
                (result 1win 1win2)
                "2 lose")

  ;; something goes wrong about their state or ordering if I don't check names 
  (check-equal? (referee-results->names (referee/state [xstate padded-1 (cons 3tt b1e-r**)]))
                (result #false Bwin2 (list (cnr Bwin)))
                "for 8 b")
  
  (check-equal? (referee/state (xstate padded-oli [list Bwin])) [reverse (onT `[,Bwin] '())])
  (check-equal? (referee/state (xstate padded-oli [list 1win])) [reverse (onT `[,1win] '())]))

#; 
(module+ test
  'cheaters 
  (for ([c cheaters])
    (define x (send (sop-player (car c)) name))
    (define s (cstate board-0 [tile-ew] [list Eve c] #false))
    (check-equal? (referee/state s) (reverse (onT `[,c] `[,Eve])) x))) 

#;
(module+ test
  'extra-goals 
  (define +goals (list (coordinate 3 3)))
  (define ++goals (list (coordinate 3 3) (coordinate 5 1)))
  (define +++goals (list (coordinate 3 3) (coordinate 5 1) (coordinate 1 5)))

  ;; the first two use the always-passing player, which is no good for external testing 
  (check-equal? (referee/state (xstate padded-oli (list pass-ω Adam-R) +goals)) pass-r "same+")
  (check-equal? (referee/state (xstate padded-oli (list pass-ω Adam-R) ++goals)) pass-r "same+")

  ;; there have a single winner 
  (check-equal? (referee/state [xstate padded-2 Euclid** ++goals]) (onT `[,Eadam] '[]) "E++")
  (check-equal? (referee/state [xstate padded-1 1e-r** +++goals]) (onT `[,Adam-E] '()) "21+")
  (check-equal? (referee/state [xstate padded-3 1r-e** +++goals]) (onT `[,Adam-E] '[]) "*pad3")

  ;; bad winners reman bad winners in the presence of multiple goals
  (check-equal? (referee-results->names (referee/state [xstate padded-1 (cons 3tt b1e-r**) +goals]))
                (result #false Bwin2 (list (cnr Bwin)))
                "for 8 b")
  (check-equal? (referee-results->names (referee/state [xstate padded-1 b1e-r** +goals]))
                (result Bwin Bwin2)
                "2 lose")
  (check-equal? (referee-results->names (referee/state [xstate padded-1 11e-r** +goals]))
                (result 1win 1win2)
                "2 lose"))

