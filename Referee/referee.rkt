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
  (define-syntax (ref-test-case stx)
    (syntax-parse stx
      [(test-case msg:string name:expr factory:expr state:expr [winners out]
                  (~optional (~seq #:players B:expr ...) #:defaults ([(B 1) null]))
                  (~optional (~seq #:quiet Q:expr) #:defaults ([Q #'#false]))
                  (~optional (~seq #:with-obs O:expr)))
       #'(let ([run (λ (x) (~? (referee/state x) (referee/state x #:with-obs O)))])
           (let* ((P (create-player "A" dag-strategy #:bad (retrieve-factory name factory)))
                  [S (set-ref-state-players state [cons P (list B ...)])]
                  [cep current-error-port])
             (check-equal?
              (if Q
                  (check-message cep #px"dropped" (run S))
                  (parameterize ([cep (open-output-string)])
                    (run S)))
              (list winners out)
              msg))
           ;; --- to cover the default #:with-obs clause 
           (let* ((P (create-player "A" dag-strategy #:bad (retrieve-factory name factory)))
                  [C (~?
                      (create-config state #:observe O #:per-turn 1.1)
                      (create-config state))]
                  [C (dict-set C QUIET Q)]
                  [cep current-error-port])
             (check-equal?
              (if Q
                  (referee/config C (cons P (list B ...)))
                  (check-message cep #px"dropped" (referee/config C (cons P (list B ...)))))
              (list winners out)
              msg)))]))

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
                 #:players (create-player "B" dag-strategy)))

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
;; -- test referee/config
;;    -- quiet!
;;    -- one player fails at setup

;; DEFAULT RESULT 
;; -- state-kick and state-take-back seem to have overlapping functionality : CHECK
;; -- no state-players


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

  (define-syntax (setup-test-case stx)
    (syntax-parse stx
      [(test-case title:string name:expr factory:expr state:expr
                  [(~optional (~seq #:dropped  e:string)) new-state dropped-out]
                  (~optional (~seq #:players B Bplayer:expr)))
       #:with S (datum->syntax #'test-case 'S #'test-case #'test-case)
       #:with A (datum->syntax #'test-case 'A #'test-case #'test-case)
       #'(let* ((A (create-player (~a "A-" title) dag-strategy #:bad (retrieve-factory name factory)))
                [B Bplayer]
                [S (set-ref-state-players state [list A B])])
           (~?
            (define-values [S++ out] (check-message current-error-port (pregexp e) (setup S)))
            (define-values [S++ out] [setup S]))
           (check-values (values S++ (map sop-player out)) new-state dropped-out title))]))

  (setup-test-case "setup works fine" "good" factory-base ref-starter-state [S '[]]
                   #:players B (create-player "B" dag-strategy))

  (setup-test-case "players drops out during setup" "setup" factory-table-7 ref-starter-state
                   [#:dropped "dropped" (state-kick S #:from-active #t) `(,A)]
                   #:players C (create-player "C" dag-strategy)))

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
  (define-syntax (*-rounds-test-case stx)
    (syntax-parse stx
      [(test-case title:string name:expr factory:expr state:expr
                  [((~literal define-values) [x:id ...] e:string) checks ...]
                  (~optional (~seq #:players B:expr ...) #:defaults ([(B 1) null])))
       #:with W (datum->syntax #'test-case 'label #'test-case #'test-case)
       #:with S (datum->syntax #'test-case 'S   #'test-case #'test-case)
       #'(let ([F (retrieve-factory name factory)]
               [T state]
               [C (list B ...)])
           (let* ([W (~a "test for " title)]
                  (P (create-player (~a "A-" title) dag-strategy #:bad F))
                  [_ (send P setup (ref-state-to-info-state T) '())]
                  [S (set-ref-state-players T [cons P C])])
             (define-values [x ...] (check-message current-error-port (pregexp e) (rounds S '[])))
             checks ...))]))

  (*-rounds-test-case "receiving new tiles fails; game goes on with one more player"
                      "new-tiles" factory-table-7
                      (active-sop-hand state1-with (list #s(tile clover red)))
                      [(define-values [winners out] "dropped out")
                       (define players (state-players S))
                       (define dropped (first players))
                       (define 1winner (second players))
                       ;; - - - 
                       (check-equal? winners `[[,1winner] []] label)
                       (check-equal? out `[,dropped] label)]
                      #:players (create-player "B" dag-strategy))

  (*-rounds-test-case "receiving new tiles fails"
                      "new-tiles" factory-table-7 (active-sop-hand state1 (list #s(tile clover red)))
                      [(define-values [winners out] "dropped out")
                       ;; - - - 
                       (check-equal? winners '[[] []] label)
                       (check-equal? out (list (first (state-players S))) label)]))

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
  (define-syntax (1-round-test-case stx)
    
    (syntax-parse stx
      [(test-case title:string name:expr factory:expr state:expr
                  [(~optional ((~literal define-values) [x:id ...] e:string)) checks ...]
                  (~optional (~seq #:players B:expr ...) #:defaults ([(B 1) null])))
       #:with S   (datum->syntax #'test-case 'S   #'test-case #'test-case)
       #:with S++ (datum->syntax #'test-case 'S++ #'test-case #'test-case)
       #:with out (datum->syntax #'test-case 'out #'test-case #'test-case)
       #:with end (datum->syntax #'test-case 'end #'test-case #'test-case)
       #:with W   (datum->syntax #'test-case 'label #'test-case #'test-case)
       
       #'(let ([F (retrieve-factory name factory)]
               [T state]
               [C (list B ...)])
           (let* ([W (~a "test for " title)]
                  (P (create-player (~a "A-" title) dag-strategy #:bad F))
                  [_ (send P setup (ref-state-to-info-state T) '())]
                  [S (set-ref-state-players T [cons P C])])
             (~?
              (define-values [x ...] (check-message current-error-port (pregexp e) (one-round S '[])))
              (define-values [S++ end out] [one-round S '[]]))
             checks ...))]))

  (1-round-test-case "receiving new tiles fails"
                     "new-tiles" factory-table-7 (active-sop-hand state1 (list #s(tile clover red)))
                     [(define-values [S++ end out] "dropped out")
                      ;; - - - 
                      (check-false S++ label)
                      (check-true end label)
                      (check-equal? out (list (first (state-players S))) label)])

  (1-round-test-case "receiving new tiles fails; game goes on with one more player"
                     "new-tiles" factory-table-7
                     (active-sop-hand state1-with (list #s(tile clover red)))
                     [(define-values [S++ end out] "dropped out")
                      ;; - - - 
                      (check-true (state? S++) label)
                      (check-false end label)
                      (check-equal? out (list (first (state-players S))) label)]
                     #:players (create-player "B" dag-strategy))

  (1-round-test-case "player wins on take turn"
                     "new-tiles" factory-table-7
                     state1
                     ((check-true (state? S++) label)
                      (check-true end label)
                      (check-equal? out '[] label))))

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
           [_illegal-request_ (values (state-kick s) (cons ap out))]]))

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
  #; {TC is pne of:
         (test-case title:String
                    className:StringExpr
                    factory:Expr
                    state:Expr
                    [maybe-define check:expr ...])
         || (test-case title:String
                       className:StringExpr
                       factory:Expr
                       state:Expr
                       [check:expr ...]
                       #:players p:Expr ...)}
  #; {MaybeDefine = ϵ || (define-values x y)}
  
  (define-syntax (1-turn-test-case stx)
    
    (syntax-parse stx
      [(test-case title:string name:expr factory:expr state:expr
                  [(~optional ((~literal define-values) [x:id ...] e:string)) checks ...]
                  (~optional (~seq #:players B:expr ...) #:defaults ([(B 1) null])))
       #:with S   (datum->syntax #'test-case 'S   #'test-case #'test-case)
       #:with S++ (datum->syntax #'test-case 'S++ #'test-case #'test-case)
       #:with out (datum->syntax #'test-case 'out #'test-case #'test-case)
       #:with W   (datum->syntax #'test-case 'label #'test-case #'test-case)
       #:with A   (datum->syntax #'test-case 'A   #'test-case #'test-case)
       
       #'(let* ([F (retrieve-factory name factory)]
                [T state]
                [C (list B ...)]
                [fake-k (λ (s end? out) (values s end? out))]
                [run (one-turn #;"fake continuation:" fake-k passed)])
           (let* ([W title]
                  (P (create-player (~a "A-> " W) dag-strategy #:bad F))
                  [_ (send P setup (ref-state-to-info-state T) '())]
                  [S (set-ref-state-players T [cons P C])]
                  [A (first (state-players S))])
             (~?
              (define-values [x ...] (check-message current-error-port (pregexp e) (run A S '[])))
              (define-values [S++ out] [run A S '[]]))
             checks ...))]))

  (define passed [box #true])
  
  (1-turn-test-case "player passes due to lack of placable tiles: ref-starter-state"
                    "good" factory-base ref-starter-state
                    [(check-true (state? S++) label)
                     (check-true (empty? out) label)
                     (check-true (unbox passed) label)]
                    #:players (create-player "B" dag-strategy))
  
  (1-turn-test-case "player requests legal placement of tiles: state0; no more passing"
                    "good" factory-base state0 
                    [(check-true (state? S++) label)
                     (check-true (empty? out) label)
                     (check-false (unbox passed) label)
                     (set-box! passed #true #;"restore old value")])

  (1-turn-test-case "player passes"
                    "good" factory-base bad-state
                    [(check-true (state? S++) label)
                     (check-true (empty? out) label)
                     (check-true (unbox passed) label)])

  (1-turn-test-case "correctly shaped, but illegal placement request; state is #false in this case"
                    "tile-not-owned" factory-table-10 bad-state
                    [(check-false S++ label)
                     (check-true (unbox passed) label)
                     (check-equal? out (list (first (state-players S))) label)])

  (1-turn-test-case "cover illegal request; state is #false in this case because there's 1 player"
                    "take-turn" factory-table-7 bad-state
                    [(define-values [S++ out] "dropped out")
                     ;; - - - 
                     (check-false S++ label)
                     (check-true (unbox passed) label)
                     (check-equal? out (list (first (state-players S))) label)])

  (1-turn-test-case "receiving new tiles fails; it's the last player so game's up"
                    "new-tiles" factory-table-7 (active-sop-hand state1 (list #s(tile clover red)))
                    [(define-values [S++ end out] "dropped out")
                     ;; - - - 
                     (check-false S++ label)
                     (check-true end label)
                     (check-false (unbox passed) label)
                     (check-equal? out (list (first (state-players S))) label)])

  (1-turn-test-case "receiving new tiles fails; game goes on with one more player"
                    "new-tiles" factory-table-7
                    (active-sop-hand state1-with (list #s(tile clover red)))
                    [(define-values [S++ out] "dropped out")
                     ;; - - - 
                     (check-true (state? S++) label)
                     (check-false (unbox passed) label)
                     (check-equal? out (list (first (state-players S))) label)]
                    #:players (create-player "B" dag-strategy))
  
  (1-turn-test-case "player wins on take turn"
                    "new-tiles" factory-table-7
                    state1
                    ((define-values [S++ end out] "")
                     ;; - - - 
                     (check-true (state? S++) label)
                     (check-true end label)
                     (check-false (unbox passed) label)
                     (check-equal? out '[] label))))

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
