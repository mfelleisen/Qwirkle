#lang racket

;; the player mechanism; several broken variants, provided via "factories" to bundle them into groups
;; ---------------------------------------------------------------------------------------------------

;; a player factory is a functiion from name and strategy to a player object
;; a factory table is a [list [list String Factoru] ...] for many similar players 

(define player-factory (-> string? any/c player/c))
(define factory-table  (listof (list/c string? player-factory)))

(provide
 #;{type Player}
 player?

 (contract-out
  
  [create-player
   ;; create a player object from a name and a strategy
   ;; it uses the player factory, if provided 
   (->* (string? (-> state? action?)) (#:bad player-factory) player/c)]
  
  [retrieve-factory
   ;; look up a player factor by name in the table 
   (-> string? factory-table player-factory)]

  [factory-base
   ;; plain old creator -- essentially create-player
   factory-table]

  [factory-table-pass
   ;; players that return PASS for take-turn (always or once): useful for terminating game 
   factory-table]

  [factory-table-7
   ;; methods that raise exceptions after a specified number of calls 
   factory-table]

  [factory-table-8
   ;; methods that return a type-correct, but logically incorrect result 
   factory-table]

  [factory-table-9
   ;; methods that go into infinite loops after a specified number of calls, but at most Count# 
   factory-table]
  [Count# natural?]

  [factory-all factory-table]))

(provide all-cheater-classes) ;; for homework 

(module+ json
  (provide
   ACHEAT ;; for homework 
   (contract-out
    [lax-names-okay? (parameter/c boolean?)]
    [player->jsexpr  (-> player/c jsexpr?)]
    [jsexpr->player  (->* (jsexpr?) (#:loops any/c #:cheating any/c) (or/c #false player/c))]
    [jsexpr->player* (->* (jsexpr?) (#:loops any/c #:cheating any/c) (or/c #false (listof player/c)))]
    [player*->jsexpr (-> (listof player/c) jsexpr?)])))

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
                                                                 
(require Qwirkle/Common/player-interface)
(require (except-in (submod Qwirkle/Common/game-state examples) Tests/ ForStudents/))
(require Qwirkle/Common/game-state)
(require Qwirkle/Common/coordinates)
(require Qwirkle/Common/tiles)
(require Qwirkle/Common/map)
(require Qwirkle/Common/state-of-player)
(require Qwirkle/Player/strategies)
(require (submod (lib "Qwirkle/scribblings/qwirkle.scrbl") spec))

(require (for-syntax syntax/parse))

(module+ json
  (require (submod ".."))
  (require Qwirkle/Lib/parse-json))

(module+ test
  (require (submod ".."))
  (require (except-in (submod Qwirkle/Common/game-state examples) Tests/ ForStudents/))
  (require (except-in (submod Qwirkle/Referee/ref-state examples) Tests/ ForStudents/))
  (require Qwirkle/Referee/ref-state)
  (require Qwirkle/Lib/check-message)
  (define cep current-error-port)
  (require racket/sandbox)
  (require rackunit))

;                                                                          
;                                                                          
;     ;                                                ;                   
;     ;                       ;                                            
;     ;                       ;                                            
;   ;;;;;     ;;;     ;;;   ;;;;;;   ;;;;    ;;;;    ;;;     ;;;;    ;;;;  
;     ;      ;   ;   ;   ;    ;     ;;  ;;   ;;  ;     ;    ;    ;  ;    ; 
;     ;          ;  ;         ;     ;    ;   ;         ;    ;;;;;;  ;      
;     ;      ;;;;;  ;         ;     ;    ;   ;         ;    ;        ;;;;  
;     ;     ;    ;  ;         ;     ;    ;   ;         ;    ;            ; 
;     ;     ;   ;;   ;   ;    ;     ;;  ;;   ;         ;    ;;   ;  ;    ; 
;     ;      ;;; ;    ;;;      ;;;   ;;;;    ;       ;;;;;   ;;;;;   ;;;;  
;                                                                          
;                                                                          
;                                                                          
;                                                                          

(module+ test
  (check-true (is-a? (create-player "hello" dag-strategy) player%))

  (define bsetup-factory (retrieve-factory "setup" factory-table-7))
  (define bsetup (create-player "hello" dag-strategy #:bad bsetup-factory))
  (check-true (is-a? bsetup player%))
  (check-equal? (object-name bsetup) 'object:setup%))

(define (create-player n s #:bad (factory (retrieve-factory "good" factory-base)))
  (factory n s))

(define (retrieve-factory name table+)
  (define maker (assoc name table+))
  (unless maker
    (error 'retrieve-factory "cannot retrieve ~a in ~v\n" name table+))
  (and maker (second maker)))

(define factory-base
  `[["good" ,(λ (n s) (new player% [my-name n] [strategy s]))]])

(module+ test (check-exn #px"cannot" (λ () (retrieve-factory "boo" factory-all))))

(define (player? x) (is-a? x player%))

;                                                                                          
;                                                                                          
;   ;                                                ;;;                                   
;   ;                                                  ;                                   
;   ;                                                  ;                                   
;   ; ;;;     ;;;    ;;;;    ;;;;           ; ;;;      ;      ;;;   ;    ;   ;;;;    ;;;;  
;   ;;  ;;   ;   ;  ;    ;  ;    ;          ;;  ;;     ;     ;   ;   ;  ;;  ;    ;   ;;  ; 
;   ;    ;       ;  ;       ;;;;;;          ;    ;     ;         ;   ;  ;   ;;;;;;   ;     
;   ;    ;   ;;;;;   ;;;;   ;               ;    ;     ;     ;;;;;   ;  ;   ;        ;     
;   ;    ;  ;    ;       ;  ;               ;    ;     ;    ;    ;    ; ;   ;        ;     
;   ;;  ;;  ;   ;;  ;    ;  ;;   ;          ;;  ;;     ;    ;   ;;    ;;    ;;   ;   ;     
;   ; ;;;    ;;; ;   ;;;;    ;;;;;          ; ;;;       ;;;  ;;; ;     ;     ;;;;;   ;     
;                                           ;                          ;                   
;                                           ;                         ;                    
;                                           ;                        ;;                    
;                                                                                          

(require (submod Qwirkle/Player/strategies json))

;; this player serves as the base (like a common abstract class) for implementing several variants 
(define player%
  (class object% 
    (init-field
     [my-name  "Adam"]
     [strategy dag-strategy])
    
    (field [my-tiles '()])
    
    (super-new)

    (define/public (description)
      `[,my-name ,(strategy->jsexpr strategy)])
    
    (define/public (name)
      my-name)

    (define/public (reset)
      (void))

    (define/public (setup state0 tiles0)
      (reset)
      (set! my-tiles tiles0))

    (define/public (take-turn s)
      (iterate-strategy strategy s))

    (define/public (new-tiles lot)
      (void))

    (define/public (win b)
      (eprintf "~a ~a\n" my-name (if b "won" "lost"))
      (void))))

(module+ test (check-true (is-a? (new player%) player%) "test coverage"))

;                                  
;                                  
;                                  
;                                  
;                                  
;   ; ;;;     ;;;    ;;;;    ;;;;  
;   ;;  ;;   ;   ;  ;    ;  ;    ; 
;   ;    ;       ;  ;       ;      
;   ;    ;   ;;;;;   ;;;;    ;;;;  
;   ;    ;  ;    ;       ;       ; 
;   ;;  ;;  ;   ;;  ;    ;  ;    ; 
;   ; ;;;    ;;; ;   ;;;;    ;;;;  
;   ;                              
;   ;                              
;   ;                              
;                                  

(define pass-player% ;; always return PASS 
  (class player%
    (super-new)
    (field [badfm '["pass-player"]])
    (define/override (take-turn s) PASS)))

(define factory-table-pass
  `[["pass-player" ,(λ (n s) (new pass-player% [my-name n] [strategy s]))]])

;                                                                          
;                                                                          
;   ;                    ;                                                 
;   ;                    ;                                                 
;   ;                    ;                                                 
;   ; ;;;     ;;;    ;;; ;          ;;;;;;    ;;;     ;;;    ;;;;    ;;;;  
;   ;;  ;;   ;   ;  ;;  ;;          ;  ;  ;  ;   ;   ;   ;   ;;  ;  ;;  ;; 
;   ;    ;       ;  ;    ;          ;  ;  ;      ;  ;        ;      ;    ; 
;   ;    ;   ;;;;;  ;    ;          ;  ;  ;  ;;;;;  ;        ;      ;    ; 
;   ;    ;  ;    ;  ;    ;          ;  ;  ; ;    ;  ;        ;      ;    ; 
;   ;;  ;;  ;   ;;  ;;  ;;          ;  ;  ; ;   ;;   ;   ;   ;      ;;  ;; 
;   ; ;;;    ;;; ;   ;;; ;          ;  ;  ;  ;;; ;    ;;;    ;       ;;;;  
;                                                                          
;                                                                          
;                                                                          
;                                                                          

;; allow the creation of player% subclasses that have one method go bad after "n" calls
;; call the `reset` method between test cases to get the state of count ("n") right 

(define-syntax (class/fail stx)
  (syntax-parse stx
    [(class/fail go-bad-after-this-many-times [(method-that-goes-bad args) body ...])
     #'(class* player% ()
         (init-field #; [String {Nat}]  badfm) ;; descriptor for use in integration tests
         (inherit-field strategy my-name my-tiles)
         (super-new)

         (define/override (description)
           (append (super description) badfm))

         ;; `reset` is called at the very beginning of the game and only once. 
         ;; If method-that-goes-bad is setup, then new count is 1 because `setup` is running.
         ;; Otherwise the new count is 0, because no other method has been called yet. 
         (field [count 0])
         (define/override (reset)
           (set! count (if (eq? 'method-that-goes-bad 'setup) 1 0)))
       
         (define/override (method-that-goes-bad . args)
           (set! count (+ count 1))
           (cond
             [(< count go-bad-after-this-many-times)
              (super method-that-goes-bad . args)]
             [(>= count go-bad-after-this-many-times)
              (begin ;; call super to get the effects (it either times out or goes bad)
                (super method-that-goes-bad . args)
                (let () body ...))])))]))

;                                                                  
;                                                                  
;                                              ;              ;    
;                                                             ;    
;                                                             ;    
;    ;;;;   ;;  ;;  ; ;;;                    ;;;    ; ;;;   ;;;;;  
;   ;    ;   ;  ;   ;;   ;                     ;    ;;   ;    ;    
;   ;;;;;;    ;;    ;    ;                     ;    ;    ;    ;    
;   ;         ;;    ;    ;                     ;    ;    ;    ;    
;   ;         ;;    ;    ;                     ;    ;    ;    ;    
;   ;;   ;   ;  ;   ;    ;    ;;               ;    ;    ;    ;    
;    ;;;;;  ;    ;  ;    ;    ;;             ;;;;;  ;    ;    ;    
;                             ;                                    
;                            ;;                                    
;                                                                  
;                                                                  

#; {[-> Any] {N} -> [Class <: Player%]}
;; mixins for creating classes that go bad on setup, take-turn, or win
(define (setup% th [failure-timing 1])     (class/fail failure-timing [(setup _) [th]]))
(define (take-turn% th [failure-timing 1]) (class/fail failure-timing [(take-turn _) [th]]))
(define (new-tiles% th [failure-timing 1]) (class/fail failure-timing [(new-tiles _) [th]]))
(define (win% th [failure-timing 1])       (class/fail failure-timing [(win _) [th]]))

#; {Class -> String}
;; retrieve the non-% part of a class's name, which is assumed to be alphabetic plus a hyphen 
(define (class-name c)
  (second (regexp-match #px"([a-z\\-A-Z]*)%" (~a (object-name c)))))

;; ---------------------------------------------------------------------------------------------------
;; a factory table for methods that raise exceptions 

(define [exn] (/ 1 0))

(define factory-table-7
  (for*/list ([f (list setup% take-turn% new-tiles% win%)])
    (define class% (f exn))
    (define name-% (class-name f))
    (list (format "~a" name-%) (λ (n s) (new class% [badfm `(,name-%)] [my-name n] [strategy s])))))

;; ---------------------------------------------------------------------------------------------------
;; a factory tble for methods tht go into infinite loops when called the n-th time 

(define [loop] [loop])
(define Count# 7)

(define factory-table-9
  (for*/list ([f (list setup% take-turn% new-tiles% win%)] [k (in-range 1 (+ Count# 1) 1)])
    (define class% (f loop k))
    (define name-% (class-name f))
    (list (format "~a-~a" name-% k)
          (λ (n s) (new class% [badfm `(,name-% ,k)] [my-name n] [strategy s])))))

;                                                                  
;                                                                  
;           ;                                                      
;           ;                         ;                            
;           ;                         ;                            
;     ;;;   ; ;;;    ;;;;     ;;;   ;;;;;;   ;;;;    ;;;;    ;;;;  
;    ;   ;  ;;   ;  ;    ;   ;   ;    ;     ;    ;   ;;  ;  ;    ; 
;   ;       ;    ;  ;;;;;;       ;    ;     ;;;;;;   ;      ;      
;   ;       ;    ;  ;        ;;;;;    ;     ;        ;       ;;;;  
;   ;       ;    ;  ;       ;    ;    ;     ;        ;           ; 
;    ;   ;  ;    ;  ;;   ;  ;   ;;    ;     ;;   ;   ;      ;    ; 
;     ;;;   ;    ;   ;;;;;   ;;; ;     ;;;   ;;;;;   ;       ;;;;  
;                                                                  
;                                                                  
;                                                                  
;                                                                  

;; tiles that don't fit 

(define non-adjacent-coordinate%
  ;; [players#+1,0] cannot be adjacent to an existing tile: at most # of players have placed tiles 
  (class/fail 1 ((take-turn args)
                 (define players# (length (state-players (first args))))
                 (list (placement (coordinate (+ players# 1) 0) #s(tile circle orange))))))

;; ---------------------------------------------------------------------------------------------------
(define tile-not-owned%
  ;; [players#+1,0] cannot be adjacent to an existing tile: at most # of players have placed tiles 
  (class/fail 1 ((take-turn args)
                 (define the-state (first args))
                 (define my-tiles  (sop-tiles (first (state-players the-state))))
                 (define bad-tile  (find-non-existent-tile my-tiles))
                 (cond
                   ;; this can happen when players get handed as many tiles as there are combos:
                   [(boolean? bad-tile) (super take-turn . args)]
                   [else 
                    (define candidate (set-first (find-candidates (state-map the-state) bad-tile)))
                    (list (placement (candidate-place candidate) bad-tile))]))))

(define (find-non-existent-tile tiles)
  (for/first ([t ALL-SHAPE-COLOR-COMBOS] #:unless (member t tiles)) t))

;; ---------------------------------------------------------------------------------------------------
(define not-a-line%
  ;; returns placements that are not in a line, if possible 
  (class/fail
   1
   ([take-turn args]
    (define the-state (first args))
    (define my-tiles (sop-tiles (first (state-players the-state))))
    (cond
      [(< (length my-tiles) 2) (super take-turn . args)]
      [else
       (let/ec escape
         (iterate-strategy (get-field strategy this) the-state (not-a-line-placements escape)))]))))

(define ((not-a-line-placements  escape) legal? placements-so-far+)
  (eprintf "placements ~a\n" placements-so-far+)

  (if (and (false? legal?) (non-line placements-so-far+))
      (escape placements-so-far+)
      #false))

(define (non-line placements-so-far+)
  (and (>= (length placements-so-far+) 2)
       (not (or (same-row placements-so-far+) (same-column placements-so-far+)))))

(define (two-non-aligned-coordinates candidat1 candidat2)
  (define one (set-map candidat1 candidate-place))
  (define two (set-map candidat2 candidate-place))
  (for/first ([pair (for*/list ([o one] [t two]) (list o t))]
              #:unless (or (same-row pair) (same-column pair)))
    pair))

;; ---------------------------------------------------------------------------------------------------
(define bad-ask-for-tiles%
  (class/fail 1 ([take-turn args]
                 (define the-state (first args))
                 (define my-tiles  (sop-tiles (first (state-players the-state))))
                 (define tile#     (state-tiles the-state))
                 (if (< tile# (length my-tiles)) REPLACEMENT (super take-turn . args)))))

;; ---------------------------------------------------------------------------------------------------
(define no-fit%
  ;; [players#+1,0] cannot be adjacent to an existing tile: at most # of players have placed tiles 
  (class/fail 1 ((take-turn args)
                 (define the-state (first args))
                 (define my-tiles  (sop-tiles (first (state-players the-state))))
                 (define bad-place (find-bad-place (state-map the-state) my-tiles))
                 (if (boolean? bad-place) (super take-turn . args) (list bad-place)))))

#; {Map [Listof Tile] -> Option<Placement>}
(define (find-bad-place gmap tile*)
  (define places (all-free-neighbors gmap))
  (let/ec return 
    (for* ([ti tile*] [co places])
      (unless (fits gmap (placement co ti))
        (return (placement co ti))))
    #false))

;; ---------------------------------------------------------------------------------------------------
(define all-cheater-classes
  `[[,non-adjacent-coordinate% "the placement of a tile that is not adjacent to a placed tile."]
    [,tile-not-owned%          "the placement of a tile that it does not own."]
    [,not-a-line%              "placements that are not in one line (row, column)."]
    [,bad-ask-for-tiles%       "a tile replacement but it owns more tiles than the referee has left."]
    [,no-fit%                  "the placement of a tile that does not match its adjacent tiles."]])

(define ACHEAT "a cheat")

(define factory-table-8
  (for*/list ([c% (map first all-cheater-classes)])
    (define name (class-name c%))
    (list (format "~a" name) (λ (n s) (new c% [badfm `(,ACHEAT ,name)] [my-name n] [strategy s])))))

;                       
;                       
;          ;;;    ;;;   
;            ;      ;   
;   ;;;;     ;      ;   
;       ;    ;      ;   
;       ;    ;      ;   
;    ;;;;    ;      ;   
;   ;   ;    ;      ;   
;   ;   ;    ;      ;   
;    ;;;;     ;;     ;; 
;                       
;                       
;                       

(define factory-all (append (append factory-table-8 factory-table-9 factory-table-7)))

;                                          
;                                          
;                                          
;     ;                       ;            
;     ;                       ;            
;   ;;;;;;   ;;;;    ;;;;   ;;;;;;   ;;;;  
;     ;     ;    ;  ;    ;    ;     ;    ; 
;     ;     ;;;;;;  ;         ;     ;      
;     ;     ;        ;;;;     ;      ;;;;  
;     ;     ;            ;    ;          ; 
;     ;     ;;   ;  ;    ;    ;     ;    ; 
;      ;;;   ;;;;;   ;;;;      ;;;   ;;;;  
;                                          
;                                          
;                                          
;                                          


(module+ test ;; a normal player 
  (define starter-tiles (list +starter-tile))

  (define player-normal (create-player "BenL" dag-strategy))
  
  (check-equal? (send player-normal name) "BenL")
  (check-equal? (send player-normal setup info-starter-state starter-tiles) (void))
  (check-equal? (send player-normal take-turn info-starter-state) REPLACEMENT)
  (check-equal? (send player-normal new-tiles starter-tiles) [void])
  (check-equal? (check-message "normal" cep #px"won" (send player-normal win #true)) [void])

  (define player-ldasg  (create-player "BobF" ldasg-strategy))
  (define state0- (ref-state-to-info-state state0))
  (check-true (map? (legal state0- (send player-ldasg take-turn state0-))))

  (define new-pass (retrieve-factory "pass-player" factory-table-pass))
  (define player-pass (create-player "P" dag-strategy #:bad new-pass))
  (check-equal? (send player-pass take-turn info-starter-state) PASS))

;; ---------------------------------------------------------------------------------------------------
(module+ test ;; a player that raises an exn for setup new-tiles win 
  (define new-exn-setup    (retrieve-factory "setup" factory-table-7))
  (define exn-setup-player (create-player "baddy" dag-strategy #:bad new-exn-setup))
  
  (check-equal? (check-message "div" cep #px"baddy won" (send exn-setup-player win #t)) (void))
  (check-exn #px"division"
             (λ ()
               (send exn-setup-player setup info-starter-state starter-tiles)))

  (define new-exn-nt    (retrieve-factory "new-tiles" factory-table-7))
  (define exn-nt-player (create-player "bad" dag-strategy #:bad new-exn-nt))
  (check-exn #px"division" (λ () (send exn-nt-player new-tiles starter-tiles)))

  (define new-exn-win    (retrieve-factory "win" factory-table-7))
  (define exn-win-player (create-player "bad" dag-strategy #:bad new-exn-win))
  (check-exn #px"division"
             (λ ()
               (check-message "div2" cep #px"bad won"  (send exn-win-player win #false)))))

;; ---------------------------------------------------------------------------------------------------
(module+ test ;; a player that goes into an infinite loop on setup (1st call) 
  (define new-setup-1    (retrieve-factory "setup-1" factory-table-9))
  (define setup-1-player (create-player "bad" dag-strategy #:bad new-setup-1))
  
  (check-equal? (get-field badfm setup-1-player) '["setup" 1])
  (check-equal? (send setup-1-player name) "bad")
  (check-exn #px"out of time"
             (λ ()
               (with-deep-time-limit 2
                 (send setup-1-player setup info-starter-state starter-tiles)))))

(module+ test ;; a player that goes into an infinite loop on third call to new-tiles 
  (define new-nt-3    (retrieve-factory "new-tiles-3" factory-table-9))
  (define nt-3-player (create-player "bad-3" dag-strategy #:bad new-nt-3))

  (check-equal? (send nt-3-player setup info-starter-state starter-tiles) [void])
  (check-exn #px"out of time"
             (λ ()
               (with-deep-time-limit 2
                 (send nt-3-player setup info-starter-state starter-tiles)
                 (send nt-3-player new-tiles starter-tiles)
                 (send nt-3-player new-tiles starter-tiles)
                 (send nt-3-player new-tiles starter-tiles)))))

(module+ test ;; a player that goes into an infinite loop on third call to take-turn 
  (define new-tt-3    (retrieve-factory "take-turn-3" factory-table-9))
  (define tt-3-player (create-player "tt-3" dag-strategy #:bad new-tt-3))

  (check-equal? 
   (begin (send tt-3-player setup info-starter-state starter-tiles)
          (send tt-3-player take-turn info-starter-state)
          (void))
   
   (void))
  
  (check-equal?
   (begin (send tt-3-player setup info-starter-state starter-tiles)
          (send tt-3-player take-turn info-starter-state)
          (send tt-3-player take-turn info-starter-state)
          (void))
   (void))
  
  (check-exn #px"out of time"
             (λ ()
               (with-deep-time-limit 2
                 (send tt-3-player setup info-starter-state starter-tiles)
                 (send tt-3-player take-turn info-starter-state)
                 (send tt-3-player take-turn info-starter-state)
                 (send tt-3-player new-tiles starter-tiles)
                 (send tt-3-player take-turn info-starter-state)))))

;; ---------------------------------------------------------------------------------------------------
(module+ test ;; checker

  #; {PubKnowledge -> x:Any -> Boolean : (x is illegal (series of) placement(s))}
  (define ((illegal-placement state) placements)
    (eprintf "~a\n" placements)
    (and (andmap placement? placements) (not (legal state placements))))

  (define illegal-in-info-starter-state? (illegal-placement info-starter-state)))

(module+ test ;; the requested placement is not adjacted to an existig tiles 
  (define new-nat    (retrieve-factory "non-adjacent-coordinate" factory-table-8))
  (define nat-player (create-player "bad" dag-strategy #:bad new-nat))
  (check-pred illegal-in-info-starter-state? (send nat-player take-turn info-starter-state)))

(module+ test ;; player places a tile it doesn't own 
  (define new-tno    (retrieve-factory "tile-not-owned" factory-table-8))
  (define tno-player (create-player "bad" dag-strategy #:bad new-tno))
  (check-true (placement? (first (send tno-player take-turn info-all-tiles))) "coverage")
  (check-pred illegal-in-info-starter-state? (send tno-player take-turn info-starter-state)))

(module+ test ;; the requested placements are not on a line
  (define new-nal    (retrieve-factory "not-a-line" factory-table-8))
  (define nal-player (create-player "bad" dag-strategy #:bad new-nal))

  (check-equal? (send nal-player take-turn info-starter-state) REPLACEMENT "coverage")
  'b
  (render-info-state info-special-state)
  (check-pred (illegal-placement info-special-state) (send nal-player take-turn info-special-state)))

(module+ test ;; player requests tiles when there aren't enough 
  (define new-br    (retrieve-factory "bad-ask-for-tiles" factory-table-8))
  (define br-player (create-player "bad" dag-strategy #:bad new-br))
  (check-equal? (send br-player take-turn info-starter-state) REPLACEMENT "coverage")
  (check-equal? (send br-player take-turn info-bad-state) REPLACEMENT))

(module+ test ;; the requested placement does not match its neighnor(s)
  (define new-nf    (retrieve-factory "no-fit" factory-table-8))
  (define nf-player (create-player "bad" dag-strategy #:bad new-nf))
  (check-true (placement? (first (send nf-player take-turn info-+starter-state))) "coverage")
  (check-pred illegal-in-info-starter-state? (send nf-player take-turn info-starter-state)))

;                              
;      ;                       
;                              
;                              
;    ;;;    ;;;    ;;;   ; ;;  
;      ;   ;   ;  ;; ;;  ;;  ; 
;      ;   ;      ;   ;  ;   ; 
;      ;    ;;;   ;   ;  ;   ; 
;      ;       ;  ;   ;  ;   ; 
;      ;   ;   ;  ;; ;;  ;   ; 
;      ;    ;;;    ;;;   ;   ; 
;      ;                       
;      ;                       
;    ;;                        

(module+ json
  (define lax-names-okay? (make-parameter #false))

  (define (player*->jsexpr p*) (map player->jsexpr p*))

  (define (player->jsexpr p) (send p description))

  (define (jsexpr->player* j #:loops [loops #false] #:cheating [cheaters #false])
    (match j
      [(list (app (λ (j) (jsexpr->player j #:loops loops #:cheating cheaters)) (? player? p)) ...) p]
      [_ (eprintf "value does not match JActors schema:\n~a\n" (jsexpr->string j))
         #false]))

  (define (jsexpr->player j #:loops [loops #false] #:cheating [cheaters #false])
    (match j
      [(list* (? jname? name) (app jsexpr->strategy (? procedure? s)) remainder)
       (define check-then-create [check-then-create/curried j name s])
       (match remainder
         ['()
          (check-then-create #false "good" 'x factory-base)]
         [(list (? string? bad-method))
          (check-then-create #false bad-method "exception" factory-table-7)]
         [(list (== ACHEAT) (? string? bad-method))
          (check-then-create (false? cheaters) bad-method "cheating" factory-table-8)]
         [(list (? string? bad-method) (? natural? n))
          (check-then-create (false? loops) (~a bad-method "-" n) "looping" factory-table-9)]
         [_ (err "options beyond 'plain' don't match" j)])]
      [_ (err "not an array" j)]))

  #; {JSexpr -> [Option String]}
  (define (jname? j)
    (cond
      [(not (string? j)) (err "name not a string" j)]
      [(lax-names-okay?) j]
      [(not (regexp-match (pregexp PLAYER-NAME) j)) (err "name not alphanumeric or too short" j)]
      [(not (<= (string-length j) MAX-PLAYER-NAME)) (err "name too long" j)]
      [else j]))

  #; {JSexpr String Strategey -> Boolean String Natural FactoryTable -> [Option Player]}
  (define ([check-then-create/curried j name s] b? bad-method n factory-table)
    (cond
      [(or b? (not-in bad-method factory-table)) (err n j)]
      [else (create-player name s #:bad (retrieve-factory bad-method factory-table))]))
  
  #; {String FactoryTable -> Boolean}
  (define (not-in bad-method factory-table)
    (define r (assoc bad-method factory-table))
    (not r))

  #; {JSexpr N -> False}
  (define (err n j)
    (define s (jsexpr->string/ j))
    (eprintf "~a does not match JActorSpec schema [~a] \n ~a\n" 'jsexpr->player n s)
    #false))
            
(module+ test
  (require (submod ".." json))
  
  (check-false (check-message "a" cep #px"schema" (jsexpr->player 1)) "bad JSexpr 1")
  (check-false (check-message "b" cep #px"schema" (jsexpr->player '["a" "dag" 1])) "bad JSexpr 2")
  (check-false (check-message "c" cep #px"not match" (jsexpr->player `["a" "dag" "setup" 1])) "BAD")

  (check-equal? (let ()
                  (define j (player->jsexpr player-normal))
                  (send (jsexpr->player j) setup info-+starter-state '[]))
                (void)
                "normal player")
  
  (check-exn #px"division"
             (λ ()
               (define j (player->jsexpr exn-setup-player))
               (send (jsexpr->player j) setup info-+starter-state '[]))
             "exception raising player")


  (check-exn #px"out of time"
             (λ ()
               (with-deep-time-limit 1
                 (define j (player->jsexpr setup-1-player))
                 (define p (jsexpr->player j #:loops 'yes))
                 (send p setup info-+starter-state '[])))
             "infinite loop player")
  
  (check-true (let* ([factory (second (first factory-table-8))]
                     [j (player*->jsexpr (list (factory "A" dag-strategy)))]
                     [p (jsexpr->player* j #:cheating 'yes!) ])
                (andmap player? p))
              "cheating player"))
