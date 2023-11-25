#lang racket

;; a data representation of the state of a Q player
;; ---------------------------------------------------------------------------------------------------

(provide 
 #; {type [SoPlayer Y] = [sop Natural [Listof Tile] Y]}
 ;; where Y is typically an external player or just a symbolic name
 sop?
 sop-score
 sop-tiles
 sop-player
 sop-set-player
 
 (contract-out
  [sop (-> natural? (listof tile?) any/c sop?)])

 #; {[Y] [SoPlayer Y] Natural -> [SoPlayer Y]}
 sop-score++

 #; {[Y] [SoPlayer Y] [Listof Tile] -> [SoPlayer Y]}
 sop-tiles--

 #; {[Y] [SoPlayer Y] [Listof Tile] -> [SoPlayer Y]}
 sop-tiles++

 #; {[SoPlayer Y] -> [SoPlayer (U Symbol String)]}
 sop-special

 #; {Player [Listof Tile] -> Boolean}
 player-owns-tiles

 #; {[Listof SoPlayer] -> [Listof String]}
 extract-names 
 
 #; {[X Y] [SoPlayer X] Y [Y ->Image] {{ [Y] [SoPlayer Y] -> Image }} -> Image}
 render-sop*

 #; {[Y] [SoPlayer Y] -> Image}
 render-sop)

(module+ json
  (provide
   #; {type JPlayer = Natural || { NAME : String, SCORE : Natural, TILES : [Listof JTile]}}
   TILES SCORE NAME
   
   players->jsexpr
   ;; inverse depends on kind of state 

   1player->jsexpr
   jsexpr->1player))

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

(require (submod (lib "Qwirkle/scribblings/qwirkle.scrbl") spec))
(require Qwirkle/Common/tiles)
(require (prefix-in 2: 2htdp/image))
(require (for-syntax syntax/parse))

(module+ json
  (require (submod Qwirkle/Common/tiles json))
  (require Qwirkle/Lib/parse-json))

(module+ test
  (require (submod ".."))
  (require (submod Qwirkle/Common/tiles examples))
  (require rackunit))

;                                                                 
;       ;                                  ;            ;;        
;       ;           ;                      ;           ;          
;       ;           ;                      ;           ;          
;    ;;;;  ;;;;   ;;;;;  ;;;;           ;;;;   ;;;   ;;;;;        
;   ;; ;;      ;    ;        ;         ;; ;;  ;;  ;    ;          
;   ;   ;      ;    ;        ;         ;   ;  ;   ;;   ;          
;   ;   ;   ;;;;    ;     ;;;;         ;   ;  ;;;;;;   ;          
;   ;   ;  ;   ;    ;    ;   ;         ;   ;  ;        ;          
;   ;; ;;  ;   ;    ;    ;   ;         ;; ;;  ;        ;     ;;   
;    ;;;;   ;;;;    ;;;   ;;;;          ;;;;   ;;;;    ;     ;;   
;                                                                 
;                                                                 
;                                                                 

(struct sop [score tiles player] #:prefab)

(define-match-expander sop/m
  (λ (stx)
    (syntax-parse stx
      [(sop/m score tiles payload) #'(sop score tiles payload)])))

#; {[SoPlayer Y] Z -> {SoPlayer Z}}
(define [sop-set-player p xp]
  (struct-copy sop p [player xp]))

;; ---------------------------------------------------------------------------------------------------
#; {[SoPlayer Y] -> {SoPlayer Y}}
(define (sop-score++ p n)
  (struct-copy sop p [score (+ (sop-score p) n)]))

;; ---------------------------------------------------------------------------------------------------
#; {[SoPlayer Y] -> {SoPlayer Y}}
(define (sop-tiles-- p old-tile*)
  (define tiles-owned (sop-tiles p))
  (for ([placed old-tile*])
    (set! tiles-owned (remove placed tiles-owned)))
  (struct-copy sop p [tiles tiles-owned]))

(module+ test
  (define green-square (tile 'square 'green))
  (define sop-with-2-identical-tiles (sop 0 (list green-square green-square) 'sop1))
  (define sop-with-1-such-tile       (sop 0 (list green-square) 'sop1))
  (check-equal? (sop-tiles-- sop-with-2-identical-tiles (list green-square)) sop-with-1-such-tile))

;; ---------------------------------------------------------------------------------------------------
#; {[SoPlayer Y] -> {SoPlayer Y}}
(define (sop-tiles++ p new-tile*)
  (struct-copy sop p [tiles (append new-tile* (sop-tiles p))]))

;; ---------------------------------------------------------------------------------------------------
#; {[SoPlayer Y] -> [SoPlayer (U Symbol String)]}
(define (sop-special p)
  (define payload (sop-player p))
  (define name
    (cond
      [(or (symbol? payload) (string? payload)) payload]
      [(object? payload) (send payload name)]
      [else (error 'sop-special "proper payload expected, given" payload)]))
  (struct-copy sop p [player name]))

;; ---------------------------------------------------------------------------------------------------
#; {Player [Listof Tile] -> Boolean}
(define (player-owns-tiles player placed-tiles)
  (define tiles-owned (sop-tiles player))
  (for/and ([placed placed-tiles])
    (begin0
      (cons? (member placed tiles-owned))
      (set! tiles-owned (remove placed tiles-owned)))))

(module+ test
  (define the-starter-player (apply sop 0 [list starter-tile* "player1"]))
  (check-true (player-owns-tiles the-starter-player starter-tile*) "it owns lshaped")
  (check-true (player-owns-tiles (sop 0 (cons +starter-tile tiles1) 'p) tiles1) "owns, 1"))

;; ---------------------------------------------------------------------------------------------------
(define (extract-names lo-sop)
  (map (λ (p) (send (sop-player p) name)) lo-sop))

;                                            
;                            ;               
;                            ;               
;                            ;               
;    ;;;;   ;;;   ; ;;    ;;;;   ;;;    ;;;; 
;    ;;  ; ;;  ;  ;;  ;  ;; ;;  ;;  ;   ;;  ;
;    ;     ;   ;; ;   ;  ;   ;  ;   ;;  ;    
;    ;     ;;;;;; ;   ;  ;   ;  ;;;;;;  ;    
;    ;     ;      ;   ;  ;   ;  ;       ;    
;    ;     ;      ;   ;  ;; ;;  ;       ;    
;    ;      ;;;;  ;   ;   ;;;;   ;;;;   ;    
;                                            
;                                            
;                                            

#; {[X Y] [SoPlayer X] Y [Y ->Image] -> Image}
(define (render-sop* one l-sop [render-one render-sop])
  (define sop-images (cons (render-sop one) (map render-one l-sop)))
  (for/fold ([r (first sop-images)]) ([s (rest sop-images)])
    (2:above/align 'left r vblank s)))

#; {[Y] {SoPlayer Y} -> Image}
(define (render-sop 1sop)
  (match-define [sop/m score tiles player] (sop-special 1sop))
  (define tiles-image (map render-tile tiles))
  (define score-image (2:text (number->string score) 20 'black))
  (define player-image (2:text (~a player) 20 'black))
  (apply 2:beside/align 'top player-image hblank score-image hblank tiles-image))

(define hblank (2:rectangle 10 1 'solid 'white))
(define vblank (2:rectangle 1 10 'solid 'white))

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

  (define SCORE 'score)
  (define TILES 'tile*)
  (define NAME 'name)

  (define (players->jsexpr p*) (map 1player->jsexpr p*))

  #; {(U SoPlayer Natural) -> JPlayer}
  (define (1player->jsexpr 1player)
    (match 1player [(sop/m score tiles x)
                    (define name (if (or (symbol? x) (string? x)) (~a x) (send x name)))
                    (hasheq SCORE score TILES (map tile->jsexpr tiles) NAME name)]))

  (define (jsexpr->1player j #:name (name "a name"))
    (def/jsexpr-> 1player
      #:object {[NAME (? string? n)] [SCORE (app jsexpr->tiles# (? tile#? s))] [TILES tiles (list t ...)]}
      (sop s t n))
    (jsexpr->1player j)))
