#lang racket

;; data representation of tiles, shapes, and colors
;; ---------------------------------------------------------------------------------------------------

(provide
 #; {type Tile}
 tile?
 tiles?
 tile-shape
 tile-color
 tile/m

 (contract-out 
  [tile (-> shape? color? tile?)])

 #; {Tile Tile -> Boolean}
 tile<

 #; {[Listof Tile] -> Boolean}
 all-shapes?
 all-colors? 
 
 #; {Tile -> Image}
 render-tile

 #; Image
 blank

 #; {type Shape = (U 'star '8star 'square 'circle 'clover 'diamond)}
 #; {type Color = (U 'red 'green 'blue 'yellow 'orange 'purple)}
 
 #; {Any -> Boolean : ShapeSymbol}
 shape?

 #; {Any -> Boolean : ColorSymbol}
 color?

 #; Natural 
 ; COLORS#
 ; SHAPES#
 )

(provide ;; for documentation 
 #; {-> [Pair [Listof name-of-color:String]
              [Listof [Pair name-of-shape:String [Listof Image]]]]}
 ;; the very first string is "" to make the table square 
 render-all-shapes)

(provide ;; for homework & cheating players 
 ALL-SHAPES
 ALL-COLORS
 ALL-SHAPE-COLOR-COMBOS
 ALL-TILES
 ALL-TILES-PERM)

(module+ examples
  (provide
   +starter-tile

   duplicate-red-all-colors
   duplicate-star-all-shapes
   
   exactly-all-colors
   exactly-all-shapes

   starter-tile
   starter-tile*
   1starter-tile*
   2starter-tile*
   3starter-tile*
   qwirkle-tile*

   tiles0 tiles1 tiles2 tiles3 tiles4 tiles5 tiles6 tiles7 tiles8 tiles9 tiles10))

(module+ json
  (provide
   SHAPE
   COLOR

   jsexpr->tiles
   tiles->jsexpr 

   (contract-out 
    [tile->jsexpr (-> tile? j:jsexpr?)]
    [jsexpr->tile (-> j:jsexpr? (or/c tile? #false))])))

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

(require Qwirkle/Common/tile-colors)
(require Qwirkle/Common/tile-shapes)
(require Qwirkle/Lib/fixed-perm)
(require (prefix-in 2: 2htdp/image))

(module+ json
  (require (submod Qwirkle/Common/tile-colors json))
  (require (submod Qwirkle/Common/tile-shapes json))
  (require Qwirkle/Lib/json)
  (require Qwirkle/Lib/parse-json)
  (require (prefix-in j: json)))

(module+ test
  (require (submod ".."))
  (require (submod ".." examples))
  (require (submod ".." json))
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

(struct tile [shape color] #:prefab)

(define (tiles? x) (and (list? x) (andmap tile? x)))

(define-match-expander (tile/m s c) (λ (stx) #'(tile s c)))

(define ALL-SHAPE-COLOR-COMBOS
  (for*/list ([s ALL-SHAPES] [c ALL-COLORS])
    (tile (first s) c)))

(define ALL-TILES (apply append (make-list 30 ALL-SHAPE-COLOR-COMBOS)))

(define ALL-TILES-PERM (pick-fixed-permutation ALL-TILES))

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

(module+ examples
  (define starter-tile (tile 'star 'red))
  (define +starter-tile (tile 'circle 'red))

  (define 8-green (tile '8star 'green))
  (define sq-blue (tile 'square 'blue))
  (define ci-llow (tile 'circle 'yellow))
  (define cr-prpl (tile 'clover 'purple))
  (define di-llow (tile 'diamond 'yellow))
  (define ci-ange (tile 'circle 'orange))

  (define starter-tile*  [list 8-green sq-blue ci-llow cr-prpl di-llow ci-ange])
  (define 1starter-tile* [list sq-blue sq-blue ci-llow cr-prpl di-llow ci-ange])
  (define 2starter-tile* [list 8-green sq-blue cr-prpl cr-prpl di-llow ci-ange])
  (define 3starter-tile* [list 8-green sq-blue cr-prpl cr-prpl di-llow di-llow])

  (define qwirkle-tile* [build-list 6 (λ _ 8-green)])

  (define exactly-all-colors
    (take ALL-SHAPE-COLOR-COMBOS COLORS#))
  (define duplicate-red-all-colors (cons (first exactly-all-colors) exactly-all-colors))

  (define exactly-all-shapes
    (for/list ([i (in-range 0 (* SHAPES# SHAPES#) SHAPES#)])
      [list-ref ALL-SHAPE-COLOR-COMBOS i]))
  (define duplicate-star-all-shapes (cons (first exactly-all-shapes) exactly-all-shapes))
  
  (define tiles0 (list #s(tile square red) #s(tile square blue) #s(tile square purple)))
  (define tiles1 (list #s(tile circle blue)))
  (define tiles2 (list #s(tile clover green) #s(tile diamond green)))
  (define tiles3 (list #s(tile 8star green) #s(tile circle green)))
  (define tiles4 (list #s(tile square orange) #s(tile square red)))
  (define tiles5 (list #s(tile 8star yellow) #s(tile 8star orange)))
  (define tiles6 (list #s(tile star orange) #s(tile diamond orange)))
  (define tiles7 (list #s(tile diamond yellow) #s(tile circle yellow)))
  (define tiles8 (list #s(tile 8star red)))
  (define tiles9 (list #s(tile star red)   #s(tile star orange) #s(tile star blue)))
  (define tiles10 (list #s(tile square yellow) #s(tile square blue))))

;                                     
;                                     
;     ;       ;   ;;;                 
;     ;             ;                 
;   ;;;;;   ;;;     ;     ;;;        ;
;     ;       ;     ;    ;;  ;    ;;; 
;     ;       ;     ;    ;   ;; ;;    
;     ;       ;     ;    ;;;;;; ;;    
;     ;       ;     ;    ;        ;;; 
;     ;       ;     ;    ;           ;
;     ;;;   ;;;;;    ;;   ;;;;        
;                                     
;                                     
;                                     

;; 'star '8star 'square 'circle 'clover 'diamond
;; with preference given to colors in the following order
;; 'red 'green 'blue 'yellow 'orange 'purple

(define (tile< s t)
  (match-define [tile ss sc] s)
  (match-define [tile ts tc] t)
  (or (shape< ss ts)
      (and (shape= ss ts)
           (< (index-of ALL-COLORS sc) (index-of ALL-COLORS tc)))))

(module+ test
  (check-true (tile< (tile '8star 'red) (tile 'square 'red)))
  (check-true (tile< (tile '8star 'red) (tile '8star 'green)))
  (check-false (tile< (tile '8star 'red) (tile '8star 'red))))

;                                                                        
;                                      ;                                 
;          ;;;    ;;;                  ;                                 
;            ;      ;                  ;                                 
;   ;;;;     ;      ;            ;;;   ; ;;   ;;;;   ;;;;    ;;;    ;;;  
;       ;    ;      ;           ;   ;  ;;  ;      ;  ;; ;;  ;;  ;  ;   ; 
;       ;    ;      ;           ;      ;   ;      ;  ;   ;  ;   ;; ;     
;    ;;;;    ;      ;            ;;;   ;   ;   ;;;;  ;   ;  ;;;;;;  ;;;  
;   ;   ;    ;      ;               ;  ;   ;  ;   ;  ;   ;  ;          ; 
;   ;   ;    ;      ;           ;   ;  ;   ;  ;   ;  ;; ;;  ;      ;   ; 
;    ;;;;     ;;     ;;          ;;;   ;   ;   ;;;;  ;;;;    ;;;;   ;;;  
;                                                    ;                   
;                                                    ;                   
;                                                    ;                   

;; in support of Q BONUS

#; {[Listof Tile] -> Boolean}
(define (all-shapes? lot)
  (and (complete-set-of-shapes (map tile-shape lot)) (= (length lot) SHAPES#)))

(module+ test
  (define s-all (map (λ (s) {tile (first s) 'red}) ALL-SHAPES))
  (check-true  (all-shapes? s-all) "just all of them")
  (check-false (all-shapes? (rest s-all)) "missing one")
  (check-false (all-shapes? (cons (tile (caadr ALL-SHAPES) 'red) (rest s-all))) "with duplicate"))

#; {[Listof Tile] -> Boolean}
(define (all-colors? lot)
  (and (complete-set-of-colors (map tile-color lot)) (= (length lot) COLORS#)))

(module+ test
  (define c-all (map (λ (c) {tile 'square c}) ALL-COLORS))
  (check-true  (all-colors? c-all) "just all of them")
  (check-false (all-colors? (rest c-all)) "missing one")
  (check-false (all-colors? (cons (tile 'square (second ALL-COLORS)) (rest c-all))) "with duplicate"))

(module+ test 
  (check-false (all-colors? duplicate-red-all-colors) "duplicate red (star)")
  (check-false (all-shapes? duplicate-red-all-colors) "no it contains only stars")

  (check-false (all-shapes? duplicate-star-all-shapes) "duplicate (red) star")
  (check-false (all-colors? duplicate-star-all-shapes) "no it contains only red"))

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

(define (render-all-shapes [t values])
  (define all-shapes
    (for/list ([s ALL-SHAPES])
      (define sname  (first s))
      (define render (second s))
      (define tiles (map (compose frame render) ALL-COLORS))
      (cons (t (~a sname)) tiles)))
  (cons (map t (cons "" (map ~a ALL-COLORS))) all-shapes))

#; {Image [N] -> Image}
;; provide equal white space around all edges 
(define (frame i [white 4])
  (define w (2:image-width i))
  (define h (2:image-height i))
  (2:overlay i (2:rectangle (+ white w) (+ white h) 'solid 'white)))

;; ---------------------------------------------------------------------------------------------------
(define (render-tile 1tile)
  (match-define [tile s c] 1tile)
  (define t [(shape-of-color s) c])
  (2:overlay t (2:rectangle (+ (2:image-width t) 6) (+ (2:image-height t) 6) 'outline 'black)))

(define blank
  (let* ([s (render-tile (tile 'star 'green))]
         [w (2:image-width s)]
         [h (2:image-height s)]
         [s (2:rectangle w h 'solid 'white)])
    s))

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
  #; {type JTile  = Hasheq[('Shape, JShape), ('Color, JColor)]}
  #; {type JShape = (U "star" "8star" "square" "circle" "clover" "diamond")}
  #; {type JColor = (U "red" "green" "blue" "yellow" "orange" "purple")}

  (define SHAPE 'shape)
  (define COLOR 'color)

  #; {Tile -> JTile}
  (define (tile->jsexpr t)
    (match-define [tile shape color] t)
    (hasheq SHAPE (shape->jsexpr shape) COLOR (color->jsexpr color)))
  
  #; {JSexpr -> Option<Tile>}
  (def/jsexpr-> tile #:object {[SHAPE shape (? shape? s)] [COLOR color (? color? c)]} (tile s c))
  
  (define (tiles->jsexpr t*) (map tile->jsexpr t*))
  (def/jsexpr-> tiles #:array [(list (app jsexpr->tile (? tile? t)) ...) t]))

(module+ test
  (check-equal? (jsexpr->tile (tile->jsexpr +starter-tile)) +starter-tile))