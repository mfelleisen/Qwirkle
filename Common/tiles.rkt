#lang racket

;; data representation of tiles, shapes, and colors
;; ---------------------------------------------------------------------------------------------------

(provide
 #; {type Tile}
 tile?
 tile-shape
 tile-color
 tile/m

 (contract-out 
  [tile (-> (or/c 'star '8star 'square 'circle 'clover 'diamond)
            (or/c 'red 'green 'blue 'yellow 'orange 'purple)
            tile?)])

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
 color?)

(provide ;; for documentation 
 #; {-> [Pair [Listof name-of-color:String]
              [Listof [Pair name-of-shape:String [Listof Image]]]]}
 ;; the very first string is "" to make the table square 
 render-all-shapes)

(provide ;; for homework & cheating players 
 ALL-SHAPES
 ALL-COLORS
 ALL-TILE-COLOR-COMBOS)

(module+ examples
  (provide
   +starter-tile

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

(require 2htdp/image)

(module+ json
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

(define-match-expander (tile/m s c) (λ (stx) #'(tile s c)))

;; the shapes and colors

(define (4point-star c) (radial-star 4 15 42 'solid c))
(define (8point-star c) (radial-star 8 30 15 'solid c))
(define (square-ish c)  (square 58 'solid c))
(define (circle-ish c)  (circle 29 'solid c))
(define (diamondish c)  (rhombus 41 90 'solid c))
(define (clover-ish c)
  (define one (rectangle 2 2 'solid c))
  (let* ([s (circle 14 'solid c)]
         [t (above s one s)]
         [s (beside s one s)]
         [s (overlay s t)])
    s))

(define ALL-SHAPES
  (list
   (list 'star    4point-star)
   (list '8star   8point-star)
   (list 'square  square-ish)
   (list 'circle  circle-ish)
   (list 'clover  clover-ish)
   (list 'diamond diamondish)))

(define (shape? x) (cons? (assq x ALL-SHAPES)))

(unless (and (apply = (map image-width (map (lambda (x) (x 'green)) (map second ALL-SHAPES))))
             (apply = (map image-height (map (lambda (x) (x 'green)) (map second ALL-SHAPES)))))
  (error "images must be same size"))

(define ALL-COLORS '(red green blue yellow orange purple))

(define (color? x) (cons? (member x ALL-COLORS)))

(define ALL-TILE-COLOR-COMBOS
  (for*/list ([s ALL-SHAPES] [c ALL-COLORS])
    (tile (first s) c)))

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

(define SHAPE-NAMES (map first ALL-SHAPES))
(define (tile< s t)
  (match-define [tile ss sc] s)
  (match-define [tile ts tc] t)
  (or (< (index-of SHAPE-NAMES ss) (index-of SHAPE-NAMES ts))
      (and (= (index-of SHAPE-NAMES ss) (index-of SHAPE-NAMES ts))
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
(define SHAPES# (length ALL-SHAPES))
(define (all-shapes? lot)
  (define shapes (map tile-shape lot))
  (= (set-count (apply set shapes)) SHAPES#))

(module+ test
  (define s-all (map (λ (s) {tile (first s) 'red}) ALL-SHAPES))
  (check-true  (all-shapes? s-all) "just all of them")
  (check-false (all-shapes? (rest s-all)) "missing one")
  (check-false (all-shapes? (cons (tile (caadr ALL-SHAPES) 'red) (rest s-all))) "with duplicate"))

#; {[Listof Tile] -> Boolean}
(define COLORS# (length ALL-COLORS))
(define (all-colors? lot)
  (define colors (map tile-color lot))
  (= (set-count (apply set colors)) COLORS#))

(module+ test
  (define c-all (map (λ (c) {tile 'square c}) ALL-COLORS))
  (check-true  (all-colors? c-all) "just all of them")
  (check-false (all-colors? (rest c-all)) "missing one")
  (check-false (all-colors? (cons (tile 'square (second ALL-COLORS)) (rest c-all))) "with duplicate"))

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
  (define w (image-width i))
  (define h (image-height i))
  (overlay i (rectangle (+ white w) (+ white h) 'solid 'white)))

;; ---------------------------------------------------------------------------------------------------
(define (render-tile 1tile)
  (match-define [tile s c] 1tile)
  (define t [(second [assq s ALL-SHAPES]) (first (member c ALL-COLORS))])
  (overlay t (rectangle (+ (image-width t) 6) (+ (image-height t) 6) 'outline 'black)))

(define blank
  (let* ([s (render-tile (tile 'star 'green))]
         [w (image-width s)]
         [h (image-height s)]
         [s (rectangle w h 'solid 'white)])
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
    (hasheq SHAPE (symbol->jsexpr shape) COLOR (symbol->jsexpr color)))

  #; {JSexpr -> Option<Tile>}
  (def/jsexpr-> tile #:object {[SHAPE symbol (? shape? s)] [COLOR symbol (? color? c)]} (tile s c))
  
  (define (tiles->jsexpr t*) (map tile->jsexpr t*))
  (def/jsexpr-> tiles #:array [(list (app jsexpr->tile (? tile? t)) ...) t]))

(module+ test
  (check-equal? (jsexpr->tile (tile->jsexpr +starter-tile)) +starter-tile))