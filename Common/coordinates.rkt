#lang racket

;; data representation of map coordinates
;; -----------------------------------------------------------------------------

(provide
 #; {type Coordinate}
 coordinate?
 coordinate 
 coordinate-row
 coordinate-column

 #; {Coordinate -> Coordinate}
 add1-row
 sub1-row
 add1-column
 sub1-column

 #; Coordinate 
 origin

 #; {[Listof Coordinate] -> Option<Integer>}
 same-row
 same-column

 #; {Coordinate Coordinate -> Boolean}
 top-down-left-to-right-order<
 right-to-left-bottom-up-order<

 #; {Coordinate -> Coordinate}
 left-of
 right-of
 top-of
 below-of)

(module+ examples
  (provide
   coord0 coord1 coord2 coord3 coord4 coord5 coord6 coord7 coord8 coord9 coord10
   
   +starter-coor lshaped-coordinates))

(module+ json
  (provide
   ROW
   COLUMN 
   (contract-out
    [coordinate->jsexpr (-> coordinate? jsexpr?)]
    [jsexpr->coordinate (-> jsexpr? (or/c coordinate? #false))])))

;; ---------------------------------------------------------------------------------------------------
(module+ json
  (require (except-in Qwirkle/Lib/parse-json string->jsexpr jsexpr->string))
  (require json))

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

(struct coordinate [row column] #:prefab)

#; {type Coordinate = [coordinate Integer Integer]}
;; the first tile denotes the origin of the map
;; -- fix an up/down and left/right orientation of the map
;; -- a negative/positive row coordinate meands up/down, respectively
;; -- a negative/positive column coordinate means left/right, respectively 
;; Thst is, coordinates are "computer graphics" coordinates.

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

(define origin [coordinate 0 0])

(module+ examples
  
  (define coord0 (list (coordinate +3 0) (coordinate +3 +1) (coordinate +3 +2)))
  (define can001 ;; candidates for map0, (first tiles0)
    (list
       (coordinate -1 0)
      (coordinate 0 -1)
      (coordinate 0 1)
      (coordinate 1 -1)
      (coordinate 1 1)
      (coordinate 2 -1)
      (coordinate 2 1)
      (coordinate 3 0)))

  (define coord1 (list (coordinate +2 +1)))
  (define coord2 (list (coordinate 0 -1)   (coordinate +1 -1)))
  (define can221 ;; candidates for map2, (first tiles2)
    (list
      (coordinate -1 0)
      (coordinate 0 -1)
      (coordinate 0 1)))
  (define can222 (list (coordinate 1 -1)))

  (provide can001 can221 can222 can992 can993)


  (define coord3 (list (coordinate -1 -1) (coordinate +2 -1)))
  (define coord4 (list (coordinate +3 +3) (coordinate +4 +3)))
  (define coord5 (list (coordinate -1 -2)  (coordinate -1 -3)))
  (define coord6 (list (coordinate 0 -3)  (coordinate +1 -3)))
  (define coord7 (list (coordinate +1 -2)    (coordinate +2 -2)))
  (define coord8 (list (coordinate -1 0)))
  (define coord9 (list (coordinate +4 0) (coordinate +4 -1) (coordinate +4 +1)))

  (define can992
    (list
      (coordinate -2 -3)
      (coordinate -1 -4)
      (coordinate 0 -4)
      (coordinate 1 -4)
      (coordinate 2 3)
      (coordinate 3 4)))

  (define can993 (list (coordinate 0 -4) (coordinate 4 1)))

  (define coord10 (list (coordinate +3 +4) (coordinate +4 +4)))

  (define +starter-coor (coordinate 0 -1))

  (define lshaped-coordinates
    [list (coordinate -1  0)
          (coordinate +1  0)
          (coordinate +1 +1)
          (coordinate +1 +2)
          (coordinate +1 +3)
          (coordinate  0 +3)]))

;                              
;                              
;                              
;                              
;    ;;;   ;;;;    ;;;         
;   ;; ;;  ;; ;;  ;   ;        
;   ;   ;  ;   ;  ;            
;   ;   ;  ;   ;   ;;;         
;   ;   ;  ;   ;      ;        
;   ;; ;;  ;; ;;  ;   ;   ;;   
;    ;;;   ;;;;    ;;;    ;;   
;          ;                   
;          ;                   
;          ;                   

#; {Coordinate -> Coordinate}
(define (left-of co)
  (struct-copy coordinate co [column (- (coordinate-column co) 1)]))

#; {Coordinate -> Coordinate}
(define (right-of co)
  (struct-copy coordinate co [column (+ (coordinate-column co) 1)]))

#; {Coordinate -> Coordinate}
(define (top-of co)
  (struct-copy coordinate co [row (- (coordinate-row co) 1)]))

#; {Coordinate -> Coordinate}
(define (below-of co)
  (struct-copy coordinate co [row (+ (coordinate-row co) 1)]))

;; ---------------------------------------------------------------------------------------------------

#; {Coordinate -> Coordinate -> Coordinate}
(define (coordinate+-1 delta-coordinate)
  (match-define [coordinate δ-row δ-column] delta-coordinate)
  (λ (co)
    (match-define [coordinate row column] co)
    (coordinate (+ row δ-row) (+ column δ-column))))

(define add1-row    (coordinate+-1 (coordinate +1 0)))
(define sub1-row    (coordinate+-1 (coordinate -1 0)))
(define add1-column (coordinate+-1 (coordinate 0 +1)))
(define sub1-column (coordinate+-1 (coordinate 0 -1)))


;                                                                 
;                     ;                                           
;                     ;                   ;                       
;                     ;                                           
;    ;;;    ;;;;   ;;;;   ;;;    ;;;;   ;;;   ; ;;    ;;;;   ;;;  
;   ;; ;;   ;;  ; ;; ;;  ;;  ;   ;;  ;    ;   ;;  ;  ;;  ;  ;   ; 
;   ;   ;   ;     ;   ;  ;   ;;  ;        ;   ;   ;  ;   ;  ;     
;   ;   ;   ;     ;   ;  ;;;;;;  ;        ;   ;   ;  ;   ;   ;;;  
;   ;   ;   ;     ;   ;  ;       ;        ;   ;   ;  ;   ;      ; 
;   ;; ;;   ;     ;; ;;  ;       ;        ;   ;   ;  ;; ;;  ;   ; 
;    ;;;    ;      ;;;;   ;;;;   ;      ;;;;; ;   ;   ;;;;   ;;;  
;                                                        ;        
;                                                     ;  ;        
;                                                      ;;         

#; {Coordinate Coordinate -> Boolean}
(define (top-down-left-to-right-order< c d)
  (match-define [coordinate rc cc] c)
  (match-define [coordinate rd cd] d)
  (cond
    [(< rc rd) #true]
    [(> rc rd) #false]
    [(= rc rd) (< cc cd)]))

(define (right-to-left-bottom-up-order< c d)
  (match-define [coordinate rc cc] c)
  (match-define [coordinate rd cd] d)
  (cond
    [(> cc cd) #true]
    [(< cc cd) #false]
    [(= cc cd) (> rc rd)]))

;                              
;                              
;                              
;                              
;    ;;;   ;;;;  ;;;;;;   ;;;  
;   ;   ;      ; ;  ;  ; ;;  ; 
;   ;          ; ;  ;  ; ;   ;;
;    ;;;    ;;;; ;  ;  ; ;;;;;;
;       ;  ;   ; ;  ;  ; ;     
;   ;   ;  ;   ; ;  ;  ; ;     
;    ;;;    ;;;; ;  ;  ;  ;;;; 
;                              
;                              
;                              

#; {[Placement -> M] -> Placement* -> Option<Integer>}
(define [(same selector) placements]
  (define all-selected (map selector placements))
  (if (apply = all-selected) (first all-selected) #false))

(module+ test
  (check-true (integer? (same-row coord1)))
  (check-false (same-column (list (coordinate 1 1) (coordinate 3 2)))))

(define same-row    (same coordinate-row))
(define same-column (same coordinate-column))

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
  (define ROW 'row)
  (define COLUMN 'column)

  #; {type JCoordinate = [Hash ROW Integer COLUMN Integer]}
  
  #; {JSexpr -> (U False Coordinate)}
  (def/jsexpr-> coordinate #:object {[ROW (? integer? r)] [COLUMN (? integer? c)]} (coordinate r c))
  
  #; {Coordinate -> JCoordinate}
  (define (coordinate->jsexpr rb)
    (match-define [coordinate r c] rb)
    (hasheq ROW r COLUMN c)))

(module+ test
  (check-equal? (jsexpr->coordinate (coordinate->jsexpr (first coord0))) (first coord0)))
