#lang racket

;; a data representation for player actions, esp. placements
;; ---------------------------------------------------------------------------------------------------

(provide
 #; {type Action = REPLACEMENT || PASS || Placement}
 action?
 #; {type LAction = REPLACEMENT || PASS || [Listof Placement]}
 action*? 
 PASS
 REPLACEMENT


 #; {type Placement = [Listof (placement Coordinate Tile)]}
 #; (struct-out placement)
 placement?
 (contract-out 
  [placement (-> coordinate? tile? placement?)])
 placement-coordinate
 placement-tile)

(module+ examples
  (provide
   plmt*
   place-atop-starter
   lshaped-placement*
   +starter-plmt
   special-placements
   bad-spec-plmnt
   place-orange-circle-at--2-2))

(module+ json
  (provide
   #; {type JAction*    = "pass" || "replace" || JPlacements}
   #; {type JPlacements = [Listof 1Placement]}
   #; {type J1Placement = { TILE : JTile, COORDINATE : JCoordinate }}
   
   COORDINATE ATILE JPASS JREPLACEMENT

   (contract-out
    [action->jsexpr     (-> action? jsexpr?)]
    [jsexpr->action     (-> jsexpr? (or/c action? #false))]
    [action*->jsexpr    (-> action*? jsexpr?)]
    [jsexpr->action*    (-> jsexpr? (or/c action*? #false))]
    [placements->jsexpr (-> (listof placement?) (listof jsexpr?))]
    [jsexpr->placements (-> (listof jsexpr?) (or/c (listof placement?) #false))])))

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

(require Qwirkle/Common/coordinates)
(require Qwirkle/Common/tiles)

(module+ examples
  (require (submod Qwirkle/Common/coordinates examples))
  (require (submod Qwirkle/Common/tiles examples))
  (require (for-syntax syntax/parse))
  (require (for-syntax racket/syntax)))

(module+ json
  (require (submod Qwirkle/Common/coordinates json))
  (require (submod Qwirkle/Common/tiles json))
  (require Qwirkle/Lib/parse-json)
  (require json))

(module+ test
  (require (submod ".."))
  (require (submod ".." examples))
  (require (submod ".." json))
  (require Qwirkle/Lib/check-message)
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

#; {type Placement* = [Listof Placement]}
;; placements in the order in which the tiles are put down 
#; {type Placement  = [placement Coordinate Tile]}
(struct placement [coordinate tile] #:prefab)

(define REPLACEMENT (gensym 'replacement))
(define PASS        (gensym 'pass))

#; {Any -> Boolean : Action}
(define (action? x)
  (or (placement? x) (equal? x REPLACEMENT) (equal? x PASS)))

(define (action*? x)
  (or (and (list? x) (andmap placement? x)) (equal? x REPLACEMENT) (equal? x PASS)))

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
  (define-syntax (def/placements stx)
    (syntax-parse stx
      [(_ n)
       #:with name (format-id stx "plmt~a" (syntax-e #'n))
       #:with coord (format-id stx "coord~a" (syntax-e #'n))
       #:with tiles (format-id stx "tiles~a" (syntax-e #'n))
       #'(begin
           (provide name)
           (define oops (map placement coord tiles))
           (set! plmt* (append plmt* (list oops)))
           (define name oops))]))

  (define plmt* '[])
  
  (def/placements 0)
  (def/placements 1)
  (def/placements 2)
  (def/placements 3)
  (def/placements 4)
  (def/placements 5)
  (def/placements 6)
  (def/placements 7)
  (def/placements 8)
  (def/placements 9)
  (def/placements 10)

  (define place-atop-starter (list (placement origin #s(tile circle red))))
  (define +starter-plmt (list (placement +starter-coor +starter-tile)))
  (define lshaped-placement* (map placement lshaped-coordinates starter-tile*))
  (define special-placements
    (list 
     (placement #s(coordinate -1 1) #s(tile diamond green))
     (placement #s(coordinate -3 1) #s(tile star green))))
  (define place-orange-circle-at--2-2 [list (placement #s(coordinate -2 2) #s(tile circle orange))])

  (define bad-spec-plmnt (list (placement #s(coordinate -2 1) #s(tile square green)))))

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
  (define ATILE '1tile)
  (define COORDINATE 'coordinate)
  (define JPASS "pass")
  (define JREPLACEMENT "replace")

  (define (action*->jsexpr a)
    (cond
      [(equal? a PASS) JPASS]
      [(equal? a REPLACEMENT) JREPLACEMENT]
      [else (placements->jsexpr a)]))

  (define (action->jsexpr a)
    (cond
      [(equal? a PASS) JPASS]
      [(equal? a REPLACEMENT) JREPLACEMENT]
      [else (1placement->jsexpr a)]))

  (define (jsexpr->action* j)
    (match j
      [(regexp JPASS) PASS]
      [(regexp JREPLACEMENT) REPLACEMENT]
      [(list i ...) (jsexpr->placements j)]
      [_ (eprintf "Action* value does not match schema ~a\n" (jsexpr->string j #:indent 4))
         #false]))

  (define (jsexpr->action j)
    (match j
      [(regexp JPASS) PASS]
      [(regexp JREPLACEMENT) REPLACEMENT]
      [(app jsexpr->placements (? placement p)) p]
      [_ (eprintf "Action* value does not match schema ~a\n" (jsexpr->string j #:indent 4))
         #false]))

  #; {Placements -> [Listof JPlacement]}
  (define (placements->jsexpr p*)
    (map 1placement->jsexpr p*))
  
  #; {1Placement -> 1Placement}
  (define (1placement->jsexpr p)
    (define co (placement-coordinate p))
    (define ti (placement-tile p))
    (hasheq ATILE (tile->jsexpr ti) COORDINATE (coordinate->jsexpr co)))

  (def/jsexpr-> placements #:array [(list (app jsexpr->1placement (? placement? p)) ...) p])

  (def/jsexpr-> 1placement
    #:object {[COORDINATE coordinate (? coordinate? co)] [ATILE tile (? tile? ti)]}
    (placement co ti)))

(module+ test
  (check-equal? (jsexpr->action* (action*->jsexpr PASS)) PASS)
  (check-equal? (jsexpr->action* (action*->jsexpr REPLACEMENT)) REPLACEMENT)
  (check-equal? (jsexpr->action* (action*->jsexpr plmt0)) plmt0)
  (check-false (check-message "1" current-error-port #px"schema" (jsexpr->action* 1)))
  (check-equal? (jsexpr->placements (placements->jsexpr plmt0)) plmt0))
