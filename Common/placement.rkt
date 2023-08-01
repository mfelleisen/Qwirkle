#lang racket


(provide
 #; {type Placement = [Listof (placement Coordinate Tile)]}
 (struct-out placement))

#;
(provide ;; for integration testing 
 (contract-out 
  [placement (-> coordinate? tile? placement?)]))

(module+ examples
  (provide
   place-atop-starter
   lshaped-placement*
   +starter-plmt
   special-placements
   bad-spec-plmnt))

;; ---------------------------------------------------------------------------------------------------
(require Qwirkle/Common/coordinates)
(require Qwirkle/Common/tiles)

(module+ examples
  (require (submod Qwirkle/Common/coordinates examples))
  (require (submod Qwirkle/Common/tiles examples)))

;; ---------------------------------------------------------------------------------------------------
#; {type Placement* = [Listof Placement]}
;; placements in the order in which the tiles are put down 
#; {type Placement  = [placement Coordinate Tile]}
(struct placement [coordinate tile] #:prefab)

;; ---------------------------------------------------------------------------------------------------
(module+ examples
  (require (for-syntax syntax/parse))
  (require (for-syntax racket/syntax))
  
  (define-syntax (def/placements stx)
    (syntax-parse stx
      [(_ n)
       #:with name (format-id stx "plmt~a" (syntax-e #'n))
       #:with coord (format-id stx "coord~a" (syntax-e #'n))
       #:with tiles (format-id stx "tiles~a" (syntax-e #'n))
       #'(begin
           (provide name)
           (define name (map placement coord tiles)))]))

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

  (define bad-spec-plmnt (list (placement #s(coordinate -2 1) #s(tile square green)))))