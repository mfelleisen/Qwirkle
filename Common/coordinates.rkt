#lang racket

;; data representation of map coordinates

;; -----------------------------------------------------------------------------
(provide
 #; {type Coordinate}
 coordinate?
 coordinate 
 coordinate-row
 coordinate-column

 #; Coordinate 
 origin

 #; {Coordinate Coordinate -> Boolean}
 top-down-left-to-right-order<

 #; {Coordinate -> Coordinate}
 left-of
 right-of
 top-of
 below-of)

(module+ examples
  (provide
   coord0 coord1 coord2 coord3 coord4 coord5 coord6 coord7 coord8 coord9 coord10
   
   +starter-coor
   lshaped-coordinates))

(module+ json
  (provide
   ROW
   COLUMN 
   (contract-out
    [coordinate->jsexpr (-> coordinate? jsexpr?)]
    [jsexpr->coordinate (-> jsexpr? coordinate?)])))

;; ---------------------------------------------------------------------------------------------------
(module+ json
  (require json))

(module+ test
  (require (submod ".."))
  (require (submod ".." examples))
  (require (submod ".." json))
  (require rackunit))

;; ---------------------------------------------------------------------------------------------------
(struct coordinate [row column] #:prefab)
(define origin [coordinate 0 0])

#; {type Coordinate = [coordinate Integer Integer]}
;; the first tile denotes the origin of the map
;; -- fix an up/down and left/right orientation of the map
;; -- a negative/positive row coordinate meands up/down, respectively
;; -- a negative/positive column coordinate means left/right, respectively 
;; Thst is, coordinates are "computer graphics" coordinates.

#; {Coordinate -> Coordinate}
(define (left-of co)
  (match-define [coordinate r c] co)
  (coordinate r (- c 1)))

#; {Coordinate -> Coordinate}
(define (right-of co)
  (match-define [coordinate r c] co)
  (coordinate r (+ c 1)))

#; {Coordinate -> Coordinate}
(define (top-of co)
  (match-define [coordinate r c] co)
  (coordinate (- r 1) c))

#; {Coordinate -> Coordinate}
(define (below-of co)
  (match-define [coordinate r c] co)
  (coordinate (+ r 1) c))

#; {Coordinate Coordinate -> Boolean}
(define (top-down-left-to-right-order< c d)
  (match-define [coordinate rc cc] c)
  (match-define [coordinate rd cd] d)
  (cond
    [(< rc rd) #true]
    [(> rc rd) #false]
    [(= rc rd) (< cc cd)]))

(module+ examples
  
  (define coord0 (list #s(coordinate +3 0) #s(coordinate +3 +1) #s(coordinate +3 +2)))
  (define coord1 (list #s(coordinate +2 +1)))
  (define coord2 (list #s(coordinate 0 -1)   #s(coordinate +1 -1)))
  (define coord3 (list #s(coordinate -1 -1) #s(coordinate +2 -1)))
  (define coord4 (list #s(coordinate +3 +3) #s(coordinate +4 +3)))
  (define coord5 (list #s(coordinate -1 -2)  #s(coordinate -1 -3)))
  (define coord6 (list #s(coordinate 0 -3)  #s(coordinate +1 -3)))
  (define coord7 (list #s(coordinate +1 -2)    #s(coordinate +2 -2)))
  (define coord8 (list #s(coordinate -1 0)))
  (define coord9 (list #s(coordinate +4 0) #s(coordinate +4 -1) #s(coordinate +4 +1)))
  (define coord10 (list #s(coordinate +3 +4) #s(coordinate +4 +4)))

  (define +starter-coor (coordinate 0 -1))

  (define lshaped-coordinates
    [list (coordinate -1  0)
          (coordinate +1  0)
          (coordinate +1 +1)
          (coordinate +1 +2)
          (coordinate +1 +3)
          (coordinate  0 +3)]))

;; ---------------------------------------------------------------------------------------------------
(module+ json
  (define ROW 'row)
  (define COLUMN 'column)
  
  #; {JSexpr -> (U False Coordinate)}
  (define (jsexpr->coordinate j)
    (match j
      [(hash-table [(? (curry eq? ROW)) (? integer? r)] [(? (curry eq? COLUMN)) (? integer? c)])
       (coordinate r c)]
      [_ (eprintf "tile object does not match schema\n  ~a\n" (jsexpr->string j))
         #false]))
  
  #; {Coordinate -> JCoordinate}
  (define (coordinate->jsexpr rb)
    (match-define [coordinate r c] rb)
    (hasheq ROW r COLUMN c)))

(module+ test
  (check-equal? (jsexpr->coordinate (coordinate->jsexpr (first coord0))) (first coord0)))