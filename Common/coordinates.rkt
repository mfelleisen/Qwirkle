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

;; -----------------------------------------------------------------------------
(struct coordinate [row column] #:prefab)
(define origin [coordinate 0 0])

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