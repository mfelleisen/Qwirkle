#lang racket

(provide
 FLUSH SHOW
 
 #; {type Command
          = (U False          ;; end of game
               FLUSH          ;; get ready for next game 
               SHOW           ;; open a window so that a user can view and inspect the game states 
               RefereeState)}
 
 #; {Command -> Void}
 ;; collect game states for viewing and inspection after the game is complete 
 ;; <--    previous state
 ;; -->    next state
 ;; blank  save current state via file dialog
 ;; (By deleting the `unless` protection, the observer would work on "live" games)
 textual-observer)

;; ---------------------------------------------------------------------------------------------------
(require Qwirkle/Referee/ref-state)
(require 2htdp/universe)
(require 2htdp/image)

;; ---------------------------------------------------------------------------------------------------
;; a primitive textual observer 

(define FLUSH (gensym 'flush))
(define SHOW  (gensym 'show))

(define *complete   #false) ;; (when *complete .. the game is over ..)
(define *live-list '[])         ;; a sequence of referee states 

#; {Command -> Void}
(define (textual-observer s)
  (cond
    [(false? s)
     (set! *complete  (reverse *live-list))
     (set! *live-list '[])]
    [(eq? FLUSH s)
     (set! *complete #false)]
    [(eq? SHOW  s)
     (when (cons? *complete)
       (big-bang 0 [to-draw show] [on-key back-or-forth]))
     (void)]
    [else
     (unless *complete (set! *live-list (cons s *live-list)))
     textual-observer]))

#; {Natural -> Image}
(define (show i)
  (render-ref-state (list-ref *complete i)))

#; {Natural -> Natural}
;; 
(define (back-or-forth i key-event)
  (cond
    [(key=? key-event "left")  (sub1/nat i)]
    [(key=? key-event "right") (add1/nat i)]
    [(key=? key-event "space") (save-file i)]
    [else (void)]))

#; {Natural -> Natural}
(define (add1/nat i)
  (define next (+ i 1))
  (min (sub1 (length *complete)) next))

#; {Natural -> Natural}
(define (sub1/nat i)
  (max 0 (sub1 i)))

#; {Natural -> Natural}
(define (save-file i)
  (eprintf "file saving not iomplemented yet\n")
  i)