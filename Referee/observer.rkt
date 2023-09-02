#lang racket

(provide
 FLUSH SHOW
 
 #; {type Command
          = (U False          ;; end of game
               FLUSH          ;; get ready for next game 
               SHOW           ;; open a window so that a user can view and inspect the game states 
               RefereeState)} ;; record state 
 
 #; {μf.Command -> (U f Void)}
 ;; collect game states for viewing and inspection after the game is complete 
 ;; <--    previous state
 ;; -->    next state
 ;; blank  save current state via file dialog
 ;; (By deleting the `unless` protection, the observer would work on "live" games)
 observer)

;; ---------------------------------------------------------------------------------------------------
(require Qwirkle/Referee/ref-state)
(require (submod Qwirkle/Referee/ref-state json))
(require 2htdp/universe)
(require 2htdp/image)
(require json)
(require (only-in racket/gui get-file get-text-from-user))

;; ---------------------------------------------------------------------------------------------------
;; a primitive textual observer 

(define FLUSH (gensym 'flush))
(define SHOW  (gensym 'show))

(define *complete   #false) ;; (when *complete .. the game is over ..)
(define *live-list '[])         ;; a sequence of referee states 

#; {Command -> Void}
(define (observer s)
  (cond
    [(false? s)
     (set! *complete  (map (λ (x) (list x (scale .77 (render-ref-state x)))) (reverse *live-list)))
     (set! *live-list '[])]
    [(eq? FLUSH s)
     (set! *complete #false)]
    [(eq? SHOW  s)
     (when (cons? *complete)
       (define i0 (index-of *complete (argmax (compose image-height second) *complete)))
       (big-bang i0 [to-draw show] [on-key back-or-forth]))
     (void)]
    [else
     (unless *complete (set! *live-list (cons s *live-list)))
     observer]))

#; {Natural -> Image}
(define (show i)
  (second (list-ref *complete i)))

#; {Natural -> Natural}
;; 
(define (back-or-forth i key-event)
  (cond
    [(key=? key-event "left")  (sub1/nat i)]
    [(key=? key-event "right") (add1/nat i)]
    [(key=? key-event " ") (save-file i)]
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
  (define state0 (first (list-ref *complete i)))
  (define jstate (state->jsexpr state0))
  (define file   (or (get-file) (get-text-from-user "new json file" "specify a file")))
  (when file
    (with-output-to-file file #:exists 'replace (λ () (write-json jstate))))
  i)