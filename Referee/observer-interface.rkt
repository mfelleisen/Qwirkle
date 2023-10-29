#lang racket

;; an interface for a primitive observer

(provide
 FLUSH SHOW

 ;; SYNATX
 #; 
 (provide-observer-interface observer)
 ;; generates the provide for an observer that satisfies the following basic interface
 #; {type Command
          = (U False          ;; end of game
               FLUSH          ;; get ready for next game 
               SHOW           ;; open a window so that a user can view and inspect the game states 
               RefereeState)} ;; record state 
 
 #; {μf.Command -> (U f Void)}
 ;; collect game states for viewing and inspection after the game is complete 
 ;; <--    previous state
 ;; -->    next state
 ;; blank  save current state as JSON via file dialog
 ;; EXTRA "s" save current state as image via file dialog
 provide-observer-interface)

(define-syntax-rule (provide-observer-interface observer)
  (provide
   FLUSH SHOW
   observer))

(define FLUSH (gensym 'flush))
(define SHOW  (gensym 'show))