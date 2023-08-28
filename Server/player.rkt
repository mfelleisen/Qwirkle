#lang racket

;; this remote player implements the same interface as the player but conveys its arguments
;; to the given TCP out stream and receives the results on the TCP in stream
                                 
(provide
 (contract-out
  (make-remote-player
   (-> string? input-port? output-port? player/c))))

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
    
(require Qwirkle/Common/player-interface)

(require (submod Qwirkle/Common/tiles json))
(require (submod Qwirkle/Common/placement json))
(require (submod Qwirkle/Referee/ref-state json))
(require Qwirkle/Remote/define-remote)
(require (except-in Qwirkle/Lib/json string->jsexpr))

(module+ test
  (require (submod ".."))
  (require (submod Qwirkle/Common/game-state examples))
  (require (submod Qwirkle/Common/tiles examples))
  (require Qwirkle/Referee/ref-state)
  (require Qwirkle/Remote/remote-testing)
  (require rackunit))

;                                            
;                                            
;          ;;;                               
;            ;                               
;   ;;;;     ;    ;;;;   ;   ;   ;;;    ;;;; 
;   ;; ;;    ;        ;  ;   ;  ;;  ;   ;;  ;
;   ;   ;    ;        ;   ; ;   ;   ;;  ;    
;   ;   ;    ;     ;;;;   ; ;   ;;;;;;  ;    
;   ;   ;    ;    ;   ;   ; ;   ;       ;    
;   ;; ;;    ;    ;   ;   ;;    ;       ;    
;   ;;;;      ;;   ;;;;    ;     ;;;;   ;    
;   ;                      ;                 
;   ;                     ;                  
;   ;                    ;;                  


(define (make-remote-player name in out)
  (new remote-player% [n name] [in in] [out out]))

(define remote-player%
  (class object% [init-field in out [n (gensym 'rem)]]
    (super-new)

    (define-define/remote define/remote in out)
    
    (define/public (name) n) ;; it is safe to call this locally 
    ;; -----------------------------------------------------------------------------------------------
    (define/remote (setup pk tiles)  void)
    (define/remote (take-turn pk)    action*)
    (define/remote (new-tiles tiles) void)
    (define/remote (win boolean)     void)))

;                                     
;                                     
;     ;                    ;          
;     ;                    ;          
;   ;;;;;   ;;;    ;;;   ;;;;;   ;;;  
;     ;    ;;  ;  ;   ;    ;    ;   ; 
;     ;    ;   ;; ;        ;    ;     
;     ;    ;;;;;;  ;;;     ;     ;;;  
;     ;    ;          ;    ;        ; 
;     ;    ;      ;   ;    ;    ;   ; 
;     ;;;   ;;;;   ;;;     ;;;   ;;;  
;                                     
;                                     
;                                     

(module+ test
  (check-true
   (is-a? (make-remote-player "hello" (current-input-port) (current-output-port)) remote-player%))

  (check-equal? (send (make-remote-player "N" (current-input-port) (current-output-port)) name) "N")

  (check-true
   (is-a? (new remote-player% [in (current-input-port)] [out (current-output-port)]) remote-player%))

  (define info-special-state (ref-state-to-info-state special-state))
  (define jinfo-special-state (pk->jsexpr info-special-state))
  (define jstarter-tile* (tiles->jsexpr starter-tile*))

  (test-remote make-remote-player b
               (setup info-special-state starter-tile*)
               #:remote (void->jsexpr #f)
               #:exp (void)
               #:msg `["setup" [,jinfo-special-state ,jstarter-tile*]])

  (test-remote make-remote-player d
               (take-turn info-special-state)
               #:remote JPASS
               #:exp PASS
               #:msg `["take-turn" [,jinfo-special-state]])

  (test-remote make-remote-player d
               (new-tiles starter-tile*)
               #:remote (void->jsexpr #f)
               #:exp (void)
               #:msg `["new-tiles" [,jstarter-tile*]])

  (test-remote make-remote-player g
               (win #f)
               #:remote (void->jsexpr #f)
               #:exp (void)
               #:msg '["win" [#f]]))
