#lang racket

;; a primitive observer that saves all images in Tmp/ and allows users to view game developments

(require Qwirkle/Referee/observer-interface)

(provide-observer-interface observer)

;                                                                  
;                                                                  
;                                      ;                           
;                                                                  
;                                                                  
;    ;;;;    ;;;;    ;;; ;  ;    ;   ;;;     ;;;;    ;;;;    ;;;;  
;    ;;  ;  ;    ;  ;;  ;;  ;    ;     ;     ;;  ;  ;    ;  ;    ; 
;    ;      ;;;;;;  ;    ;  ;    ;     ;     ;      ;;;;;;  ;      
;    ;      ;       ;    ;  ;    ;     ;     ;      ;        ;;;;  
;    ;      ;       ;    ;  ;    ;     ;     ;      ;            ; 
;    ;      ;;   ;  ;;  ;;  ;   ;;     ;     ;      ;;   ;  ;    ; 
;    ;       ;;;;;   ;;; ;   ;;; ;   ;;;;;   ;       ;;;;;   ;;;;  
;                        ;                                         
;                        ;                                         
;                        ;                                         
;                                                                  

(require Qwirkle/Referee/ref-state)
(require Qwirkle/scribblings/spec)
(require (submod Qwirkle/Referee/ref-state json))
(require 2htdp/universe)
(require 2htdp/image)
(require json)
(require (only-in racket/gui get-file get-text-from-user))

;                                                                  
;                                                                  
;           ;                                                      
;           ;                                                      
;           ;                                                      
;    ;;;;   ; ;;;    ;;;;    ;;;;    ;;;;   ;    ;   ;;;;    ;;;;  
;   ;;  ;;  ;;  ;;  ;    ;  ;    ;   ;;  ;  ;;  ;;  ;    ;   ;;  ; 
;   ;    ;  ;    ;  ;       ;;;;;;   ;       ;  ;   ;;;;;;   ;     
;   ;    ;  ;    ;   ;;;;   ;        ;       ;  ;   ;        ;     
;   ;    ;  ;    ;       ;  ;        ;       ;;;;   ;        ;     
;   ;;  ;;  ;;  ;;  ;    ;  ;;   ;   ;        ;;    ;;   ;   ;     
;    ;;;;   ; ;;;    ;;;;    ;;;;;   ;        ;;     ;;;;;   ;     
;                                                                  
;                                                                  
;                                                                  
;                                                                  

(define *complete   #false) ;; (when *complete .. the game is over ..)
(define *live-list '[])     ;; a sequence of referee states 

#; {Command -> Void}
(define (observer s)
  (cond
    [(false? s) ;; expensive but 
     (set! *complete  (map (λ (x) (list x (scale .77 (render-ref-state x)))) (reverse *live-list)))
     (save-all-as-pngs)
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
(define xxx "use ->, <-, space, or \"s\"")
(define (show i) (overlay/align 'right 'bottom (text xxx 22 'grey) (second (list-ref *complete i))))

#; {Natural -> Natural}
(define (back-or-forth i key-event)
  (cond
    [(key=? key-event "left")  (sub1/nat i)]
    [(key=? key-event "right") (add1/nat i)]
    [(key=? key-event " ") (save-state-as-json i)]
    [(key=? key-event "s") (save-state-as-image i)]
    [else i]))

#; {Natural -> Natural}
(define (add1/nat i) (min (sub1 (length *complete)) (+ i 1)))

#; {Natural -> Natural}
(define (sub1/nat i) (max 0 (sub1 i)))

#; {[X] [RefState -> X] [X -> Void] -> Natural -> Natural}
;; EFFECT use 'state->` to render the `i`th state; then ask user to select file and save with write
(define ([save-state-as state-> write] i #:file (f #false))
  (define state0 (list-ref *complete i))
  (define output (state-> state0))
  (define file   (or f (get-file) (get-text-from-user "name a file" "specify a file")))
  (when file (with-output-to-file file #:exists 'replace (λ () (write output file))))
  i)

#; {-> Void}
;; save all PNGs in `*complete` in `Tmp/0.png` ...
(define (save-all-as-pngs)
  (unless (directory-exists? Tmp/)
    (make-directory Tmp/))
  (parameterize ([current-directory Tmp/])
    (for ([i (in-naturals)] [_ *complete])
      (define f (~a i PNG))
      [(save-state-as second save-image) i #:file f])))
      
#; {Natural -> Natural}
(define save-state-as-json (save-state-as (compose state->jsexpr first) (λ (j _) (write-json j))))

(define save-state-as-image (save-state-as second save-image))
