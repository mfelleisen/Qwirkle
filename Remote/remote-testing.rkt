#lang racket

;; a facility for testing remote players 

(provide
 #;(test make-remote-player
         (m args ...)
         (~optional (~seq #:remote input) #:defaults ([input #'(~a "void")]))
         (~optional (~seq #:exp e) #:defaults ([e #'(void)]))
         #:msg m)
 ;; creates a remote player, invokes method `m`, which then sends message `m` across the "wire"
 ;; #:remote and #:exp (with "void" defaults) set up a second ("inner") test case about retirn values:
 ;; #:remote specifies what the client sends in
 ;; #:exp specifies what the expected result is 
 test-remote)

;; ---------------------------------------------------------------------------------------------------
(require SwDev/Debugging/diff)
(require (for-syntax syntax/parse))
(require rackunit)
(require json)

;; ---------------------------------------------------------------------------------------------------
(define ((report-diff e) x)
  (cond
    [(and (eq? x (void)) (eq? e (void)))
     ;; special case
     #true]
    [(diff x e)
     =>
     (λ (δ)
       (pretty-print `[actual] (current-error-port))
       (newline (current-error-port))
       (pretty-print x (current-error-port))
       (newline (current-error-port))
       (pretty-print `[expected] (current-error-port))
       (newline (current-error-port))
       (pretty-print e (current-error-port))
       (newline (current-error-port))
       (pretty-print δ (current-error-port))
       #false)]
    [else #t]))
  
(define-syntax (test-remote stx)
  ; #:remote JSexpr ~~ what the client responds with
  ; #:exp JSexpr    ~~ what we expect the method to produce in response
  ; #:msg JSexpr    ~~ what the method sends to the client 
  (syntax-parse stx 
    [(_ make-remote-player name:id
        (method args ...)
        (~optional (~seq #:remote input) #:defaults ([input #'(~a "void")]))
        (~optional (~seq #:exp e) #:defaults ([e #'(void)]))
        #:msg m)
     #'(check-pred
        (report-diff m)
        (string->jsexpr
         (with-output-to-string
           (λ ()
             (check-pred ; check-equal?
              (report-diff e)
              (with-input-from-string (jsexpr->string input)
                (λ ()
                  (define ip (current-input-port))
                  (define op (current-output-port))
                  (define r1 (make-remote-player (~a 'name) ip op))
                  (send r1 method args ...)))
              (~a "exoected result of call: " m)))))
        (~a "call as message: " m))]))
