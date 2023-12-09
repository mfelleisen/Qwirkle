#lang racket

;; a utility for retrieving JSON from STDIN and check for well-formedness and validity with a predicate 

(provide
 ILL ;; ill-formed JSON 
 INV ;; invalid JSON 
 
 #; {[JSexpr -> X ...] Symbol String -> X ...}
 get)

;; ---------------------------------------------------------------------------------------------------
(require SwDev/Testing/communication)

;; ---------------------------------------------------------------------------------------------------
(define ILL "ill-formed JSON")
(define INV "invalid JSON: ")

;; read a JSON value from current input port, validate and map to internal data ~~ or (error tag msg)
(define (get validator tag msg #:eof-okay? (eof? #f) #:empty-okay? (eok? #f) #:dont-exit (no-exit #f))
  (define x (read-message))
  (cond
    [(eof-object? x)  (unless eof? (stop tag no-exit "missing JSON"))]
    [(and (string? x) (regexp-match #px"ERROR" x)) (stop tag no-exit "~a:\n ~a" ILL x)])
  (define pieces (call-with-values (λ () (validator x)) list))
  (unless (or eok? (first pieces))
    (pretty-print x (current-error-port))
    (stop tag no-exit "~a ~a: see above" INV msg))
  (apply values pieces))

(define (stop tag no-exit base . x)
  (send-message (string-append (~a tag) ": things went wrong"))
  (apply eprintf (format "~a: ~a\n" tag base) x)
  (if no-exit (apply error tag (format "~a: ~a\n" tag base) x) (exit 1)))

(module+ test
  (require rackunit)
  (check-equal?
   (with-output-to-string
     (λ ()
       (check-exn exn:fail?
                  (λ ()
                    (with-input-from-string "" (λ () (get values "test" "mt" #:dont-exit #true)))))))
   "\"test: things went wrong\"\n"))
