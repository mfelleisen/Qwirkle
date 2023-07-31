#lang racket

(provide
 ;; SYNTAX
 #; (check-message port re body ...)
 ;; returns the result of `body ...`
 ;; captures all output of `body ...` on `port` and regexp-matches against `re`
 check-message)

;; -----------------------------------------------------------------------------
(require rackunit)

;; -----------------------------------------------------------------------------
(define-syntax-rule (check-message port rgxp body ...)
  (let ([os (open-output-string)]
        [mg (~a "looking for " rgxp)])
    (begin0
      (parameterize ([port os])
        body ...)
      (close-output-port os)
      (check-true (cons? (regexp-match rgxp (get-output-string os))) mg))))

(module+ test
  (check-message current-output-port #px"hello" (printf "hello")))