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
      (let* ([out (get-output-string os)]
             [ok? (cons? (regexp-match rgxp out))])
        (check-true ok? mg)
        (unless ok? (eprintf "output string is ~s\n" out))))))

(module+ test
  (check-message current-output-port #px"hello" (printf "hello")))