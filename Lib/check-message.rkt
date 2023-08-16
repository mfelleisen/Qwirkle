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
  (let ([os  (open-output-string)]
        [err #false]
        [mg  (~a "looking for " rgxp)])
    (begin0
      (parameterize ([port os])
        (with-handlers ([exn:fail? (λ (xn) (set! err xn))])
          body ...))
      (close-output-port os)
      (when err
        (eprintf "check-message: exn intercepted:\n")
        (fprintf (current-error-port) (exn-message err))
        (eprintf "\n------------------\n")
        (raise err))
      (let* ([out (get-output-string os)]
             [ok? (cons? (regexp-match rgxp out))])
        (check-true ok? mg)
        (unless ok?
          (eprintf "output string is: \n")
          (fprintf (current-error-port) out)
          (eprintf "\n------------------\n"))))))

(module+ test
  (check-message current-output-port #px"hello" (printf "hello")))