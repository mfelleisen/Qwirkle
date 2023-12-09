#lang racket

;; deal with a port number as a command-line arguments 

(provide
 LOCALHOST
 HIGH-PORT 
 LOW-PORT
 
 (contract-out
  [string->port-number
   ;; parse string as valid port number for this project [LOW-PORT, HIGH-PORT]
   (-> string? (and/c (between/c LOW-PORT HIGH-PORT)))]))

;; ---------------------------------------------------------------------------------------------------
(define LOCALHOST "127.0.0.1")
(define HIGH-PORT 60000)
(define LOW-PORT  10000)

;; ---------------------------------------------------------------------------------------------------
(define (string->port-number p-str [low LOW-PORT] [high HIGH-PORT])
  (define p (string->number p-str))
  (unless (and p (port-number? p) (<= low p high))
    (error 'xclient "port number expected in range [~a, ~a], given ~e" low high p-str))
  p)
