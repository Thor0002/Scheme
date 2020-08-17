#lang scheme
(define (f x k)
  (cons (quotient x (expt 10 k) ) (remainder x (expt 10 k) ) ) )
