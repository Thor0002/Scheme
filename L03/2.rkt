#lang scheme
(define (f n)
  (define (g a i)
    (if (> a n) (cons (- i 1) (quotient a 2) ) (g (* 2 a) (+ 1 i) ) ) )
  (define b (g 1 0) )
  (if (= n (cdr b) ) (car b) false) )
