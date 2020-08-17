#lang scheme
(define (pr1 a)
  (define (g i)
    (if (> i (sqrt a)  ) true (if (> (remainder a i) 0 ) (g (+ 1 i) ) false) ) )
  (g 2) )
(define (f a)
  (define (g i)
    (if (> i (sqrt a) ) 0 (if (= 0 (remainder a i) ) i (g (+ i 1) ) ) ) )
  (if (= 0 (g 2) ) false  (and (pr1 (g 2) ) (pr1 (quotient a (g 2) ) ) ) ) ) 