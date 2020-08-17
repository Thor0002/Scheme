#lang scheme
(define (f a)
  (define (g s i)
    (if (> i (sqrt a) ) s
        (g (if (= (remainder a i) 0) (+ s i (quotient a i) ) s) (+ i 1) ) ) ) 
(and (> a 1) (= (g 1 2) a) ) )
    