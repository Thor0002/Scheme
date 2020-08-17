#lang scheme
(define (fib n)
  (define (g a b i)
    (if (>= i n) b (g b (+ a b) (+ i 1) ) ) )
  (g 1 1 2))
(define (f a) 
 (define (g a n)
   (if (and (>= a (fib n)) (<= a (fib (+ n 1) ) ) ) n (g a (+ 1 n) ) ) )
 (define b (g a 1) )
  (if (< (- a (fib b) ) (- (fib (+ b 1) ) a ) ) (fib b) (fib (+ 1 b) ) ) )
   
