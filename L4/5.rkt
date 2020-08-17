#lang scheme
(define (f a n)
  (define (g a s i)
    (if (empty? a) s (if (< (abs (car a) )  (expt 10 n) ) (g (cdr a) (* s (car a) ) (+ i 1) ) (g (cdr a) s (+ i 1) ) ) ) )
  (g a 1 1) )