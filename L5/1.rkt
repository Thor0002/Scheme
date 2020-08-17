#lang scheme
(define (f n)
  (define (g p k a i)
  (if (> (* i i) n) (if (empty? a) (cons (cons n 1) null) (if (> p 1) (cons (cons p 1) a ) a) )
  (if (= 0 (remainder p i) ) (g (quotient p i) (+ k 1) a i)  (if (= k 0) (g p 0 a (+ 1 i) ) (g p 0 (cons (cons i k) a ) (+ 1 i) ) ) ) ) )
  (reverse (g n 0 null 2) ) )
