#lang scheme
(define (fact n)
  (define (g a i)
    (if (= i n) (* i a) (g (* i a) (+ i 1) ) ) )
  (g 1 1) )
(define (f a) 
 (define (g a n)
   (if (> (fact n) a ) (= (fact (- n 1) ) a) (g a (+ 1 n) ) ) )
 (g a 1) )
   

