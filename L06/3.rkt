#lang scheme
(define (fib? n) 
 (define (g a b)
   (if (and (>= n a) (< n b ) ) a (g b (+ a b) ) ) )
 (and (> n -1) (= n (g 1 0) ) ) )
(define (f a)
  (define (g a k)
    (if (empty? a) k
        (if (fib? (car a) ) (g (filter (lambda (x) (not (= x (car a) ) ) ) a) (+ k 1) ) (g (cdr a) k) ) ) )
  (g a 0) )
