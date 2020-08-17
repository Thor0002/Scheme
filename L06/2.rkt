#lang scheme
(define (kill a n)
  (define (g a k)
    (if (empty? a) k
        (if (= (car a) n) (g (cdr a) (+ k 1) ) (g (cdr a) k) ) ) )
  (g a 0) )
(define (f a)
  (define (g a b)
    (if (empty? a) b
        (let ((c (car a) ) )
          (g (filter (lambda(x)(not(= x c) ) )  a) (cons (cons (car a) (kill a (car a) ) )  b) ) ) ) ) 
  (reverse (g a null) ) )
