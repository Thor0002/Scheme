#lang scheme
(define (kill a n)
  (define (g a k)
    (if (empty? a) k
        (if (= (car a) n) (g (cdr a) (+ k 1) ) (g (cdr a) k) ) ) )
  (g a 0) )
(define (f a)
  (define (g a k)
    (if (empty? a) k
        (let ((b (car a) ) )
          (g (filter (lambda(x)(not(= x b) ) ) a) (- k (- (kill a (car a) ) 1) ) ) ) ) )
  (g a (length a) ) )
