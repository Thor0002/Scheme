#lang scheme
(define (f a)
(car(foldl (λ(x res) (let ((k (count zero? x) )) (if (>= k (cdr res) ) (cons x k) res) ) ) (cons 0 0) a) ) )
