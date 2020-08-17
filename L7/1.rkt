#lang scheme
(define (f a)
  (filter (λ(x) (= 0 (remainder (car x) (cdr x) ) ) ) (map cons a (build-list (length a) (λ(x) (+ x 1) ))) ) )
