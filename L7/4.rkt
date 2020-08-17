#lang scheme
(define (f a)
  (foldl (λ(x result) (if (empty? result) (list (list x) ) (cons (cons  x (car result) ) result) ) ) null (reverse a) ) )