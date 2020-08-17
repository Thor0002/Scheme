#lang scheme
(define (f a)
  (foldl + 0 (filter (λ(x) (and (< (abs x) 1000) (> (abs x) 99) ) ) a) ) )