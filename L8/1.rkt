#lang scheme
(define (f a b)
  (map (λ(l1 l2)(map (λ(x y)(+ x y) ) l1 l2) ) a b) )
