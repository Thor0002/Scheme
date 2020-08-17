#lang scheme
(define (f a)
  (andmap (λ(l)(andmap (λ(x)(>= x 0) ) l) ) a) )
