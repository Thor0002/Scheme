#lang scheme
(define (f a b)
  (foldl (λ(x y result) ( (eval y ) result x) ) (car a) (cdr a) b ) ) 
