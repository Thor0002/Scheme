#lang scheme
(define (notzero? a)
  (if (empty? a) true
  (and (not (= 0 (car a) ) ) (notzero? (cdr a) ) ) ) )
(define (f a)
  (foldl (λ(x res)(if (notzero? x) (+ res 1) res) ) 0 a) )
  
