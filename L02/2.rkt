#lang scheme
(define mod remainder)
(define div quotient)
(define (f a)
  (or (< (abs a) 10) (and (= (mod (abs a) 10) (div (mod (abs a) 100) 10) ) (f (div (abs a) 10) ) ) ) )
