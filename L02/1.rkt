#lang scheme
(define (f a)
  (if (< (abs a) 10) (abs a) (min (remainder (abs a) 10) (f (quotient (abs a) 10) ) ) ) )
