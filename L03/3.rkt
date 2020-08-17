#lang scheme
(define (f a n)
  (define (g b i)
    (if (= i n) (display b)
        (and (display b ) (display " ")
    (if (= (remainder b 2) 0)  (g (quotient b 2) (+ i 1) )   (g (+ 1 (* 3 b) ) (+ 1 i) ) ) ) ) )
  (g a 1) )
