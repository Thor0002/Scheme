#lang scheme
(define (f n)
  (define (g a)
    (if (< a 10) (= 0 (remainder n a) ) (and (= 0 (remainder n (remainder a 10) ) ) (g (quotient a 10) ) ) ) )
  (if (= n 0) true
  (if (= 0 (remainder n 10) ) false (g n) ) ) )
