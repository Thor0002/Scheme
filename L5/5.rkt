#lang scheme
(define (obr n)
    (define (g n m)
      (if (< n 10) (+ n (* m 10) ) (g (quotient n 10) (+ (* m 10) (remainder n 10) ) ) ) )
  (g n 0) )
(define (f a b)
  (define (g b c)
    (if (empty? b) c (g (cdr b) (cons (obr (car b) ) c) ) ) )
 (and (equal? b (g a null) ) (equal? a (g b null) ) ) )

  