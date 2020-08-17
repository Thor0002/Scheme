#lang scheme
(define (f a b)
  (define (g a b c)
  (if (or (empty? a) (empty? b) ) c
      (if (= 1 (gcd (car a) (car b) ) ) (g (cdr a) (cdr b) (cons (cons (car a) (car b) ) c) )
          (g (cdr a) (cdr b) c) ) ) )
  (reverse (g a b null) ) )
