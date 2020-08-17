#lang scheme
(define (f a)
  (define (g a b c)
  (if (empty? a) c (g (cdr a) (append b (list (car a) ) )  (cons (append b (list (car a) ) ) c) ) ) )
  (reverse (g a null null)) )
