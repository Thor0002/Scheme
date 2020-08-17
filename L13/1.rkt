#lang scheme
(define (f a)
  (define (g a)
  (if (empty? (cdr a) ) null
      (cons (* (car a) (- (length a) 1) ) (g (cdr a) ) ) ) )
  (if (empty? (cdr a) ) (list 0) (g a) ) )
