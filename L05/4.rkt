#lang scheme
(define (f a)
  (define (g a b i)
    (if (empty? a) b (if (= 0 (remainder (car a) i ) ) (g (cdr a) (cons (car a) b) (+ 1 i) ) (g (cdr a) b (+ 1 i) ) ) ) )
(if (empty? (cdr a) ) (if (= 0 (car a) ) (reverse (g (cdr a) (cons 0 null ) 1) ) null )
  (if (= 0 (car a) ) (reverse (g (cdr a) (cons 0 null ) 1) ) (reverse (g (cddr a) (cons (cadr a) null ) 2) ) ) ) )
