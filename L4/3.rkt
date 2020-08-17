#lang scheme
(define (f x)
  (define (g a b i)
    (if (empty? (cddr b)) (if (and (>= (car b) a) (>= (car b) (cadr b) ) ) (car b) (cadr b) )
        (if (and (>= (car b) a) (>= (car b) (cadr b) ) ) (car b) (g (car b) (cdr b) (+ i 1) ) ) ) )
  (if (empty? (cdr x) ) (car x) (if (>= (car x) (cadr x) ) (car x) (g (car x) (cdr x) 2) ) ) )
