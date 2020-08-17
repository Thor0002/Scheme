#lang racket
(define (f s0 p a)
(define b (foldl
           (λ(x res) (cons ((eval (cadr x) ) (* (car res) (expt (+ 1.0 (/ p 100) ) (- (car x) (cdr res) ) ) ) (caddr x) ) (car x) ) )
           (cons s0 1)
           (sort a #:key car <) ) )
 (* (car b) (expt (+ 1.0 (/ p 100) ) (- 31 (cdr b) ) ) ) )
