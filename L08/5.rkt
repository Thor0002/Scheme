#lang scheme
(define (f n)
  (define b (build-list (- n 1) (λ(x)(+ x 1) ) ) )
  (reverse (map reverse (foldl (λ(x res) (cons (append (cdr (car res)) (list (car (car res) )  ) ) res ) ) (list (reverse b) ) (build-list (- n 2) +) ) ) ) )
