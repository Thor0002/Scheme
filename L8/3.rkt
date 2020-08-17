#lang scheme
(define (f a)
  (define b (apply map list a) )
  (andmap (λ(x)(andmap (λ(y)(= y 0) ) x) )
         (cdr (map (λ(z i)(take z i) ) b (build-list (length a) + ) ) ) ) )

  (define (f1 a)
 (define l (length a) )
 (andmap (λ(x)(andmap (λ(y)(= y 0) ) x) )
         (map (λ(z i)(take (reverse z) (- l i) ) ) a (build-list (length a) (λ(k)(+ k 1) ) ) ) ) )

  (define (f2 a)
  (andmap (λ(x)(= x 0) ) (car (foldl (λ(l i)(cons (car (foldl(λ(x j)
                (if (> (cdr j) (cdr i))
                    (cons (cons x (car j) ) (+ (cdr j) 1) )
                    (cons (car j) (+ 1 (cdr j) ) ) ) ) (cons (car i) 1) l) ) (+ 1 (cdr i) ) ) )
         (cons null 1) a) ) ) )