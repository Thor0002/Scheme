#lang scheme
(define (f a)
  (define (g a lM m)
    (if (empty? a) lM
    (if (and (andmap (λ(i)(and (>= i 0) (<= i 9) ) ) (car a) ) (<= (apply * (car a) ) m) )
        (g (cdr a) (car a) (apply * (car a) ) )
        (g (cdr a) lM m) ) ) )
  (g a (if (andmap (λ(i)(and (>= i 0) (<= i 9) ) ) (car a) ) (car a) null) (apply * (car a) ) ) )
