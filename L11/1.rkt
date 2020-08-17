#lang scheme
(define % remainder)
(define (ok? x)
  (define (g i)
    (if (> (* i i) x) #f
        (or (and (= 0 (% x i) ) (= 0 (% (/ x i) i) ) ) (g (+ i 1) ) ) ) )
  (g 2) )
(define (f a b)
  (filter ok? (build-list (+ 1 (- b a) ) (λ(i)(+ i a) ) ) ) )
