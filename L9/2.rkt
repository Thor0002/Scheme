#lang scheme
(define (f n)
  (build-list n (λ(i)(build-list n (λ(j)(if (< j (/ n 2) ) (if (and (<= j i) (>= (- (- n 1) i) j) ) 1 0)
                                                           (if (and (>= j i) (>= j (- (- n 1) i) ) ) 1 0) ) ) ) ) ) )
