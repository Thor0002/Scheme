#lang scheme
(define (f a x y)
  (if (>= y 0)
      (abs (- a (sqrt (+ (* x x) (* y y) ) ) ) )
      (if (and (> (abs x) a) (< y (- a) ) )
          (sqrt(+ (sqr (- (abs x) a)) (sqr(+ a y) ) ) )
          (if (and (<= y x) (<= y (- x) ) )
              (abs (+ a y))
              (abs (- a (abs x) ) ) ) ) ) ) 
