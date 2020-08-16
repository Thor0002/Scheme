#lang scheme
(define (f x1 y1 x2 y2)
  (if (= (abs (- x1 x2) ) (abs (- y1 y2) ) ) 1
    (if (= (remainder (+ x1 y1) 2) (remainder (+ x2 y2) 2) ) 2 (- 1) ) ) )