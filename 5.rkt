#lang racket
(define (f x1 y1 x2 y2)
  (if (or (not (= y1 7) ) (not (= y2 8) ) )
  (or (and (>= y1 y2) (<= (abs (- x2 x1) ) y1 ) )
      (and (<= (abs (- x2 x1) ) 1 ) (<= (abs (- y2 y1) ) 1 ) )
      (and (= y2 (+ 1 y1 ) ) (<= (abs (- x2 x1) ) y1 ) ) )
  (or
      (and (<= (abs (- x2 x1) ) 1 ) (<= (abs (- y2 y1) ) 1 ) ) ) ) ) 