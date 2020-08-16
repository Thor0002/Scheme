#lang scheme
(define (f a b) 
  (define h (if (< a 12) (+ (* a 5) (* (/ b 60) 5) )  (+ (* 5 (- a 12) ) (* (/ b 60) 5) ) ) )
  (define x (* (abs (- h b) ) 6) )
  (if (> x 180) (- 360 x) x ) )