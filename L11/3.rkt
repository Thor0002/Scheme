#lang scheme
(define (sd x)
  (define (g s i)
    (if (> (* i i) x ) s
        (g (if (= (remainder x i) 0) (if (= i (/ x i) ) (+ s i) (+ s i (/ x i) ) ) s) (+ i 1) ) ) ) 
  (g 1 2) )
(define (f a)
  (define (g a)
    (if (empty? a) #f
        (let ((x (sd (car a))))
          (if (member x (cdr a) ) (if (and (not (= x (car a) ) ) (= (car a) (sd x) ) ) (cons (car a) x) (g (cdr a) ) ) (g (cdr a) ) ) ) ) )
  (g a) )