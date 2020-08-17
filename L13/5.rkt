#lang scheme
(define (f a x e)
  (define a1 (f1 a) )
  (define (g xn xn-1)
    (if (< (abs (- xn xn-1) ) e) xn
        (g (- xn (/ (fx xn a) (fx xn a1 ) ) ) xn ) ) )
  (g (- x (/ (fx x a) (fx x a1) ) ) x) )




(define (fx x a)
  (foldl (λ(i res)(+ (* res x) i) ) 0 a) )
(define (f1 a)
  (define (g a)
  (if (empty? (cdr a) ) null
      (cons (* (car a) (- (length a) 1) ) (g (cdr a) ) ) ) )
  (if (empty? (cdr a) ) (list 0) (g a) ) )
