#lang scheme
(define (f a)
(define ta (apply map list a) )
 (foldl (λ(x r1)(foldl (λ(y r2)(if (equal? x y) x r2) ) r1 ta) ) #f a) ) 

