#lang scheme
(define (f a)
 (define ta (apply map list a) )
  (define s (foldl (λ(l1 sres)
          (let ((t (argmin + l1) ) ) (foldl (λ(l2 el) (if (= t (argmax + l2) ) (cons t el) el) ) sres ta) ) )
         null a) )
  (if (empty? s) #f s) )
  
  
