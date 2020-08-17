#lang scheme

(define (add j i vec)
  (vector-set! vec i (cons j (vector-ref vec i) ) ) )
(define (f G)
  (define n (length G) )
  (define vG (make-vector n null) )
  (foldl (λ(x i res)
         (foldl (λ(y res)(add i y vG) ) res x) )
          null G (build-list n +) )
  (vector->list vG) )
