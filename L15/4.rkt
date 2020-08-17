#lang scheme
(define data car)
(define left cadr)
(define right caddr)
(define empty-tree? empty?)

(define (min2 a b)
  (define m (min a b) )
  (cond ((= m a) a) ((= m b) b) ) )

(define (f tree)
  (if (empty-tree? tree) +inf.0
      (if (and (empty-tree? (left  tree) )
               (empty-tree? (right tree) ) )
          (data tree)
          (min2 (f (left tree) ) (f (right tree) ) ) ) ) ) 
                                             
 