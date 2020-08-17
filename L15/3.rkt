#lang scheme
(define data car)
(define left cadr)
(define right caddr)
(define empty-tree? empty?)

(define (f tree)
  (if (empty-tree? tree) 0
      (if (empty-tree? (left tree) ) (f (right tree) )
          (+ 1 (f (left tree) )
               (f (right tree) ) ) ) ) )
        
