#lang scheme
(define data car)
(define left cadr)
(define right caddr)
(define empty-tree? empty?)

(define (f tree)
  (or (empty-tree? tree)
       (or (and (empty-tree? (left tree)  )
                (empty-tree? (right tree) ) ) 
           (and (not (or (empty-tree? (left tree) ) (empty-tree? (right tree) ) ) )
                     (f (left tree) )
                     (f (right tree) ) ) ) ) ) 