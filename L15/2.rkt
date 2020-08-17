#lang scheme
(define data car)
(define left cadr)
(define right caddr)
(define empty-tree? empty?)

(define (f tree)
  (if (empty-tree? tree) tree
      (list (data tree)
            (f (right tree) )
            (f (left  tree) ) ) ) )
