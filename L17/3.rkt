#lang scheme
(define data car)
(define left cadr)
(define right caddr)
(define empty-tree? empty?)
(define (leaf? tree) (and (empty? (left tree) ) (empty? (right tree) ) ) )

(define (f tree)
  (if (empty? tree) null
      (if (leaf? tree) tree
          (if (empty? (right tree) ) (f (left tree) )
              (if (empty? (left tree) ) (f (right tree) )
                  (list (data tree) (f (left tree) ) (f (right tree) ) ) ) ) ) ) )