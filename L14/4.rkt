#lang scheme
(define data car)
(define left cadr)
(define right caddr)
(define empty-tree? empty?)

(define (f tree)
  (define (g tree)
    (if (empty-tree? tree) 0
        (if (and (empty-tree? (left tree) ) (empty-tree? (right tree) ) ) 1
            (+ (g (left tree) ) (g (right tree) ) ) ) ) )
  (g tree) )