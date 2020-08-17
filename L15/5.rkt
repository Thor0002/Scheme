#lang scheme
(define data car)
(define left cadr)
(define right caddr)
(define empty-tree? empty?)

(define (map+app l1 l2)
  (if (empty? l1) l2
      (if (empty? l2) l1
          (cons (append (car l1) (car l2) )
                (map+app (cdr l1) (cdr l2) ) ) ) ) )
(define (f tree)
  (define (g tree)
    (if (empty-tree? tree) null
        (cons (list (data tree) )
              (map+app (g (left tree) ) (g (right tree) ) ) ) ) )
  (if (empty-tree? tree) #f (g tree) ) )
