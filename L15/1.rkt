#lang scheme
(define data car)
(define left cadr)
(define right caddr)
(define empty-tree? empty?)

(define (f tree)
  (if (empty-tree? tree) #f
      (if (and (empty-tree? (left  tree) )
               (empty-tree? (right tree) ) ) #f
          (if (empty-tree? (left  tree) )
              (if (= 0 (remainder (data tree) (data (right tree) ) ) ) (data tree) #f)
              (if (empty-tree? (right tree) )
                  (if (= 0 (remainder (data tree) (data (left tree) ) ) ) (data tree) #f)
                  (if (or (= 0 (remainder (data tree) (data (right tree) ) ) )
                          (= 0 (remainder (data tree) (data (left tree) ) ) ) )
                      (data tree)
                      (or (f (left  tree) )
                          (f (right tree) ) ) ) ) ) ) ) )
