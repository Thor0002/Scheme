#lang scheme
(define data car)
(define left cadr)
(define right caddr)
(define empty-tree? empty?)

(define (f tree)
  (define (g tree)
    (if (empty-tree? tree) (cons -inf.0 0)
        (let ((l (sort (list (cons (data tree) 1) (g (left tree) ) (g (right tree) ) ) #:key car >) ) )
          (cond ( (> (caar l) (caadr l) ) (car l) )
                ( (> (caadr l) (caaddr l)  ) (cons (caar l) (+ (cdar l) (cdadr l) ) ) )
                (else (cons (caar l) (+ (cdar l) (cdadr l) (cdaddr l) ) ) ) ) ) ) )
  (cdr (g tree) ) )









;(define (f dtree)
;    (define (g tree)
;      (if (empty-tree? tree) (data dtree) 
;      (max (data tree) (g (right tree) ) (g (left tree) ) ) ) )
;  (define maxel (g dtree) )
;  (define (h tree)
;    (if (empty-tree? tree) 0
;        (+ (if (equal? maxel (data tree) ) 1 0) (h (left tree) ) (h (right tree) ) ) ) )
; (h dtree ) )