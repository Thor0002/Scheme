#lang scheme
(define data car)
(define left cadr)
(define right caddr)
(define empty-tree? empty?)
(define (leaf? tree) (and (empty? (left tree) ) (empty? (right tree) ) ) )

(define (map-tree proc tree . a)
  (if (empty-tree? tree) null
      (list (apply proc (map data (cons tree a) ) )
            (apply map-tree proc (left tree)  (map left a)  )
            (apply map-tree proc (right tree) (map right a) ) ) ) )

;(define (g ltree)
;  (if (empty? ltree) (list null null null )
;  (let ((rec (g (cdr ltree) ) ) )
;    (list (cons (data (car ltree) ) (car rec) )
;          (cons (left (car ltree) ) (cadr rec) )
;          (cons (right(car ltree) ) (caddr rec) ) ) ) ) )
