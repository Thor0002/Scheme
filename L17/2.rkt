#lang racket
(define data car)
(define left cadr)
(define right caddr)
(define empty-tree? empty?)
(define (leaf? tree) (and (empty? (left tree) ) (empty? (right tree) ) ) )

(define (foldl-tree proc init tree . a)
  (define (g res tree a)
    (if (empty? tree) res
        (g (g (apply proc (append (cons (data tree) (map data a) ) (list res) ) )
              (left tree)  (map left a) )
              (right tree) (map right a) ) ) )
  (g init tree a) )

(define (foldl-tree1 proc init tree . a)
  (define (tree->list tree)
    (if (empty? tree) null
        (cons (data tree) (append (tree->list (left tree) ) (tree->list (right tree) ) ) ) ) )
  (apply foldl proc init (map tree->list (cons tree a) ) ) )

(define (foldl-tree2 proc init tree . a)
  (apply foldl proc init (flatten tree) (map flatten a) ) )


        
          
      
  