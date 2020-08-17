#lang scheme
(define (f list)
  (define (g list a k m)
    (if (empty? list) (max k m) (if (= (remainder a 2) (remainder (car list) 2) ) (g (cdr list) (car list) (+ k 1) m) (g (cdr list) (car list) 1 (max k m) ) ) ) )
  (g (cdr list) (car list) 1 1) )