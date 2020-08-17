#lang scheme

(define (insertion-sort lst)
  (define (insert x l p)
    (if (empty? l) (list x)
        (if (p x (car l) ) (cons x l)
            (cons (car l) (insert x (cdr l) p) ) ) ) )
  (define (sum>? a b)
    (define (sum a)
      (if (< (abs a) 10) (abs a)
          (+ (abs (remainder a 10) ) (sum (quotient a 10) ) ) ) )
    (> (sum a) (sum b) ) )
  (if (empty? lst) null
      (insert (car lst) (insertion-sort (cdr lst) ) sum>? ) ) )
  