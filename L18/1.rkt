#lang scheme

(define (quick-sort lst p)
  (define (split x lst l1 l2)
    (if (empty? lst) (cons l1 l2)
        (if (p (car lst) x)
            (split x (cdr lst) (cons (car lst) l1) l2)
            (split x (cdr lst) l1 (cons (car lst) l2) ) ) ) )
  (if (empty? lst) lst
      (let ((l (split (car lst) (cdr lst) null null) ) )
        (append (quick-sort (car l) p)
                (cons (car lst) (quick-sort (cdr l) p) ) ) ) ) )
