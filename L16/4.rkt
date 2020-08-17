#lang scheme
(define (high tree)
  (if (equal? 'leaf (car tree) ) 1
      (+ 1 (max (high (car tree) ) (high (cdr tree) ) ) ) ) )
(define (f tree)
  (define k (high tree) )
  (define (g k tree)
    (if (equal? 'leaf (car tree) ) (list (cons (cdr tree) (expt 2 (- k 1) ) ) )
     (append (g (- k 1) (car tree) ) (g (- k 1) (cdr tree) ) ) ) )
  (g k tree) )
            

