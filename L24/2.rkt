#lang scheme

(define (f M)
  (define a (car M) )
  (define (g M T)
    (if (equal? (cadr M) a ) T
        (g (cdr M) (cons (list a (car M) (cadr M) ) T ) ) ) )
  (g (cdr M) null) )
