#lang scheme

(define (min+max lst)
    (if (empty? (cdr lst) ) (cons (car lst) (car lst) )
        (let ((last (min+max (cdr lst) ) ) )
          (cons (min (car lst) (car last) )
                (max (car lst) (cdr last) ) ) ) ) )

(define (f M)
  (define (g p)
    (if (empty? (cdr p) ) (list (list (caar p) ) (list (cdar p) ) )
        (let ((for_n-1 (g (cdr p) ) ) )
        (append (map (λ(x)(cons (caar p) x) ) for_n-1)
                (map (λ(x)(cons (cdar p) x) ) for_n-1) ) ) ) )
  (g  (map min+max (apply map list M) ) ) )


  
