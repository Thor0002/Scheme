#lang scheme
(define (f a)
  (define li (build-list (length a) values) )
  (define t
    (foldl
     (λ(x i res)
       (let ((b (foldl (λ(y j s)
                         (if (= i j) (cons y (cdr s) ) (cons (car s) (+ (cdr s) y) ) ) )
                       (cons 0 0) x li ) ) )      
            (cons (and (car res) (>= (car b) (cdr b) ) ) (or (cdr res) (> (car b) (cdr b) ) ) ) ) )
     (cons #t #f)
     a li ) )
  (and (car t) (cdr t) ) )
