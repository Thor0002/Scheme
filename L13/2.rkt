#lang scheme
(define (f a b . l)
  (define (s2 a b)
    (define (norm a b)
      (let ((l1 (length a)) (l2 (length b) ))
        (if (= l1 l2) (cons a b)
            (if (> l1 l2)
                (norm a (cons 0 b) )
                (norm (cons 0 a) b ) ) ) ) )
    (define c (norm a b) )
    (map + (car c) (cdr c) ) )
  (foldl (λ(x res)(s2 x res)) a (cons b l) ) )