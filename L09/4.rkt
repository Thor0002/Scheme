#lang scheme
(define (f a)
  (define l (length a) )
  (define li (build-list l values) )
  (define ta (apply map list a) )

  (define b (foldl (λ(x y i res)(cons
                    (cons (apply + x) (cons (apply + y) (car res) ) )
                    (foldl (λ(j c r)(cons (if (= i j) (+ c (car r) ) (car r) ) (if (= j (- (- l 1) i ) ) (+ c (cdr r) ) (cdr r) ) ) ) (cdr res) li x ) ) )
         (cons null (cons 0 0) ) a ta li ) )

  (define ls (cons (cadr b) (cons (cddr b) (car b) ) ) )
  (define s (car ls) )
  (andmap (λ(x)(= x s) ) ls) )



(define (f1 a)
  (define l (length a) )
  (define li (build-list l values) )
  (define ta (apply map list a) )
  (define t (andmap (λ(l1)(andmap (λ(l2)(= (apply + l1) (apply + l2) ) ) ta ) ) a) )
  (define s (foldl (λ(x i r1)(foldl (λ(y j r2)
             (cons
             (if (= i j) (+ y (car r2) ) (car r2) )
             (if (= j (- (- l 1) i) ) (+ y (cdr r2) ) (cdr r2) ) ) ) r1 x li) ) (cons 0 0) a li) )
  (and t (= (apply + (car a) ) (car s) ) (= (cdr s) (car s) ) )
  )
