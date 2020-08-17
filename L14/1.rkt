#lang scheme
(define % remainder)
(define (f a n)
  (define l3 (foldl
     (λ(x res)(if (= (% x 3) 0) (cons (cons x (car res) ) (cdr res) )
              (if (= (% x 3) 1) (cons (car res) (cons (cons x (cadr res) ) (cddr res) ) )
                                (cons (car res) (cons (cadr res) (cons (cons x (caddr res) ) null ) ) ) ) ) )
     (list null null null) a) )
  (define (g i a b c res)
    (if (> i n) res
        (cond ( (and (empty? (cdr a) ) (empty? (cdr b) ) (empty? (cdr c) ) )
                (g (+ i 1) a b c (cons (list (car a) (car b) (car c) ) res) ) )
              ( (empty? b)
                (g i (cdr a) (cadr l3) c res ) )
              ( (empty? c)
                (g i a (cdr b) (caddr l3) res) )
              (else (g (+ i 1) a b (cdr c) (cons (list (car a) (car b) (car c) ) res) ) ) ) ) )
  (if (or (empty? (car l3) ) (empty? (cadr l3) ) (empty? (caddr l3) ) ) #f
      (g 1 (car l3) (cadr l3) (caddr l3) null) ) )



;(define (f a n)
;  (define l3 (foldl
;     (λ(x res)(if (= (% x 3) 0) (cons (cons x (car res) ) (cdr res) )
;              (if (= (% x 3) 1) (cons (car res) (cons (cons x (cadr res) ) (cddr res) ) )
;                                (cons (car res) (cons (cadr res) (cons (cons x (caddr res) ) null ) ) ) ) ) )
;     (list null null null) a) )
;  (define (g i a b c res)
;    (if (> i n) res
;        (if (empty? (cdr a) )
;            (if (empty? (cdr b) )
;                (if (empty? (cdr c) ) (g (+ i 1) a b c (cons (list (car a) (car b) (car c) ) res) )    ; a b c
;                                      (g (+ i 1) a b (cdr c) (cons (list (car a) (car b) (car c) ) res) ) )
;                (g (+ i 1) a (cdr b) c (cons (list (car a) (car b) (car c) ) res) ) )
;            (g (+ i 1) (cdr a) b c (cons (list (car a) (car b) (car c) ) res) ) ) ) )
;  (if (or (empty? (car l3) ) (empty? (cadr l3) ) (empty? (caddr l3) ) ) #f
;      (g 1 (car l3) (cadr l3) (caddr l3) null) ) )
