#lang scheme

(define (f input)
  (define n+M (read-M input) )
  (number-of-isolated-vertices (car n+M) (cdr n+M) ) )

(define (number-of-isolated-vertices n M)
  (define Mt (apply map list M) )
  (define (zero_list? l1 l2 i j)
    (if (empty? l1) #t
     (if (= j i) (zero_list? (cdr l1) (cdr l2) i (+ j 1) )
         (and (= 0 (car l1) ) (= 0 (car l2) ) (zero_list? (cdr l1) (cdr l2) i (+ j 1) ) ) ) ) )
  (foldl (λ(x y i res)(+ res (if (zero_list? x y i 0) 1 0) ) ) 0 M Mt (build-list n +) ) )

(define (read-M input)
  (define in (open-input-file input)) (read-char in)
  (define (next lst n)
    (define x (read in) )
    (if (equal? x eof) (cons n lst)(next (cons x lst) (+ n 1) ) ) )
  (define t (next null 0) ) (define n (sqrt (car t) ) )
  (define (sep-lst n lst M)
    (if (empty? lst) M
        (let ((p (l-n lst n) ) )(sep-lst n (cdr p) (cons (car p) M) ) ) ) )
  (define (l-n lst n)
    (define (g lst i p)
      (if (= i 0) (cons p lst) (g (cdr lst) (- i 1) (cons (car lst) p) ) ) )
    (g lst n null) )
  (cons n (sep-lst n (cdr t) null) ) )
