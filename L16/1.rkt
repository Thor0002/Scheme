#lang scheme

(define (left tree)
  (if (empty? tree) null (cadr tree) ) )
(define (right tree)
  (if (empty? tree) null (caddr tree) ) )
(define (data tree)
 (if (empty? tree) #f (car tree) ) )

(define (f tab)
  (define (g x tree)
    (if (empty? x) (if (data tree) #f (list #t null null) )
        (if (data tree) #f
            (if (= 1 (car x) )
                (let ((r (g (cdr x) (right tree) ) ))
                  (if r (list #f (left tree) r ) #f ) )
                (let ((l (g (cdr x) (left tree) ) ) )
                  (if l (list #f l (right tree) ) #f ) ) ) ) ) )
  (print (foldl (λ(x tree)(if tree (g x tree) #f) ) null (map cdr tab) ) )
  (if (foldl (λ(x tree)(if tree (g x tree) #f) ) null (map cdr tab) )
      #t
      (if (foldl (λ(x tree)(if tree (g x tree) #f) ) null (map (λ(x)(reverse (cdr x) ) ) tab) ) #t #f) ) )

(define (f1 tab)
  (define (g tab)
    (if (empty? (cdr tab) ) #t
        (if (string-prefix? (cadr tab) (car tab) ) #f (g (cdr tab) ) ) ) )
  (or (g (sort (map (λ(x)(list->string (map (λ(t)(integer->char (+ t 48) ) ) (cdr x) ) ) ) tab) string<?) )
       (g (sort (map (λ(x)(list->string (reverse (map (λ(t)(integer->char (+ t 48) ) ) (cdr x) ) ) ) ) tab) string<?) ) ) )
