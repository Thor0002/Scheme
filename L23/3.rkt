#lang scheme

(define (f lst1 n)
  (define lsts (sort (cons n lst1) <) )     ;(displayln lst)
  (define (g lst)
    (if (empty? (cdr lst) ) null
        (cons (cons (car lst) (map (λ(x)(cons x (gcd x (car lst) ) ) ) (cdr lst) ) ) (g (cdr lst) ) ) ) )
  (define gcds (g lsts) )
  (define (find-gcd gcds i j)
    (define (h igcd)
      (if (= (caar igcd) j) (cdar igcd) (h (cdr igcd) ) ) )
    (if (= i (caar gcds) ) (h (cdar gcds) ) (find-gcd (cdr gcds) i j) ) )
  
  (define (h n lst)
    (define (continuation n lst)
      (filter (λ(x)(> (find-gcd gcds (min x n) (max x n) ) 1) ) lst) )
    (define cont (continuation n lst) ) 
    (if (empty? cont) (cons 1 (list n) )
    (let ((x (foldl (λ(x answ)
                      (let ((answ? (h x (remove x lst) ) ) )
                      (if (> (car answ?) (car answ) ) answ? answ) ) )
                     (h (car cont) (remove (car cont) lst) ) (cdr cont) ) ) )
           (cons (+ 1 (car x) ) (cons n (cdr x) ) ) ) ) )
  (cdr (h n lst1) ) )


















           
(define (f1 lst1 n)
  (define lst (sort (cons n lst1) <) )     ;(displayln lst)
  (define (g lst)
    (if (empty? (cdr lst) ) null
        (cons (cons (car lst) (map (λ(x)(cons x (gcd x (car lst) ) ) ) (cdr lst) ) ) (g (cdr lst) ) ) ) )
  (define gcds (g lst) )
  (define (find-gcd gcds i j)
    (define (h igcd)
      (if (= (caar igcd) j) (cdar igcd) (h (cdr igcd) ) ) )
    (if (= i (caar gcds) ) (h (cdar gcds) ) (find-gcd (cdr gcds) i j) ) )
  (define subsets (sort (sub-sets lst1) (λ(x y)(> (length x) (length y) ) ) ) )
  (define (h subsets)
    (define (answer? p)
      (if (empty? (cdr p) ) #t
          (and (> (find-gcd gcds (min (car p) (cadr p) ) (max (car p) (cadr p) ) ) 1)
               (answer? (cdr p) ) ) ) )
    (define permutations (per (car subsets) ) )
    (or (ormap (λ(x)(if (answer? (cons n x) ) (cons n x) #f) ) permutations)
        (h (cdr subsets) ) ) )
  (h subsets) )
  

  (define (sub-sets lst)
    (define (sub lst res)
      (if (empty? lst) (list res)
          (append
           (sub (cdr lst) res)
           (sub (cdr lst) (cons (car lst) res) ) ) ) )
    (sub lst null) )

  (define (per lst)
    (if (empty? lst) (list lst)
    (if (empty? (cdr lst) ) (list lst)
        (apply append (map (λ(x)(map (λ(y)(cons x y) )
                                     (per (remove x lst) ) ) )
                           (remove-duplicates lst) ) ) ) ) )
  