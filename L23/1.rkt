#lang scheme

(define (f lst1)
  (define lst (sort lst1 <) )
  (define (g lst)
    (if (empty? (cdr lst) ) null
        (cons (cons (car lst) (map (λ(x)(cons x (gcd x (car lst) ) ) ) (cdr lst) ) ) (g (cdr lst) ) ) ) )
  (define gcds (g lst ) )
  (define (find-gcd gcds i j)
    (define (h igcd)
       (if (= (caar igcd) j) (cdar igcd) (h (cdr igcd) ) ) )
    (if (= i (caar gcds) ) (h (cdar gcds) ) (find-gcd (cdr gcds) i j) ) )
  
  (define (per lst per-now snow per-best sbest)
    (if (empty? lst)
        (if (> snow sbest)
            (cons snow per-now)
            (cons sbest per-best) )
       (foldl (λ(x sbest+per-best)
              (let ((per-full (per (remove x lst) (cons x per-now)
                                   (if (empty? per-now) 0 (+ (find-gcd gcds (min x (car per-now) ) (max x (car per-now) ) ) snow) )
                                   (cdr sbest+per-best) (car sbest+per-best)  ) ) )
                (if (> (car per-full) (car sbest+per-best)) per-full sbest+per-best)) )
              (cons sbest per-best) lst) ) )
  (cdr (per lst null 0 null 0) ) )
  


(define (per lst)
    (if (empty? (cdr lst) ) (list lst)
        (apply append (map (λ(x)(map (λ(y)(cons x y) )
                                     (per (remove x lst) ) ) )
                           (remove-duplicates lst) ) ) ) )

(define (f1 lst1)
  (define lst (sort lst1 <) )
  (define (g lst)
    (if (empty? (cdr lst) ) null
        (cons (cons (car lst) (map (λ(x)(cons x (gcd x (car lst) ) ) ) (cdr lst) ) ) (g (cdr lst) ) ) ) )
  (define gcds (g lst ) )
  (define (find-gcd gcds i j)
    (define (h igcd)
       (if (= (caar igcd) j) (cdar igcd) (h (cdr igcd) ) ) )
    (if (= i (caar gcds) ) (h (cdar gcds) ) (find-gcd (cdr gcds) i j) ) )
  
  (define permutations (per (reverse lst) ) )
  (define (iter permutations maxs resp)
    (define (sum-dev p)
      (if (empty? (cdr p) ) 0
          (+ (find-gcd gcds (min (car p) (cadr p) ) (max (car p) (cadr p) ) ) (sum-dev (cdr p) ) ) ) )
    (if (empty? permutations) resp
        (let ((s (sum-dev (car permutations) ) ) )
          (if (> s maxs)
              (iter (cdr permutations) s (car permutations) )
              (iter (cdr permutations) maxs resp) ) ) ) )
  (iter permutations 0 0) )
