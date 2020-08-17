#lang scheme

(define (f M)
 (define (loop x i j)
   (if (= i j) (* 2 x) x) )
 (define (pair< p1 p2)
  (if (<= (car p1) (car p2) )
      (<= (cdr p1) (cdr p2) )
      #f) )
(define (l->l_e lst i j)
  (if (empty? lst) lst
      (if (= 0 (car lst) )
          (l->l_e (cdr lst) i (+ j 1) )
          (append (make-list (loop (car lst) i j) (cons (min i j) (max i j ) )  )
                  (l->l_e (cdr lst) i (+ j 1) ) ) ) ) )
 (define (delete lst)
  (define (g lst even?)
    (if (empty? lst) lst
        (if even? (g (cdr lst) #f)
            (cons (car lst) (g (cdr lst) #t ) ) ) ) )
  (g lst #t) )
 (define (g list_edge M i)
   (if (empty? M) list_edge
       (g (append list_edge (l->l_e (car M) i 0) ) (cdr M) (+ i 1) ) ) )
(define x (g null M 0 ) )
(delete (sort x pair<) ) )
