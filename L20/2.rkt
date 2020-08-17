#lang scheme

(define (f input)
  (define n+list_adj (read-list-adj input) )
  (complete_graph? (car n+list_adj) (cdr n+list_adj) ) )

(define (complete_graph? n list_adj)
  (if (empty? list_adj) #t
  (and (= (- n 1) (car list_adj) ) (complete_graph? n (cdr list_adj)) ) ) )

(define (read-list-adj input)
  (define in (open-input-file input))
  (read-char in)
  (define (read-vertex)
    (define k (read in))
    (foldl (λ(i lst)
             (read in))
           null (build-list k values)) k)
  (define n (read in))
  (cons n (reverse
   (foldl (λ(i lst)
            (if (= i n) lst (cons (read-vertex) lst)))
          null (build-list n values)))) )

;(define (full_list? i n-1 lst)
;      (if (empty? lst) (if (= i 0) (if (= n-1 0) #t #f)
;                       (if (= n-1 -1) #t #f) )
;          (if (= i n-1) (full_list? i (- n-1 1) lst)
;              (and (= (car lst) n-1) (full_list? i (- n-1 1) (cdr lst) ) ) ) ) );
;
;(define (сomplete graph n lst_adj)
; (andmap (λ(x i)(full_list? i (- n 1) (sort x >) ) ) (map cdr lst_adj) (build-list n +) ) )
