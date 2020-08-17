#lang scheme

(define (f input output)
  (define out (open-output-file output #:exists 'replace))
  (define n+list_adj (read-list-adj input) )
  (printlist (list_adj->list_edge (car n+list_adj) (cdr n+list_adj) ) out)
  (close-output-port out) )

(define (list_adj->list_edge n list_adj)
  (define (pair< p1 p2)
  (if (<= (car p1) (car p2) )  (<= (cdr p1) (cdr p2) ) #f) )
  (define (delete lst)
  (define (g lst even?)
    (if (empty? lst) lst
        (if even? (g (cdr lst) #f) (cons (car lst) (g (cdr lst) #t ) ) ) ) )
  (g lst #t) )
 (define (g list_edge list_adj i)
   (if (empty? list_adj) list_edge
       (g (append list_edge (map (λ(x)(cons (min x i) (max x i) ) ) (car list_adj) ) )
          (cdr list_adj) (+ i 1) ) ) )
(delete (sort (g null list_adj 0 ) pair<) ) )

(define (read-list-adj input)
  (define in (open-input-file input))
  (read-char in)
  (define (loop i x lst)
    (if (= i x) (cons x (cons x lst) ) (cons x lst) ) )
  (define (read-vertex i)
    (define k (read in))
    (foldl (λ(j lst)(if (= j k) lst (loop i (read in) lst)))
           null (build-list k values)))
  (define n (read in))
  (cons n (reverse
   (foldl (λ(i lst)(if (= i n) lst (cons (read-vertex i) lst)))
          null (build-list n values)))))

(define (printlist lst out)
  (define (printpair p)
    (display (car p) out) (display #\space out) (display (cdr p) out) (display #\return out) (display #\newline out) )
  (if (empty? (cdr lst) ) (printpair (car lst) )
      (begin (printpair (car lst) ) (printlist (cdr lst) out ) ) ) )