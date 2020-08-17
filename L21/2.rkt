#lang scheme

(define (f list-edge list-vertex)
  (define (delete-v v list-edge)
    (if (empty? list-edge) null
        (let ((v1 (caar list-edge) )(v2 (cdar list-edge) ) )
        (if (or (= v v1) (= v v2) ) (delete-v v (cdr list-edge) )
            (cons (cons (if (> v1 v) (- v1 1) v1)
                        (if (> v2 v) (- v2 1) v2) )
                  (delete-v v (cdr list-edge) ) ) ) ) ) )
  (define (g list-edge list-vertex)
  (if (empty? list-vertex) list-edge
      (g (delete-v (car list-vertex) list-edge) (map sub1 (cdr list-vertex) ) ) ) )
  (g list-edge (sort list-vertex <) ) )




#|
(define (f1 list-edge list-vertex)
  (define (delete-edge list-edge new-list n)
    (if (empty? list-edge) (cons new-list (+ n 1) )
        (if (or (member (caar list-edge) list-vertex)
                (member (cdar list-edge) list-vertex) )
            (delete-edge (cdr list-edge) new-list (max n (caar list-edge) (cdar list-edge) ) )
            (delete-edge (cdr list-edge) (cons (car list-edge) new-list) (max n (caar list-edge) (cdar list-edge) ) ) ) ) )
  (define x (delete-edge list-edge null 0) )
  (define n (cdr x) )
  (define change-list (car x) )
  (define (n/k listn list-vertex i vector-transit)
    (if (empty? listn) vector-transit
        (if (empty? list-vertex)
            (begin (vector-set! vector-transit (car listn) i)
                   (n/k (cdr listn) list-vertex (+ i 1) vector-transit) )
        (if (< (car listn) (car list-vertex) )
            (begin (vector-set! vector-transit (car listn) i)
                   (n/k (cdr listn) list-vertex (+ i 1) vector-transit) )
             (n/k (cdr listn) (cdr list-vertex) i vector-transit) ) ) ) )
  (define vector-transit (n/k (build-list n +) list-vertex 0 (make-vector n 0) ) )
  (map (λ(edge)(cons (vector-ref vector-transit (car edge) )
                     (vector-ref vector-transit (cdr edge) ) ) )
       change-list) ) |#