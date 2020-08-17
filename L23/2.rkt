#lang scheme

(define (f M)
  (define B<M> (cdr (sub-sets M) ) )
  (define (g B<M> maxd X)
    (if (empty? B<M>) ;(begin (display maxd)
                             X
                             ;)
        (let* ((c (find-centre (car B<M>) ) )
               (s (foldl (λ(t s)(+ s (dist t c) ) ) 0 (car B<M>) ) ) )
          (if (> s maxd)
              (g (cdr B<M>) s (car B<M>) )
          (if (= s maxd)
              (g (cdr B<M>) s (cons (car B<M>) X) )
              (g (cdr B<M>) maxd X) ) ) ) ) )
  (g B<M> 0 null) )
                   
(define (sub-sets lst)
  (define (sub lst res)
    (if (empty? lst) (list res)
        (append
         (sub (cdr lst) res)
         (sub (cdr lst) (cons (car lst) res) ) ) ) )
  (sub lst null) )
  
(define (p+ p1 p2)
  (cons (+ (car p1) (car p2) ) (+ (cdr p1) (cdr p2) ) ) )

(define (p* p a)
  (cons (* a (car p) ) (* a (cdr p) ) ) )

(define (find-centre X)
  (define n (length X) )
  (p* (foldl (λ(t c)(p+ t c) ) (cons 0 0) X) (/ 1 n) ) )

(define (vec t1 t2)
  (cons (- (car t2) (car t1)) (- (cdr t2) (cdr t1))))

(define (norm v)
  (sqrt (+ (sqr (car v)) (sqr (cdr v)))))

(define (dist a b)
  (norm (vec a b)))

