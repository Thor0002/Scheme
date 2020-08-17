#lang scheme
(define (f k . a)
  (define b (apply sm
        (map (λ(y)(map (λ(x)(- (char->integer x) 48) ) (string->list (number->string y) ) ) ) a) ) )
  (define (h b s res)
    (if (empty? (cdr b) )
        (if (< (+ s (car b) ) k) (cons (+ s (car b) ) res)
        (h (list 0) (// (+ s (car b) ) k) (cons (% (+ s (car b) ) k) res ) ) )
        (h (cdr b) (// (+ (car b) s) k) (cons (% (+ (car b) s) k) res) ) ) )
   (foldl (λ(x res)(+ x (* 10 res) ) ) 0 (h (reverse b) 0 null) ) )

(define % remainder)
(define // quotient)
(define (sm a . l)
  (define (s2 a b)
    (define (norm a b)
      (let ((l1 (length a)) (l2 (length b) ))
        (if (= l1 l2) (cons a b)
            (if (> l1 l2)
                (norm a (cons 0 b) )
                (norm (cons 0 a) b ) ) ) ) )
    (define c (norm a b) )
    (map + (car c) (cdr c) ) )
  (foldl (λ(x res)(s2 x res)) a l) )
;(define (h b s res)
;   (if (empty? b) (if (= s 0) res (cons s res) )
;        (h (cdr b) (// (+ (car b) s) k) (cons (% (+ (car b) s) k) res) ) ) )
;  (define (t l)
;    (if (< (car l) k) l
;    (t (cons (// (car l) k) (cons (% (car l) k) (cdr l) ) ) ) ) )
;  (foldl (λ(x res)(+ x (* 10 res) ) ) 0 (t (h (reverse b) 0 null) ) )"
;(define (n->l n)
;    (define (g n)
;    (if (= 0 n) null
;        (cons (% n 10) (g (quotient n 10) ) ) ) )
;    (reverse (g n) ) )
  