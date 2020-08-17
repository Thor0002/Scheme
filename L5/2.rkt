#lang scheme
(define (indexes-of a x)
  (define (g a b i)
    (if (empty? a) b (if (= x (car a) ) (g (cdr a) (cons i b) (+ i 1) ) (g (cdr a) b (+ i 1) ) ) ) )
   (reverse (g a null 0) ) )     
    
(define (f a)
  (define s (/ (foldl + 0 a) (length a) ) )
  (define (g a m)
    (if (empty? a) m (if (< (abs (- s (car a) ) ) (abs (- s m ) ) ) (g (cdr a) (car a) ) (if (= (abs (- s (car a) ) ) (abs (- s m ) ) ) (g (cdr a) (min m (car a) ) ) (g (cdr a) m) ) ) ) )
 (define x (g (cdr a) (car a) ) )
  (cons x (indexes-of a x) ) )
