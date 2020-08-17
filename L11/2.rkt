#lang scheme
(define (f a)
  (define (g s a i F1 F2)
    (if (empty? a) s
        (if (= i F1)
            (g (+ s (car a) ) (cdr a) (+ i 1) F2 (+ F1 F2) )
            (g s (cdr a) (+ i 1) F1 F2) ) ) )
  (g (car a) (cdr a) 1 1 2) )
    
