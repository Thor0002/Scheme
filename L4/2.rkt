#lang scheme
(define (f a)
    (if (empty? (cdr a)) (= 0 (remainder (car a) 2) ) (and (= 0 (remainder (car a) 2) ) (f (cdr a)) ) ) )
  
    
