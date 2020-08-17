#lang scheme
(define (f a)
  (define (g l a)
    (if (empty? (cddr a) )
        l
        (if (or (= (* (car a) (car a) ) (* (cadr a) (caddr a) ) ) (= (* (cadr a) (cadr a) ) (* (car a) (caddr a) ) ) (= (* (caddr a) (caddr a) ) (* (cadr a) (car a) ) ) )
            (g (cons (list (car a) (cadr a) (caddr a) ) l) (cdr a) )
            (g l (cdr a) ) ) ) )
  (if (< (length a) 3) null (g null a) ) )
            
        
    
