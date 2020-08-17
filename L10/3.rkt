#lang scheme
(define (split str delim)
  (define a  (string->list str)  )
  (define lp (string->list delim) )
  (define t (foldl (λ(x res)(let ((p (member x lp) ))
                   (if p 
                       (if (not (car res) )
                       (list p null (cons (list->string (reverse (cadr res) ) ) (caddr res))) 
                       (list p null (caddr res) ) )
                       (list p (cons x (cadr res)) (caddr res) ) ) ) )
                       (list #t null null) a) )
  (reverse (if (car t)  (caddr t) (cons (list->string (reverse (cadr t) ) ) (caddr t)) ) ) )