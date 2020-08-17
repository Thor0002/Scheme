#lang scheme
(define (f s sp)
  (define a  (string->list s)  )
  (define lp (string->list sp) ) 
  (define t (foldl (λ(x res)
          (let ((t (member x lp)))
            (if (and t (not (cdr res) ) )
                       (cons (+ 1 (car res) ) t) (cons (car res) t) ) ) ) (cons 0 #t) a) )
(if (cdr t) (car t) (+ 1 (car t) ) ) )




;(define (f1 s sp)
; (if (equal? "" s) 0 (+ 