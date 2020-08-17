#lang scheme
(define (f s)
  (define lw (sort (string-split s " ") string<?) )  ;member (car t) lp     
  (define d (cdr (foldl (λ(z res)
                (if (equal? (car res) z) (cons z  z) (cons z (cdr res) ) ) )
              (cons (car lw) #f) (cdr lw) ) ) )
  (if d (apply string (reverse (string->list d))) d))



(define (f1 s)
  (define a  (string->list s)  )                                                                                  ;(define lp (string->list sp) )
  (define t (foldl (λ(x res)
                   (if (equal? x #\space)                                                                         ;(if (member x lp) 
                       (if (not (equal? (car res) #\space) )                                                      ;(if (not (member (car res) lp) )
                       (list x null (cons (list->string (cadr res) ) (caddr res)))
                       (list x null (caddr res) ) )
                       (list x (cons x (cadr res)) (caddr res) ) ) )
                       (list #\space null null) a) )
  (define lw (sort (if (equal? (car t) #\space) (caddr t) (cons (list->string (cadr t) ) (caddr t)) ) string<?) )  ;member (car t) lp     
  (define d (cdr (foldl (λ(z res)
                (if (equal? (car res) z) (cons z  z) (cons z (cdr res) ) ) )
              (cons (car lw) #f) (cdr lw) ) ) )
  (if d (apply string (reverse (string->list d))) d))