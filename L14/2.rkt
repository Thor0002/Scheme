#lang scheme
(define (f a)
  (define (g a res l1 l2)
    (if (empty? a) (reverse res)
        (if (member #\/ (string->list (car a) ) )
            (g (cdr a) (list (car a) ) (car a) #f)
            (if (empty? res)
                (g (cdr a) res l1 l2)
                (if (equal? (car a) l2)
                    (g (cdr a) (cdr res) l2 (caddr res) )
                    (g (cdr a) (cons (car a) res) (car a) l1) ) ) ) ) )
  (define w (g a null #f #f) )
  (if (empty? w) #f
      (string-append (car w)
                     (substring
                      (apply string-append
                             (map string-append (make-list (- (length w) 1) "/") (cdr w) ) ) 1) ) ) )
                   