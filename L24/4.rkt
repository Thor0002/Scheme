#lang scheme

(define (f M1 M2) 
(define (g M1* M2*)
  (define M1 (map (λ(p)(pair*k p 1.0) ) (cdr M1*) ) )
  (define M2 (map (λ(p)(pair*k p 1.0) ) (cdr M2*) ) )
  (define (make-distM M)
    (if (empty? (cdr M) ) null (cons (dist (car M) (cadr M) ) (make-distM (cdr M) ) ) ) )
  
  (define (find-k end? distM1 distM2 cM1 cM2)
   ; (display distM1) (display " ") (display distM2) (display " ")
   ; (display cM1) (display " ") (display cM2) (displayln "")
    (if (equal? cM2 M2)
        (if end? #f (if (all=? (map / distM1 distM2) )
                        (if (||? (/ (car distM1) (car distM2) ) cM1 cM2) #t
                            (find-k #t distM1 (1->last distM2) cM1 (1->last cM2) ) )
                            (find-k #t distM1 (1->last distM2) cM1 (1->last cM2) ) ) )
        (if (||? (/ (car distM1) (car distM2) ) cM1 cM2) #t
            (find-k #t distM1 (1->last distM2) cM1 (1->last cM2) ) ) ) )
  
  (define (||? k M1 M2)
   ; (display k) (display " ") (display M1) (display " ") (display M2) (displayln "")
    (all=? (map pair- M1 (map (λ(p)(pair*k p k) ) M2) ) ) )
  (if (= (length M1) (length M2) )
      (find-k #f (make-distM M1*)  (make-distM M2*) M1 M2)
      #f) )
  (define M1* (append M1 (list (car M1) ) ) )
  (define M2* (append M2 (list (car M2) ) ) )
  (or (g M1* M2*) (g M1* (reverse M2*) ) ) )

(define (all=? lst)
  (if (empty? (cdr lst) ) #t (and (equal? (car lst) (cadr lst) ) (all=? (cdr lst) ) ) ) )  

(define (1->last lst)(append (cdr lst) (list (car lst) ) ) )

(define (pair- p1 p2) (cons (- (car p1) (car p2)) (- (cdr p1) (cdr p2)) ) )

(define (pair*k p k)
  (cons (* k (0? (car p) ) ) (* k (0? (cdr p) ) ) ) )

(define (vec t1 t2)
  (cons (- (car t2) (car t1)) (- (cdr t2) (cdr t1))))

(define (norm v)
  (* 1.0 (sqrt (+ (sqr (car v)) (sqr (cdr v))))) )

(define (dist a b)
  (norm (vec a b)))

(define (0? n) (if (= n 0) 0.0 n) )