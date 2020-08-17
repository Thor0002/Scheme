#lang scheme

(define (empty-queue? q) (and (empty? (car q)) (empty? (cdr q))))

(define (insert-queue q x)
 (cons (car q) (cons x (cdr q))))

(define (delete-queue q)
 (if (empty? (car q))
 (cons (cdr (reverse (cdr q))) '())
 (cons (cdar q) (cdr q))))

(define (front-queue q)
 (if (empty-queue? q)
 (error "FRONT вызвана с пустой очередью" q)
 (if (empty? (car q))
 (car (reverse (cdr q)))
 (caar q))))

(define (ai:=ai+d vec i d)
  (vector-set! vec i (+ d (vector-ref vec i) ) ) )

(define (f list-adj)
 (define n (length list-adj) )
 (define vector-adj (list->vector list-adj) )
 (define (for1 vector-adj v DegIn)
   (define (g listv)
     (if (empty? listv)  DegIn
          (begin (ai:=ai+d DegIn (car listv) 1)
                 (g (cdr listv) ) ) ) )
   (if (= v n) DegIn
       (begin (g (vector-ref vector-adj v) ) (for1  vector-adj (+ v 1) DegIn) ) ) )

  (define (for2 vector-adj v Q number DegIn Index)
   ;(displayln "") (display v) (display " ") (display number) (display " ") (display DegIn) (display " ") (displayln Index)
    
   (define (while vector-adj Q number DegIn Index)
     ;(display Q) (display " ") (display number) (display " ") (display DegIn) (display " ") (displayln Index)
     
     (define (g listv Q\x DegIn)
      ; (display listv) (display " ") (displayln Q\x) (display " ") (display DegIn) 
       (if (empty? listv) Q\x
           (begin (ai:=ai+d DegIn (car listv) -1)
           (g (cdr listv)
              (if (equal? 0 (vector-ref DegIn (car listv) ) )
                  (insert-queue Q\x (car listv) ) Q\x)
              (if (equal? 0 (vector-ref DegIn (car listv) ) )
                  (begin (vector-set! DegIn (car listv) #f) DegIn) DegIn) ) ) ) )
     (if (empty-queue? Q) number
         (let*((x (front-queue Q) )
               (Q\x (delete-queue Q) )
               (new-Q (g (vector-ref vector-adj x)  Q\x DegIn) ) )            
              (vector-set! Index x number)
              (while vector-adj new-Q (+ number 1) DegIn Index) ) ) )
   (if (= v n) Index
    (for2 vector-adj (+ v 1)(cons null null)
          (while vector-adj
                (if (equal? 0 (vector-ref DegIn v) )
                    (insert-queue Q v) (cons null null) )
                      number
                      (if (equal? 0 (vector-ref DegIn v) )
                          (begin (vector-set! DegIn v #f) DegIn) DegIn) Index )
               DegIn Index) ) ) 
  
  (define DegIn (make-vector n 0) )
  (for1 vector-adj 0 DegIn)
  (for2 vector-adj 0 (cons null null) 0 DegIn (make-vector n 0) ) )
