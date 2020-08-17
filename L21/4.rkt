#lang scheme


(define (f G)
 (define n (length G) )
 (define (DFS a G)
    (define (iter component stack )
          (if (empty? stack) component
              (let* ((pos (car stack))
                     (next (foldl (lambda (x y)
                                    (if y y (if (element-of-set? x component) #f x) ) )
                                  #f (list-ref G pos))) )
                (if next (iter (adjoin-set next component)  (cons next stack) )
                                     (iter component (cdr stack))))))
  (iter (list a) (list a)))
  (define (g i visited components)
    (if (= i n) components
        (if (element-of-set? i visited)
            (g (+ i 1) visited components)
            (let ((c (DFS i G)))
            (g (+ i 1) (union-set c visited) (cons c components) ) ) ) ) )
  (g 0 null null) )

(define (element-of-set? x set)
  (cond  ((null? set) false)
         ((= x (car set)) true)
         ((< x (car set)) false)
         (else (element-of-set? x (cdr set)))))
(define (adjoin-set x set)
  (cond ((element-of-set? x set) set)
        ((empty? set) (list x) )
        ((< x (car set) ) (cons x set) )
        (else (cons (car set) (adjoin-set x (cdr set) ) ) ) ) )
(define (union-set set1 set2)
        (if (empty? set1) set2
        (if (empty? set2) set1
            (let ((x1 (car set1) ) (x2 (car set2)) )
                 (cond ((equal? x1 x2) (cons x1 (union-set (cdr set1) (cdr set2) ) ) )
                       ((< x1 x2) (cons x1 (union-set (cdr set1) set2) ) )
                       ((< x2 x1) (cons x2 (union-set set1 (cdr set2) ) ) ) ) ) ) ) )