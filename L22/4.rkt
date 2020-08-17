#lang scheme

(define (f G)
  (define n (length G) )
  (define vG (list->vector (map (λ(listv)(sort listv <) ) G) ) )
  
  (define (find-root i G)
    (if (empty? G) #f
        (if (empty? (car G) ) (find-root (+ i 1) (cdr G) )
        (if (empty? (cdar G) ) i (find-root (+ i 1) (cdr G) ) ) ) ) )
  
  (define (build-tree v set-of-v lastv)
    (define list*v (remove lastv (vector-ref vG v)) )
    (define l (length list*v) )
    (if (empty? (intersection-set set-of-v list*v) )
        (if (= l 0) (cons set-of-v (list v null null) )
        (if (= l 1)
            (let ((x (build-tree (car list*v) (adjoin-set (car list*v) set-of-v) v) ) )
            (if x (cons (car x) (list v (cdr x) null) ) #f) )
        (if (= l 2)
            (if (= (car list*v) (cadr list*v) ) #f
            (let  ((x (build-tree (car list*v) (adjoin-set (car list*v) set-of-v) v) ))
            (if x 
            (let  ((y (build-tree (cadr list*v) (adjoin-set (cadr list*v) (car x) ) v) ) )
                   (if y (cons (car y) (list v (cdr x) (cdr y) ) ) #f) ) #f) ) )
             #f) ) )
             #f) )
  (define tree
      (let ((root (find-root 0 G) ) ) ;(displayln root)
      (if root (let ((child1 (car (vector-ref vG root) ) ))
               (if (= root child1) #f
               (let ((left (build-tree child1 (list (min child1 root) (max child1 root) ) root) ) )
                     (if left (list root (cdr left) null) #f) ) ) )
               #f) ) )
 ;(displayln tree)
 (if (= n 1) (if (empty? (car G) ) (list 0 null null) #f)
             (if tree (if (= n (number-of-tree-vertices tree) ) tree #f) #f) ) )


(define (adjoin-set x set)
  (cond ((element-of-set? x set) set)
        ((empty? set) (list x) )
        ((< x (car set) ) (cons x set) )
        (else (cons (car set) (adjoin-set x (cdr set) ) ) ) ) )
(define (intersection-set set1 set2)
  (if (or (null? set1) (null? set2)) '()
      (let ((x1 (car set1)) (x2 (car set2)))
        (cond ((equal? x1 x2) (cons x1 (intersection-set (cdr set1) (cdr set2))))
              ((< x1 x2) (intersection-set (cdr set1) set2))
              ((< x2 x1) (intersection-set set1 (cdr set2)))))))
(define (element-of-set? x set)
  (cond  ((null? set) false)
         ((= x (car set)) true)
         ((< x (car set)) false)
         (else (element-of-set? x (cdr set)))))
  
(define (number-of-tree-vertices tree)
 (define data car)
 (define left cadr)
 (define right caddr)
 (define empty-tree? empty?)
 (if (empty-tree? tree) 0
     (if (empty-tree? (right tree) ) (+ 1 (number-of-tree-vertices (left  tree) ) )
     (if (empty-tree? (left tree) )  (+ 1 (number-of-tree-vertices (right tree) ) )
          (+ 1 (number-of-tree-vertices (left tree) ) (number-of-tree-vertices (right tree) ) ) ) ) ) )
#|
(define (f list_adj)
  (list_edge->tree (list_adj->list_edge list_adj) ) )

(define (list_adj->list_edge list_adj)
  (define (pair<? p1 p2)
  (or (< (min (car p1) (cdr p1) ) (min (car p2) (cdr p2) ) )
      (and (= (min (car p1) (cdr p1) ) (min (car p2) (cdr p2) ) )
           (< (max (car p1) (cdr p1) ) (min (car p2) (cdr p2) ) ) ) ) )
;  (define (delete lst)
;   (define (g lst even?)
;    (if (empty? lst) lst
;        (if even? (g (cdr lst) #f) (cons (car lst) (g (cdr lst) #t ) ) ) ) )
;  (g lst #t) )
 (define (g list_edge list_adj i)
   (if (empty? list_adj) list_edge
       (g (append list_edge (map (λ(x)(cons i x) ) (car list_adj) ) )
          (cdr list_adj) (+ i 1) ) ) )
(remove-duplicates (sort (g null list_adj 0 ) pair<?) ) )

(define (list_edge->tree list_edge)
  (define (find_root root  list_edge)
    (if (empty? list_edge) root
        (if (equal? (cdar list_edge) root)
            (find_root (caar list_edge) (cdr list_edge) )
            (find_root root (cdr list_edge) ) ) ) )
  
  (define (find_child pair_child vertex list_edge new_list safelist)
   (if (empty? list_edge) (cons new_list pair_child)
      (if (and (equal? (caar list_edge) vertex) (equal? (cdar list_edge) vertex) )
          (cons (append (reverse (cdr list_edge) ) safelist) vertex)
          (if  (cdr pair_child)
               (find_child pair_child vertex (cdr list_edge) (cons (car list_edge) new_list) (cons (car list_edge) safelist) )
               ;(cons (append (reverse list_edge) new_list) pair_child)
               (if (equal? (caar list_edge) vertex)
                   (find_child (if (car pair_child) (cons (car pair_child) (cdar list_edge) )
                                   (cons  (cdar list_edge) #f ) )
                               vertex (cdr list_edge) new_list (cons (car list_edge) safelist) )
                   (find_child pair_child vertex (cdr list_edge) (cons (car list_edge) new_list) (cons (car list_edge) safelist) ) ) ) ) ) )
  
  (define x (find_root (caar list_edge) (cdr list_edge) ) )
  (define (g root list_edge)
    (let ((y (find_root root list_edge) ) )
      (if (equal? root y) root (g y list_edge) ) ) )

  (define (tree_from_top vertex list_edge)
   (let* ((res (find_child (cons #f #f) vertex list_edge null null) )
         (new_list (car res) )
         (p (cdr res) ) )
    (if (pair? p)
        (if (car p)
            (if (cdr p)
                (let* ((l (tree_from_top (car p) new_list) ) (r (tree_from_top (cdr p) (car l) ) ) )
                  (cons (car r) (list vertex (cdr l) (cdr r) ) ) )
                (let ((l (tree_from_top (car p) new_list) ) )
                  (cons (car l) (list vertex (cdr l) null) ) ) )
            (cons new_list (list vertex null null) ) )
        (let ((l (tree_from_top vertex new_list) ) )
          (cons (car l) (list vertex (cdr l) null) ) ) ) ) )
  (cdr (tree_from_top (g x list_edge) list_edge) ) )
|#