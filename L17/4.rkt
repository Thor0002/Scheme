#lang scheme

(define (f list_edge)
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
  (define root (find_root (caar list_edge) (cdr list_edge) ) )
  
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
  (cdr (tree_from_top root list_edge) ) )



;(define (find_child pair_child vertex list_edge)
;    (if (empty? list_edge) pair_child
;    (if  (cdr pair_child)  pair_child
;        (if (car pair_child)
;            (if (equal? (caar list_edge) vertex)
;                                 (find_child (cons (car pair_child) (cdar list_edge) ) vertex (cdr list_edge) )
;                                 (find_child pair_child vertex (cdr list_edge) ) )
;            (if (equal? (caar list_edge) vertex)
;                                 (find_child (cons  (cdar list_edge) #f ) vertex (cdr list_edge) )
;                                 (find_child pair_child vertex (cdr list_edge) ) ) ) ) ) )
;(define (tree_from_top vertex list_edge)
;    (let* ((res (find_child (cons #f #f) vertex list_edge null) )
;           (new_list (car res) )
;           (p (cdr res) ) )
;      (if (car p)
;          (if (cdr p)
;              (list vertex (tree_from_top (car p) )
;                      (tree_from_top (cdr p) ) )
;              (list vertex (tree_from_top (car p) ) null) )
;          (list vertex null null) ) ) )