#lang scheme

(define (f file-in )
  (define in (open-input-file file-in))
  (define outrec (open-output-file "recursive functions.txt" #:exists 'replace))
  (define outdif (open-output-file "non recursive functions.txt" #:exists 'replace))
  (define (line out) (display #\return out) (display #\newline out) )
  
  (define (except s stack x a)
    (define c (read-char in))
    (if (equal? stack 0) (next 0 x a)
    (cond
      ((= s 0)
       (if (equal? c #\() (except 0 (+ stack 1) x a)
       (if (equal? c #\)) (except 0 (- stack 1) x a)
       (if (equal? c #\") (except 1 stack x a)
       (if (equal? c #\;) (comment stack x a) (except 0 stack x a) ) ) ) ) )
      ((= s 1)
       (if (equal? c #\\)
           (begin (read-char in) (except 1 stack x a) )
           (if (equal? c #\") (except 0 stack x a) (except 1 stack x a) ) ) ) ) ) )
  
  (define (comment stack x a)
    (define c (read-char in) )
    (if (equal? c #\newline) (except 0 stack x a) (comment stack x a) ) )
  
  (define (next s x a)
    (define c (read-char in))
    (if (equal? c eof)
        (reverse a)
        (cond
        ((= 0 s)
          (if (equal? c #\uFEFF)
              (next 0 null a)
          (if (equal? c #\\)
              (next 0 (cons (read-char in) (cons #\\ x) ) a)
          (if (or (equal? c #\() (equal? c #\[) (equal? c #\{) )
              (next 0 null (cons "(" (if (empty? x) a (cons  (list->string (reverse x) ) a) ) ) )
          (if (or (equal? c #\)) (equal? c #\}) (equal? c #\]) )
              (next 0 null (cons ")" (if (empty? x) a (cons  (list->string (reverse x) ) a) ) ) )
          (if (or (equal? c #\space) (equal? c #\return) (equal? c #\newline) )
              (next 0 null (if (empty? x) a (cons  (list->string (reverse x) ) a) ) )
          (if (equal? c #\") (next 1 null (if (empty? x) a (cons  (list->string (reverse x) ) a) ) )
          (if (equal? c #\;) (next 2 null (if (empty? x) a (cons  (list->string (reverse x) ) a) ) )
          (if (equal? c #\')
           (let ((newc (read-char in)))
           (if (equal? newc #\() (except 0 1 x a) (next 0 (cons newc (cons #\' x) ) a) ) )
          (if (or (equal? c #\return)  (equal? c #\newline) ) (next 0 s x) (next 0 (cons c x) a ) ) ) ) ) ) ) ) ) ) )
        ((= s 1)
         (if (equal? c #\\) (begin (read-char in) (next 1 x a) )
         (if (equal? c #\") (next 0 x a) (next 1 x a) ) ) )
        ((= s 2)
         (if (equal? c #\newline) (next 0 x a) (next 2 x a) ) ) ) ) )
  
  (define a (next 0 null null) )
  
  ;(define (findname name lst res t)
   ; (if (empty? lst) (if t (reverse res) #f) (if (equal? name (car lst) ) (findname name (cdr lst) res #t)
    ;                                                                   (findname name (cdr lst) (cons (car lst) res)  t) ) ) )
  (define (findname name lst)
    (if (empty? lst) #f (or (equal? name (car lst) ) (findname name (cdr lst) ) ) ) )

  (define (insert name lst)
    (if (empty? lst) (list name) (if (equal? (car lst) name) lst (cons (car lst) (insert name (cdr lst) ) ) ) ) )

  (define (insert_stack stack lst name_rec)
    (foldl (λ(name res)(if (findname name name_rec) lst  (insert name res) ) ) lst stack) )
   
 (define (add1list lst) (map add1 lst) )
  (define (sub1list lst) (map sub1 lst) )
  
  (define (g s a stack stack_name name_rec name_dif)
    (if (empty? a)
        (if (empty? stack)
            (cons name_rec (insert_stack stack_name name_dif name_rec) )
            (if (empty? stack_name)
                              (cons name_rec name_dif)
                              (cons name_rec (if (findname (car stack_name) name_rec) name_dif (insert (car stack_name) name_dif) ) ) ) )
        (let ((x (car a) ) )
        (cond ((= s 0) (if (equal? x "(")
                           (g 1 (cdr a) null stack_name name_rec name_dif)
                           (g 0 (cdr a) null stack_name name_rec name_dif) ) )
              ((= s 1) (if (equal? x "define")
                           (g 2 (cdr a) stack stack_name name_rec name_dif)
                           (g 0 (cdr a) null stack_name name_rec name_dif) ) )
              ((= s 2) (if (equal? x "(")
                           (g 3 (cdr a) (list 2) stack_name name_rec name_dif)
                           (g 0 (cdr a) null stack_name name_rec name_dif) ) )
              ((= s 3) (g 4 (cdr a) stack (list x) name_rec name_dif) )
              ((= s 4)(if (empty? stack) (g 0 a null null name_rec (insert_stack stack_name name_dif name_rec) )
                      (if (= 0 (car stack) )
                          (if (empty? stack_name)
                              (g 4 a (cdr stack) null name_rec name_dif)
                              (g 4 a (cdr stack) (cdr stack_name) name_rec (if (findname (car stack_name) name_rec) name_dif (insert (car stack_name) name_dif) ) ) )
                      (if (equal? x "(")
                          (g 4 (cdr a) (add1list stack) stack_name name_rec name_dif )
                      (if (equal? x ")")
                          (g 4 (cdr a) (sub1list stack) stack_name name_rec name_dif )
                      (if (equal? x "define")
                          (if (equal? (cadr a) "(")
                              (g 4 (cdddr a) (cons 2 (add1list stack) ) (cons (caddr a) stack_name) name_rec name_dif )
                              (g 4 (cddr a) stack stack_name name_rec name_dif ) )
                          (if (findname x stack_name)
                              (g 4 (cdr a) stack stack_name (insert x name_rec) name_dif )
                              (g 4 (cdr a) stack stack_name name_rec name_dif ) ) ) ) ) ) ) ) ) ) ) )
  (define list_func (g 0 a null null null null) )

  (define (print_out lst out)
    (if (empty? lst) (display "" out)
        (begin (display (car lst) out ) (line out) (print_out (cdr lst) out) ) ) )
  (print_out (car list_func) outrec)
  (print_out (cdr list_func) outdif)
  (close-output-port outrec)
  (close-output-port outdif) )
                                       










                            

