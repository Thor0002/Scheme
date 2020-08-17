#lang scheme

(define (printlist lst)(if (empty? (cdr lst) ) (displayln (car lst) )  (begin (displayln (car lst) ) (printlist (cdr lst) ) ) ) )

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
  (define (insert name stack_call)
    (if (empty? stack_call) (list (list name) ) (map (λ(call)(cons name call) ) stack_call) ) )
   
  (define (add1list lst) (map add1 lst) )
  (define (sub1list lst) (map sub1 lst) )
  
  (define (g s a stack stack_name stack_call list_call)
    (print s) (display " ")  (print stack) (display " ") (print stack_name) (display " ") (println stack_call)
    (if (empty? a)
        (if (empty? stack) list_call (cons (cons (car stack_name) (car stack_call) ) list_call)  )
        (let ((x (car a) ) )
        (cond ((= s 0) (g (if (equal? x "(") 1 0) (cdr a) null null null list_call) )
              ((= s 1) (g (if (equal? x "define") 2 0) (cdr a) null null null list_call) )
              ((= s 2) (if (equal? x "(")
                           (g 3 (cdr a) (list 2) null null list_call)
                           (g 0 (cdr a) null null null list_call) ) )
              ((= s 3) (g 4 (cdr a) stack (list x) (list (list "define") ) list_call) )
              ((= s 4)(if (empty? stack)
                          (g 0 a null null null list_call)
                      (if (= 0 (car stack) )
                          (g 4 a (cdr stack) (cdr stack_name) (cdr stack_call) (cons (cons (car stack_name) (car stack_call) ) list_call) )   
                      (if (equal? x "(")
                          (g 4 (cdr a) (add1list stack) stack_name (insert (cadr a) stack_call) list_call)
                      (if (equal? x ")")
                          (g 4 (cdr a) (sub1list stack) stack_name stack_call list_call)
                      (if (equal? x "define")
                          (if (equal? (cadr a) "(")
                              (g 4 (cdddr a) (cons 2 (add1list stack) ) (cons (caddr a) stack_name) (cons null stack_call) list_call)
                              (g 4 (cddr a) stack stack_name stack_call list_call ) )
                          (g 4 (cdr a) stack stack_name stack_call list_call) ) ) ) ) ) ) ) ) ) )
  (define list_call (g 0 a null null null null) )
  
  (define (find_rec list_call rec dif)
    (if (empty? list_call) (cons rec dif)
        (if (member (caar list_call) (cdar list_call) )
            (find_rec (cdr list_call) (cons (caar list_call) rec) dif )
            (find_rec (cdr list_call) rec (cons (caar list_call) dif) ) ) ) )
  
  (define func (find_rec list_call null null) )
  (define (print_out lst out)
    (if (empty? lst) (display "" out)
        (begin (display (car lst) out ) (line out) (print_out (cdr lst) out) ) ) )
  (print_out (car func) outrec)
  (print_out (cdr func) outdif)
  (close-output-port outrec)
  (close-output-port outdif) )

  