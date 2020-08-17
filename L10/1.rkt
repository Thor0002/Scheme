#lang scheme
(define (f s)
 (define (ch->n ch)
   (cond ((eq? ch #\0) 0) ((eq? ch #\1) 1) ((eq? ch #\2) 2)
         ((eq? ch #\3) 3) ((eq? ch #\4) 4) ((eq? ch #\5) 5)
         ((eq? ch #\6) 6) ((eq? ch #\7) 7) ((eq? ch #\8) 8) ((eq? ch #\0) 0) ) )
  (define (g s n)
    (if (equal? "" s) n
        (g (substring s 1) (+ (* n 10) (ch->n (string-ref s 0) ) ) ) ) )
  (g s 0) )

(define (f1 s)
  (define (g s n)
    (if (equal? "" s) n
        (g (substring s 1) (+ (* n 10) (- (char->integer (string-ref s 0) ) (char->integer #\0) ) ) ) ) )
  (g s 0) )
