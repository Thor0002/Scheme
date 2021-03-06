#lang scheme

(define (f file-in file-out)
  (define in (open-input-file file-in))
  (define out (open-output-file file-out #:exists 'replace))
  (define (line) (display #\return out) (display #\newline out) )
  (define (numeral? ch) (and (char<=? #\0 ch) (char>=? #\9 ch) ) )
  
  (define (next s x sum k)
    (define c (read-char in))
    (if (equal? c eof)
        (if (empty? x)
            (begin (println sum out) (line)
                   (println k out)
                   (close-output-port out) )
            (let ((n (string->number (list->string (reverse x) ) ) ) )
                           (println n out) (line)
                           (println (+ n sum) out) (line)
                           (println (+ 1 k) out) (line) 
                           (close-output-port out) ) )
        (cond
          ((= s 0) (if (numeral? c)
                         (next 1 (list c) sum k)
                     (if (equal? c #\-)
                         (next 2 (list c) sum k)
                         (next 0 null sum k) ) ) )
          ((= s 1) (if (numeral? c)
                         (next 1 (cons c x) sum k)
                         (let ((n (string->number (list->string (reverse x) ) ) ) )
                           (println n out) (line)
                           (next 0 null (+ sum n) (+ k 1) ) ) ) )
          ((= s 2) (if (numeral? c)
                         (next 1 (cons c x) sum k)
                         (next 0 null sum k) ) ) ) ) )
  (next 0 null 0 0) )