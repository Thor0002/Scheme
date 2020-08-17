#lang scheme

(define (letter? char)
    (define int (char->integer char) )
     (if (or (and (< 96 int) (> 122 int) )
             (and (< 64 int) (> 91 int) )
             (and (< 1039 int) (> 1072 int) )
             (and (< 1071 int) (> 1104 int) ) ) #t #f) )

(define (f file-in file-dict file-out)
  (define in   (open-input-file file-in))
  (define out (open-output-file file-out #:exists 'replace))
  (define dicin (open-input-file file-dict) )
  (define (line) (display #\return out) (display #\newline out) )
  
  
  (define (read_dict s x dict)
    (define c (read-char dicin) )
    (if (equal? c eof) dict
    (if (equal? c #\uFEFF) (read_dict 0 null null) 
    (cond
      ((= s 0)(cond ((equal? c #\space) (read_dict 1 null (cons (cons x (read dicin) ) dict) ) )
                    (else (read_dict 0 (cons c x) dict) ) ) )
      ((= s 1)(if (equal? c #\return) (read_dict 1 null dict) (read_dict 0 null dict) ) ) ) ) ) )
 (define dict (read_dict 0 null null) )
  
 (define (find_word_in_dict dict word)
        (if (equal? (caar dict) word) (cdar dict)
            (find_word_in_dict (cdr dict) word) ) )
  
  (define (next s x)
    (define c (read-char in) )
    (if (equal? c eof)
        (if (empty? x)
            (close-output-port out)
            (begin (write (find_word_in_dict dict x) out)
                   (close-output-port out) ) )
       (cond ((= s 0)(if (letter? c)
                         (next 1 (list c) )
                         (begin (display c out) (next 0 null) ) ) )
             ((= s 1)(if (letter? c)
                         (next 1 (cons c x) )
                         (begin  (write (find_word_in_dict dict x) out)
                                 (display c out)
                                 (next 0 null) ) ) ) ) ) )
   (next 0 null) )
                         