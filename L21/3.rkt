#lang scheme

(define (f list-adj)
  (define n (length list-adj) )
  (define (n/k i listn listk)
   ;(display listn) (display " ") (displayln listk)
    (if (empty? listn)
        (if (empty? listk) null
            (if (= i (car listk) )
                (cons i (n/k i listn (cdr listk) ) )
                        (n/k i listn (cdr listk) ) ) )
        (if (empty? listk) listn
        (if (= i (car listk) )
            (cons (car listk) (n/k i listn (cdr listk) ) )
        (if (< (car listn) (car listk) )
            (cons (car listn) (n/k i (cdr listn) listk) )
            (if (> (car listn) (car listk) )
                (n/k i listn (cdr listk) )
                (n/k i (cdr listn) (cdr listk) ) ) ) ) ) ) )
  (map (λ(listk i)(n/k i (build-list (- n 1) (λ(x)(if (< x i) x (+ x 1) ) ) ) (sort listk <) ) ) list-adj (build-list n +) ) )