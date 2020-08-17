#lang scheme

(define (f G)
  (define vG (list->vector G) )
  (define (g i)
    (foldl (λ(j+j->k res)
             (foldl (λ(k res1)(if (member i (vector-ref vG k))
                                  (cons (sort (list i (car j+j->k) k) <) res1) res1) )
                    res (cdr j+j->k) ) )
           null
          (map (λ(j)(cons j (vector-ref vG j) ) ) (vector-ref vG i)) ) )
  (remove-duplicates (foldl (λ(i res)(append (g i) res) ) null (build-list (length G) +) ) ) )