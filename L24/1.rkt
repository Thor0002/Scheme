#lang scheme

(define (f n)
  (map (λ(i)(cons (* (cos (/ (* 2 pi i) n) ) (/ 1 (* 2 (sin (/ pi n) ) ) ) )
                  (* (sin (/ (* 2 pi i) n) ) (/ 1 (* 2 (sin (/ pi n) ) ) ) ) ) ) (build-list n +) ) )
