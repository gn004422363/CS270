#lang racket
;#4
(define (pow a b)
  (if (<= b 0) 1 (* a (pow a (- b 1))))
 )
(pow 2 4)

;#5
(define (S n)
  (if (<= n 0) 0 (+ n (S (- n 1))))
)
(S 4)
(S 5)