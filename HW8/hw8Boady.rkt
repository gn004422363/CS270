#lang racket
(require rackunit)
(require rackunit/text-ui)

;CS 270
;Homework 8
;Professor B. Char, M. Boady, and J. Johnson

;Important Rules:
;1.) You may not use loop constructs like while/for/sum. If used, your answer will get a zero.
;2.) If the instructions state something must be recursive, you will recieve a zero if it is not recursive.
;    Recursive helper functions are allowed (the main function not being recursive).
;3.) You may not use the set! command. If used, your answer will get a zero.
;4.) Using If/Cond to explicitly pass tests instead of following the instructions
;    will always result in a zero for that question.

;Each of the below questions has two parts.
;First, you will be asked to write a Racket function to solve a problem.
;Secondly, you will be asked to prove by induction that your
;Racket code has some property.


;---------------------------------------------------------------------
;---------------------------------------------------------------------
;---------------------------------------------------------------------
;Question 1a (10 points)
;Write a recursive function to compute
;the sum( 6*x^2 , x = 1..n) for a given n
;You must write a recursive function.
;If you use any iterative commands (for/loop/sum/etc you will receive a 0)

; Computes sum( 6*x^2 , x = 1..n)
; Input:  n an integer >= 1
; Output: an integer, the result of the summation
(define (spec_sum n)
  (if (equal? n 1)
      6
      (+ (* n 2) (spec_sum (- n 1)))
      )
  )

;Test Bed
(display "Question 1a spec_sum Tests (5 points)\n")
(define-test-suite test_spec_sum
  (check-equal? (spec_sum 1) 6)
  (check-equal? (spec_sum 2) 30)
  (check-equal? (spec_sum 3) 84)
  (check-equal? (spec_sum 4) 180)
  (check-equal? (spec_sum 5) 330)
  (check-equal? (spec_sum 6) 546)
  (check-equal? (spec_sum 7) 840)
  (check-equal? (spec_sum 8) 1224)
  (check-equal? (spec_sum 9) 1710)
  (check-equal? (spec_sum 10) 2310)
)
(define q1a_score (- 10 (run-tests test_spec_sum 'verbose)))

;Question 1b (10 points)
;Prove by induction that
;for all integers n > 1 -> (spec_sum n) = 2n^3+3n^2+n

;Give your proof below in comments
#|
Base Case
taking n = 1
2(1)^3 + 3(1)^2 + 1 = 6
(spec_sum 1)
(if (= 1 1) 6 (+ (* n 2) (spec_sum (- n 1)))))
(if #t 6 (+ (* n 2) (spec_sum (- n 1)))))
6 by definition of spec_sum

Inductive hypotesis


|#

;---------------------------------------------------------------------
;---------------------------------------------------------------------
;---------------------------------------------------------------------
;Question 2 (10 points)
; Write a recursive function evenzeros to check if a list of integers
; contains an even number of zeros.
; Don't forget the base case and the necessary recursion. 

; Check if a list contains an even number of zeros
; Input:  L is a list of integers.
; Output: a boolean value which is true when an even number of the elements
;          in L is equal to zero and false otherwise.
; 0 is even, so the Null list should return true
(define (evenzeros X)
  (cond
    [(null? X) #t]
    [(equal? (first X) '1) (evenzeros (rest X))]
    [else (xor #t (evenzeros (rest X)))]
    )
  )

;Test Bed
(display "Question 2a evenzeros Tests (10 points)\n")
(define-test-suite test_even_zeros
  (check-equal? (evenzeros '()) #t)
  (check-equal? (evenzeros '(1)) #t)
  (check-equal? (evenzeros '(0)) #f)
  (check-equal? (evenzeros '(0 0)) #t)
  (check-equal? (evenzeros '(7 0)) #f)
  (check-equal? (evenzeros '(1 -2)) #t)
  (check-equal? (evenzeros '(0 0 1)) #t)
  (check-equal? (evenzeros '(4 0 1)) #f)
  (check-equal? (evenzeros '(1 0 8)) #f)
  (check-equal? (evenzeros '(0 11 0 -9)) #t)
)
(define q2a_score (- 10 (run-tests test_even_zeros 'verbose)))
;Question 2b (10 points)
;Prove by induction, algebra, and equational reasoning that
;If L contains an even number of zeros then (equal? (test_even_zeros L) #t)
;If L contains an odd number of zeros then (equal? (test_even_zeros L) #f)
;Hint: You need 4 cases (cons 0 E), (cons x E), (cons 0 O), (cons x O)
;Where, x!=0, E is an even list and O is an odd list.

#|
Base Case
(evenzeros '())
((null? '()) #t ...)
(#t #t (...))
#t By the definition of even_sum

Inductive Hypotesis
if L contains even number of zeros, (even_zero E) equals #t
if L contains odd number of zeros, (even_zero O) equals #f
Prove (evenzeros (cons x E)) where E is an even number of zeros to be true
Prove (evenzeros (cons x E)) where E is an odd number of zeros to be false
Prove (evenzeros (cons 0 E)) where E is an even number of zeros to be false
Prove (evenzeros (cons 0 E)) where E is an odd number of zeros to be true


Inductive Proof 1
Assuming (evenzeros E) is #t
(evenzeros (cons x E))
((null? (cons x E)) #t ...)
(#f #t (...))
((equal? (first (cons x E)) x) (evenzeros (rest (cons x E))) ...)
((equal? x x) (evenzeros (rest (cons x E))) ...)
(evenzeros (rest (cons x E)))
(evenzeros E)
#t [Using I.H.]


Inductive Proof 2
Assuming (evenzeros E) is #f
(evenzeros (cons x E))
((null? (cons x E)) #t ...)
(#f #t (...))
((equal? (first (cons x E)) x) (evenzeros (rest (cons x E))) ...)
((equal? x x) (evenzeros E))
(#t (evenzeros E))
(evenzeros E)
#f [Using I.H.]


Inductive Proof 3
Assuming (evenzeros E) is #t
(evenzeros (cons 0 E))
((null? (cons 0 E)) #t ...)
(#f #t (...))
((equal? (first (cons 0 E)) 1) (evenzeros (rest (cons 0 E))) ...)
((equal? 0 1) (evenzeros (rest (cons 0 E))) ...)
(xor #t (evenzeros E))
(xor #t #t) [Using I.H.]
#f


Inductive Proof 4
Assuming (evenzeros E) is #f
(evenzeros (cons 0 E))
((null? (cons 0 E)) #t ...)
(#f #t (...))
((equal? (first (cons 0 E)) 1) (evenzeros (rest (cons 0 E))) ...)
((equal? 0 1) (evenzeros (rest (cons 0 E))) ...)
(xor #t (evenzeros E))
(xor #t #f) [Using I.H.]
#t 
|#

;---------------------------------------------------------------------
;---------------------------------------------------------------------
;---------------------------------------------------------------------
;Q3a (10 Points)
;Write a recursive function duplicate that takes every element in a list
;and makes a second copy of the item.
;For example if we started with (1 2 3)
;then the duplicated list would be (1 1 2 2 3 3)


; Duplicates Elements in a list
; Input:  X a list
; Output: A new list with two copies of even value in X
(define (duplicate X)
  (cond
    [(null? X) '()]
    [(append (cons (first X) (list (first X))) (duplicate (rest X)))]
    )
  )
(display "Question 3a duplicate Tests (10 points)\n")
(define-test-suite test_duplicate
  (check-equal? (duplicate '()) '())
  (check-equal? (duplicate '(1)) '(1 1))
  (check-equal? (duplicate '(1 2)) '(1 1 2 2))
  (check-equal? (duplicate '(4 6)) '(4 4 6 6))
  (check-equal? (duplicate '((1) (2 3))) '((1) (1) (2 3) (2 3)))
  (check-equal? (duplicate '(4 5 6)) '(4 4 5 5 6 6))
  (check-equal? (duplicate '(7 8 9 10)) '(7 7 8 8 9 9 10 10))
  (check-equal? (duplicate '(1 2 3 4 5)) '(1 1 2 2 3 3 4 4 5 5))
  (check-equal? (duplicate '(9 9 9)) '(9 9 9 9 9 9))
  (check-equal? (duplicate '(1 4 5 6 4 3 4 5))
                '(1 1 4 4 5 5 6 6 4 4 3 3 4 4 5 5))
)
(define q3a_score (- 10 (run-tests test_duplicate 'verbose)))

;Q3b (10 Points)
;Prove By Induction
;(length (duplicate L)) = (* 2 (length L))
;You may use the def of length from class.

#|
Base Case
(length (duplicate '()))
(length ((null? '()) '() ...)
(length (#t '() ...))
(length '())
0


Inductive Hypothesis
(length (duplicate A)) = (* 2 (length A))


Induction Proof

(length (duplicate (cons x A))) = (* 2 (length (cons x A)))
 LHS:

(length (null? (cons x A))  ...)
(length (append (cons (first (cons x A)) (list (first (cons x A)))) (duplicate (rest (cons x A))))
(length (append (cons x (list x)) (duplicate A)))
(+ 1 (length (append x (duplicate A))))
(+ 1 (+ 1 (length (duplicate A))))
(+ 1 (+ 1 (* 2 (length A)))) [Using I.H.]
(* 2 (+ 1 (length A)))


RHS:
(* 2 (length (cons x A)))
(* 2 (+ 1 (length A)))


|#

;---------------------------------------------------------------------
;---------------------------------------------------------------------
;---------------------------------------------------------------------
;Question 4a (10pts)
;Write a recursive function (cut_end L) that removes the last element from the list

; Removes the last element in a list
; Input:  X a list
; Output: A new list with the last element removed
;        If the list is empty, return the empty list
(define (cut_end L)
  (cond
    [(null? L) '()]
    [(append (cons (first L) (list (first L))))]
    )
  )

(display "Question 4a cut_end Tests (10 points)\n")
(define-test-suite test_cut_end
  (check-equal? (cut_end '()) '())
  (check-equal? (cut_end '(1)) '())
  (check-equal? (cut_end '(1 2)) '(1))
  (check-equal? (cut_end '(3 4 5)) '(3 4))
  (check-equal? (cut_end '( (1) (2) (3) )) '( (1) (2) ))
  (check-equal? (cut_end '((1 2 3 4))) '())
  (check-equal? (cut_end '((1 2) (3 4))) '((1 2)))
  (check-equal? (cut_end '(9 9 8)) '(9 9))
  (check-equal? (cut_end '(AND A B)) '(AND A))
  (check-equal? (cut_end '(NOT X)) '(NOT))
)
(define q4a_score (- 10 (run-tests test_cut_end 'verbose)))

;Question 4b
;Prove by Induction that (length (cut_end L)) = (- (length L) 1)
;for all L where (length L) >= 1
;You may use the def of length from class.

#|
Base Case
(cut_end '())

(cond
    [(null? L) '()]
    [(append (cons (first L) (list (first L))))]))

(cond
    [#t '()]
    [...]))

'()
|#

;---------------------------------------------------------------------
;---------------------------------------------------------------------
;---------------------------------------------------------------------
;Question 5a (10pts)
;Write a recursive function (add_pairs L)
;that adds pairs of numbers.
;You may assume the length of L will always be even.

; Adds pairs of numbers
; Input:  L a list (the list must have even length)
; Output: A new list with pairs of elements added together.
(define (add_pairs L) 
  (cond
    [(null? L) '()]
    [(append (cons (first L) (list (first L))))]
    )
  )

(display "Question 5a add_pairs Tests (10 points)\n")
(define-test-suite test_add_pairs
  (check-equal? (add_pairs '()) '())
  (check-equal? (add_pairs '(1 2)) '(3))
  (check-equal? (add_pairs '(1 2 3 4)) '(3 7))
  (check-equal? (add_pairs '(2 2 2 2)) '(4 4))
  (check-equal? (add_pairs '(0 -1 -2 3)) '(-1 1))
  (check-equal? (add_pairs '(1 1 1 1)) '(2 2))
  (check-equal? (add_pairs '(1 2 3 4 5 6 7 8)) '(3 7 11 15))
  (check-equal? (add_pairs '(9 9 9 9 9 9)) '(18 18 18))
  (check-equal? (add_pairs '(7 3 4 6 5 5)) '(10 10 10))
  (check-equal? (add_pairs '(-9 9 -8 8)) '(0 0))
  
)
(define q5a_score (- 10 (run-tests test_add_pairs 'verbose)))

;Question 4b
;Prove by Induction that (length (add_pairs L)) = 1/2*(length L)
;for all L where (even? (length L)) and (length L) >= 0
;You may use the def of length from class.


;---------------------------------------------------------------------
;---------------------------------------------------------------------
;---------------------------------------------------------------------
;;;;;;;;;;;;;;Grade Summary;;;;;;;;;;;;;;;;;;;;;;;
(display "------Grade Summary------\n")
(display "Q1a Scored: ")
(display q1a_score)
(display "/10\n")
(display "Q1b Scored: ?/10 (Graded by TA)\n")
(display "Q2a Scored: ")
(display q2a_score)
(display "/10\n")
(display "Q2b Scored: ?/10 (Graded by TA)\n")
(display "Q3a Scored: ")
(display q3a_score)
(display "/10\n")
(display "Q3b Scored: ?/10 (Graded by TA)\n")
(display "Q4a Scored: ")
(display q4a_score)
(display "/10\n")
(display "Q4b Scored: ?/10 (Graded by TA)\n")
(display "Q5a Scored: ")
(display q5a_score)
(display "/10\n")
(display "Q5b Scored: ?/10 (Graded by TA)\n")


(define grand_total (+ q1a_score q2a_score q3a_score q4a_score q5a_score))
(display "\n")
(display "Total: ")
(display grand_total)
(display "/100\n")