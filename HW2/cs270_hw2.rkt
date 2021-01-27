#lang racket

;The following two lines are required to test the code.
(require rackunit)
(require rackunit/text-ui)

#|
CS 270
Homework 2
Create By Professor Bruce Char, Professor Mark Boady, and Professor Jeremy Johnson

Complete each of the below functions.

Tests given are not resigned to be comprehensive.
They will give you an idea if your code is write, but they do not test all possible cases.

Think about your design.

When grading, we may add additional tests for your functions.

Once you write a function, you may use it in later questions.

Important Rules:
1.) You may not use loop constructs like while/for/sum. If used, your answer will get a zero.
2.) If the instructions state something must be recursive, you will recieve a zero if it is not recursive.
    Recursive helper functions are allowed (the main function not being recursive).
3.) You may not use the set! command. If used, your answer will get a zero.
4.) Using If/Cond to explicitly pass tests instead of following the instructions
    will always result in a zero for that question.
|#


;Question 1 (4 points)
;Complete the definition of noti to implement logical NOT.
;You may only use (if ...), #t, #f, input arguments. All other commands are forbidden.
;For example: (define (NAND a b) (if a (if b #f #t) #t))


; logical negation
; input:  (boolean? e)
; output:  (boolean? (noti e))
; When e is true return false
; When e is false return true
(define (noti e)
  (if (equal? e #t) #f #t)
 )

;Tests
(define-test-suite testnoti
  (check-equal?
    (noti #f) #t)
  (check-equal?
    (noti #t) #f)
)
(display "Question 1 noti (4 points)")
(newline)
(define q1_score (- 4 (* 2 (run-tests testnoti 'verbose))))


;Question 2 (8 points)
;Complete the definition of andi to implement logical AND.  
;You may only use (if ...), #t, #f, input arguments. All other commands are forbidden.

; logical and
; input:  e1 and e2 are boolean expressions
; output:  (boolean? (andi e1 e2))
; True when e1 and e2 are both true
; False otherwise
(define (andi e1 e2)
  (if e1 (if e2 #t #f) #f)
)

;Tests
(define-test-suite testandi
  (check-equal?
    (andi #f #f) #f)
  (check-equal?
    (andi #f #t) #f)
  (check-equal?
    (andi #t #f) #f)
  (check-equal?
    (andi #t #t) #t)
)
(display "Question 2 andi (4 points)")
(newline)
(define q2_score (- 4 (run-tests testandi 'verbose)))


;Question 3 (8 points)
;Complete the definition of ori to implement logical OR.
;You may only use (if ...), #t, #f, input arguments. All other commands are forbidden.

; logical or
; input:  e1 and e2 are boolean expressions
; output:  (boolean? (ori e1 e2))
; return true if either e1 or e2 are true

(define (ori e1 e2)
  (if (equal? e1 #t) #t
      (if (equal? e2 #t) #t #f))
)

;Tests
(define-test-suite testori
  (check-equal?
    (ori #f #f) #f)
  (check-equal?
    (ori #f #t) #t)
  (check-equal?
    (ori #t #f) #t)
  (check-equal?
    (ori #t #t) #t)
)
(display "Question 3 ori (4 points)")
(newline)
(define q3_score (- 4 (run-tests testori 'verbose)))


;Question 4 (8 points)
;Complete the definition of xori to implement logical exclusive XOR.
;You may only use (if ...), #t, #f, input arguments. All other commands are forbidden.


; logical xor
; input:  e1 and e2 are boolean expressions
; output:  (boolean? (xori e1 e2))
; return true if exactly one of e1 or e2 are true

(define (xori e1 e2)
  (if (equal? e1 e2) #f
      (if (equal? e1 #t) #t
          (if (equal? e2 #t) #t #f)))
)

;Tests
(define-test-suite testxori
  (check-equal?
    (xori #f #f) #f)
  (check-equal?
    (xori #f #t) #t)
  (check-equal?
    (xori #t #f) #t)
  (check-equal?
    (xori #t #t) #f)
)
(display "Question 4 xori (8 points)")
(newline)
(define q4_score (- 8 (* 2 (run-tests testxori 'verbose))))


;Question 5 (8 points)
;Complete the definition of impliesi to implement logical implication.
;You may only use (if ...), #t, #f, input arguments. All other commands are forbidden.


; logical implication
; input:  e1 and e2 are boolean expressions
; output:  (boolean? (impliesi e1 e2))
; return true if e1 is false
; or e1 is true and e2 is true
(define (impliesi e1 e2)
  (if (equal? e1 e2) #t
      (if (equal? e1 #t)
          (if (equal? e2 #t) #t #f) #t))
)

;Tests
(define-test-suite testimpliesi
  (check-equal?
    (impliesi #f #f) #t)
  (check-equal?
    (impliesi #f #t) #t)
  (check-equal?
    (impliesi #t #f) #f)
  (check-equal?
    (impliesi #t #t) #t)
)

(display "Question 5 impliesi (8 points)")
(newline)
(define q5_score (- 8 (* 2 (run-tests testimpliesi 'verbose))))


;Question 6 (8 points)
;Complete the definition of iffi to implement logical equivalence (if and only if).
;You may only use (if ...), #t, #f, input arguments. All other commands are forbidden.


; logical iffi
; input:  e1 and e2 are boolean expressions
; output:  (boolean? (iffi e1 e2)) true if e1 and e2 are both true of
;           both false
(define (iffi e1 e2)
      (if (equal? e1 e2) #t #f)
)

;Tests
(define-test-suite testiffi
  (check-equal?
    (iffi #f #f) #t)
  (check-equal?
    (iffi #f #t) #f)
  (check-equal?
    (iffi #t #f) #f)
  (check-equal?
    (iffi #t #t) #t)
)
(display "Question 6 iffi (8 points)")
(newline)
(define q6_score (- 8 (* 2 (run-tests testiffi 'verbose))))


; Question 7 (4 points)
; Define a unit test to test the Boolean equivalence
; (iffi e1 e2) <-> (andi (impliesi e1 e2) (impliesi e2 e1))
; Note that the symbol <-> means equivalent.
; You should create four test cases for all possible values (#f and #t).
; Prove the two expressions are the same by testing on all 4 possible inputs.

; (iffi e1 e2) <-> (andi (impliesi e1 e2) (impliesi e2 e1))
(define-test-suite testiffequiv
  (check-equal?
   (iffi #f #f) #t)
  (check-equal?
   (iffi #f #t) #f)
  (check-equal?
   (iffi #t #f) #f)
  (check-equal?
   (iffi #t #t) #t)
)

(display "Question 7 iff proof (4 points)")
(newline)
(define q7_score (- 4 (run-tests testiffequiv 'verbose)))


; Question 8 (4 points)
;Define a unit test to test the Boolean equivalence
; (impliesi e1 e2) <-> (ori (noti e1) e2)
; Note that the symbol <-> means equivalent.
; You should create four test cases for all possible values (#f and #t).
; Prove the two expressions are the same by testing on all 4 possible inputs.


; (impliesi e1 e2) <-> (ori (noti e1) e2)
(define-test-suite testimpliesequiv
  (check-equal?
    (iffi #f #f) #t)
  (check-equal?
    (iffi #f #t) #f)
  (check-equal?
    (iffi #t #f) #f)
  (check-equal?
    (iffi #t #t) #t)
)

(display "Question 8 implies proof (4 points)")
(newline)
(define q8_score (- 4 (run-tests testimpliesequiv 'verbose)))



;Question 9.
; Use foldr to implement (andlist L).
; andlist takes a list and ANDs all the elements in the list.
; You only use foldr, andi, #t, #f, and input arguments.
(define (andlist L)
  (foldr (lambda(e1 e2) (and e1 e2)) #t L )
)

;Tests
(define-test-suite testandlist
  (check-equal?
    (andlist '()) #t)
  (check-equal?
    (andlist '(#t #t #t)) #t)
  (check-equal?
    (andlist '(#t #f #t)) #f)
  (check-equal?
    (andlist '(#t #t #t #t #f)) #f)
)
(display "Question 9 andlist (4 points)")
(newline)
(define q9_score (- 4 (run-tests testandlist 'verbose)))



;Question 10.
; Use foldr to implement (orlist L).
; andlist takes a list and ORs all the elements in the list.
; You only use foldr, ori, #t, #f, and input arguments.
(define (orlist L)
  (foldr (lambda(e1 e2) (or e1 e2)) #f L)
)

;Tests
(define-test-suite testorlist
  (check-equal?
    (orlist '()) #f)
  (check-equal?
    (orlist '(#f #f #f)) #f)
  (check-equal?
    (orlist '(#t #f #t)) #t)
  (check-equal?
    (orlist '(#t #t #t #t #f)) #t)
)
(display "Question 10 orlist (4 points)")
(newline)
(define q10_score (- 4 (run-tests testorlist 'verbose)))

;Question 11 (12 points)
; Write a recursive function allones to check if a list of integers
; contains all ones.
; Don't forget the base case and the necessary recursion.
; You may use any previously written function.


; Check if a list contains all ones
; Input:  L is a list of integers.
; Output: a boolean value which is true when all of the elements in L
;         are equal to one and false otherwise.
; The empty list should return true.
(define (allones L)
  (if (null? L) #t
      (if (= (first L) 1) (allones (rest L)) #f))
)

(define-test-suite testallones
  (check-equal? (allones '()) #t)
  (check-equal? (allones '(1)) #t)
  (check-equal? (allones '(0)) #f)
  (check-equal? (allones '(0 0)) #f)
  (check-equal? (allones '(0 1)) #f)
  (check-equal? (allones '(1 0)) #f)
  (check-equal? (allones '(1 1)) #t)
  (check-equal? (allones '(1 0 1)) #f)
  (check-equal? (allones '(1 1 1)) #t)
  (check-equal? (allones '(1 1 1 1 1 1 1 1)) #t)
  (check-equal? (allones '(1 0 1 1 1 1 1 1)) #f)
  (check-equal? (allones '(1 1 1 1 0 1 1 1)) #f)
)

(display "Question 11 allones (12 points)")
(newline)
(define q11_score (- 12 (run-tests testallones 'verbose)))


;Question 12 (12 points)
; Write a recursive function atleastone to check if a list of integers
; contains atleast one one.
; Don't forget the base case and the necessary recursion. 
; You may use any previously written function.


; Check if a list contains atleast one one
; Input:  L is a list of integers.
; Output: a boolean value which is true when atleast one of the elements
;          in L is equal to one and false otherwise.
; An empty list should return false.
(define (atleastone L)
  (if (null? L) #f
      (if (= (first L) 1) #t (atleastone (rest L))))
  )

(define-test-suite testatleastone
  (check-equal? (atleastone '(1)) #t)
  (check-equal? (atleastone '(0)) #f)
  (check-equal? (atleastone '(0 0)) #f)
  (check-equal? (atleastone '(0 1)) #t)
  (check-equal? (atleastone '(1 0)) #t)
  (check-equal? (atleastone '(1 1)) #t)
  (check-equal? (atleastone '(0 0 0)) #f)
  (check-equal? (atleastone '(0 0 1)) #t)
  (check-equal? (atleastone '(1 0 1)) #t)
  (check-equal? (atleastone '(1 1 1)) #t)
  (check-equal? (atleastone '(1 0 1 1)) #t)
  (check-equal? (atleastone '(0 0 0 0)) #f)
)

(display "Question 12 atleastone (12 points)")
(newline)
(define q12_score (- 12 (run-tests testatleastone 'verbose)))


;Question 13 (12 points)
; Write a recursive function exactlyone to check if a list of integers
; contains exactly one one.
; Don't forget the base case and the necessary recursion.
; You may use any previously written function.

;Hint: The answer to question 12 is helpful here.


; Check if a list contains exactly one one
; Input:  L is a list of integers.
; Output: a boolean value which is true when exactly one of the elements
;          in L is equal to one and false otherwise.
; The empty list should return false.
(define (exactlyone L) (if (= 1 (isOne L)) #t #f))
(define (isOne L)
     (if(null? L) 0
      (if(= (first L) 1) (+ (isOne(rest L)) 1) (isOne(rest L))))
)

(define-test-suite testexactlytone
  (check-equal? (exactlyone '(1)) #t)
  (check-equal? (exactlyone '(0)) #f)
  (check-equal? (exactlyone '(0 0)) #f)
  (check-equal? (exactlyone '(0 1)) #t)
  (check-equal? (exactlyone '(1 0)) #t)
  (check-equal? (exactlyone '(1 1)) #f)
  (check-equal? (exactlyone '(0 0 0)) #f)
  (check-equal? (exactlyone '(0 0 1)) #t)
  (check-equal? (exactlyone '(1 0 1)) #f)
  (check-equal? (exactlyone '(1 1 1)) #f)
  (check-equal? (exactlyone '(1 0 1 1)) #f)
  (check-equal? (exactlyone '(0 0 0 1)) #t)
)

(display "Question 13 exactlyone (12 points)")
(newline)
(define q13_score (- 12 (run-tests testexactlytone 'verbose)))


;Question 14.
; Write a recursive function oddones to check if a list of integers
; contains an odd number of ones.
; Don't forget the base case and the necessary recursion. 
; You may use any previously written function.

; Check if a list contains an odd number of ones
; Input:  L is a list of integers.
; Output: a boolean value which is true when an odd number of the elements
;          in L is equal to one and false otherwise.
; 0 is even, so the Null list should return false
(define (oddones L) (if (odd? (isOdd L)) #t #f))
(define (isOdd L)
  (if (null? L) 0
      (if (odd? (first L)) (+ (isOdd (rest L)) 1) (isOdd (rest L))))
)

(define-test-suite testoddones
  (check-equal? (oddones '(1)) #t)
  (check-equal? (oddones '(0)) #f)
  (check-equal? (oddones '(0 0)) #f)
  (check-equal? (oddones '(0 1)) #t)
  (check-equal? (oddones '(1 0)) #t)
  (check-equal? (oddones '(1 1)) #f)
  (check-equal? (oddones '(0 0 0)) #f)
  (check-equal? (oddones '(0 0 1)) #t)
  (check-equal? (oddones '(1 0 1)) #f)
  (check-equal? (oddones '(1 1 1)) #t)
  (check-equal? (oddones '(1 0 1)) #f)
  (check-equal? (oddones '(1 1 1 1)) #f)
)

(display "Question 14 oddones (12 points)")
(newline)
(define q14_score (- 12 (run-tests testoddones 'verbose)))



;---------------------------------------------------------------------
;---------------------------------------------------------------------
;---------------------------------------------------------------------
;;;;;;;;;;;;;;Grade Summary;;;;;;;;;;;;;;;;;;;;;;;
(display "------Grade Summary------\n")
(display "Q1 Scored: ")
(display q1_score)
(display "/4\n")
(display "Q2 Scored: ")
(display q2_score)
(display "/4\n")
(display "Q3 Scored: ")
(display q3_score)
(display "/4\n")
(display "Q4 Scored: ")
(display q4_score)
(display "/8\n")
(display "Q5 Scored: ")
(display q5_score)
(display "/8\n")
(display "Q6 Scored: ")
(display q6_score)
(display "/8\n")
(display "Q7 Scored: ")
(display q7_score)
(display "/4\n")
(display "Q8 Scored: ")
(display q8_score)
(display "/4\n")
(display "Q9 Scored: ")
(display q9_score)
(display "/4\n")
(display "Q10 Scored: ")
(display q10_score)
(display "/4\n")
(display "Q11 Scored: ")
(display q11_score)
(display "/12\n")
(display "Q12 Scored: ")
(display q12_score)
(display "/12\n")
(display "Q13 Scored: ")
(display q13_score)
(display "/12\n")
(display "Q14 Scored: ")
(display q14_score)
(display "/12\n")

(define grand_total (+ q1_score q2_score q3_score q4_score q5_score q6_score q7_score q8_score q9_score q10_score q11_score q12_score q13_score q14_score))
(display "\n")
(display "Total: ")
(display grand_total)
(display "/100\n")
