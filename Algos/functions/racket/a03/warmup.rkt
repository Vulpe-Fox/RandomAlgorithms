;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname warmup) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;;
;;/----------------------------------\
;;    Cameron Carmichael (20881375)
;;    CS 135 Fall 2020
;;    Assignment 03, Problem 1
;;\----------------------------------/
;;

;;(replace-word string-to-replace replacement-string sLst) produces a new string list which includes
;;   all the elements of the original sLst except all elements which are the same as string-to-replace
;;   are replaced with replacement-string
;;(mult num1 num2) produces the product of two natural numbers through recursion
;;(add num1 num2) produces the sum of two natural numbers through recursion

;;Examples
(check-expect(replace-word "exam" "assessment"(cons "content"(cons "exam"(cons "assignment" empty))))
             (cons "content" (cons "assessment" (cons "assignment" empty))))
(check-expect (add 2 3) 5)
(check-expect (mult 2 3) 6)

;;(replace-word string-to-replace replacement-string sLst) String + String + sLst -> sLst
(define (replace-word string-to-replace replacement-string sLst)
  (cond[(empty? sLst) empty]
       [(string=? (first sLst) string-to-replace)
        (cons replacement-string (replace-word string-to-replace replacement-string (rest sLst)))]
       [else (cons (first sLst) (replace-word string-to-replace replacement-string (rest sLst)))]))

;;(mult num1 num2) Num + Num -> Num
;;Num1,Num2 >= 0
(define (mult num1 num2)
  (cond[(= num2 0) 0]
       [(> num2 1) (add num1 (mult num1 (sub1 num2)))]
       [else num1]))

;;(add num1 num2) Num + Num -> Num
;;num1,num2 >= 0
(define (add num1 num2)
  (cond[(> num2 0) (add (add1 num1) (sub1 num2))]
       [else num1]))

;;Tests
(check-expect(replace-word "exams" "assessment"(cons "content"(cons "exam"(cons "assignment" empty))))
             (cons "content" (cons "exam" (cons "assignment" empty))))
(check-expect (add 2 0) 2)
(check-expect (mult 2 0) 0)
(check-expect (mult 4 3) 12)