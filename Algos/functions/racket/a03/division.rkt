;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname division) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;;
;;/----------------------------------\
;;    Cameron Carmichael (20881375)
;;    CS 135 Fall 2020
;;    Assignment 03, Problem 5
;;\----------------------------------/
;;

;;(divide dividend divisor) produces a list composed of the quotient when the dividend is divided by
;;   the divisor and the remainder without using division
;;(calc-max-multiply-into max-result number-to-multiply multiplied-by) determines the highest 
;;   number-to-multiply which can be multiplied into multiplied-by before they cross a max-result

;;Examples
(check-expect (divide 17 5) (cons 3 (cons 2 empty)))

;;(divide dividend divisor) Nat + Nat -> natLst
;;divisor > 0
(define (divide dividend divisor)
  (cond[(< dividend divisor) (cons 0 (cons dividend empty))]
       [(= dividend divisor) (cons 1 (cons 0 empty))]
       [else (cons (calc-max-multiply-into dividend divisor 1)
                   (cons (- dividend (* divisor (calc-max-multiply-into dividend divisor 1)))
                         empty))]))

;;(calc-max-multiply-into max-result number-to-multiply multiplied-by) Nat + Nat + Nat -> Nat
(define (calc-max-multiply-into max-result number-to-multiply multiplied-by)
  (cond[(>(* number-to-multiply multiplied-by) max-result) 0]
       [else (add1 (calc-max-multiply-into max-result number-to-multiply (add1 multiplied-by)))]))

;;Tests
(check-expect (divide 27 3) (cons 9 (cons 0 empty)))
(check-expect (divide 1 3) (cons 0 (cons 1 empty)))
(check-expect (divide 3 3) (cons 1 (cons 0 empty)))
(check-expect (divide 28 3) (cons 9 (cons 1 empty)))