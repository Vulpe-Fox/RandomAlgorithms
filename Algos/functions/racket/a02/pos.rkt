;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname pos) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;;
;;/----------------------------------\
;;    Cameron Carmichael (20881375)
;;    CS 135 Fall 2020
;;    Assignment 02, Problem 4
;;\----------------------------------/
;;

;;(change-due costs paid) produces a number which is the difference between
;;   the total of the amount paid and the sum from the list of costs
;;(paid-enough? costs paid) produces a boolean true or false if paid >= sum costs or
;;   paid < sum costs, respectively
;;(free-item costs paid) produces the num value of the first item in costs which can cover the
;;   difference if someone hasn't paid enough
;;(calc-sum costs) produces the sum of a list of numbers

(define test-prices (cons 2.65 (cons 23.30 (cons 7.99 (cons 59.99 empty)))))

;;Examples
(check-expect (change-due test-prices 100) 6.07)
(check-expect (change-due test-prices 93.93) 0)
(check-expect (paid-enough? test-prices 100) true)
(check-expect (paid-enough? test-prices 50) false)
(check-expect (free-item test-prices 80) 23.3)
(check-expect (free-item test-prices 60) 59.99)

;;(change-due costs paid) nLst + Num -> Num
;;paid >= sum of costs
(define (change-due costs paid)
  (- paid (calc-sum costs)))

;;(paid-enough? costs paid) nLst + Num -> Boolean
;;paid >= 0
(define (paid-enough? costs paid)
  (cond[(< paid (calc-sum costs)) false]
       [else true]))

;;(free-item costs paid) nLst + Num -> nLst
;;  paid < sum of costs
;;  sum of costs - an element of sum of costs <= paid
(define (free-item costs paid)
  (cond[(< (- (calc-sum costs) (first costs)) paid) (first costs)]
       [else (free-item (rest costs) (- paid (first costs)))]))

;;(calc-sum costs) nLst -> Num
(define (calc-sum costs)
  (cond[(empty? costs) 0]
       [(empty? (rest costs)) (first costs)]
       [else (+ (first costs) (calc-sum (rest costs)))]))

;;Tests
(check-expect (change-due empty 0) 0)