;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname extremes) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;;
;;/----------------------------------\
;;    Cameron Carmichael (20881375)
;;    CS 135 Fall 2020
;;    Assignment 02, Problem 2
;;\----------------------------------/
;;

;;
;;Part A
;;

;;(smallest nLst) produces the smallest number in the list of numbers nLst

;;Examples
(check-expect (smallest (cons -5 (cons 2 (cons 10.5 empty)))) -5)

;;(smallest nLst) nLst -> Num
;;nLst is not empty

(define (smallest nLst)
  (cond[(empty? (rest nLst)) (first nLst)]
      [(< (first nLst) (smallest (rest nLst))) (first nLst)]
      [else (smallest (rest nLst))]))

;;Tests
(check-expect (smallest (cons 10.5 (cons 2 (cons -5 empty)))) -5)
(check-expect (smallest (cons 10.5 (cons -5 (cons 2 empty)))) -5)

;;
;;Part B
;;

;;(largest nLst) produces the largest number in the list of numbers nLst

;;Examples
(check-expect (largest (cons -5 (cons 2 (cons 10.5 empty)))) 10.5)

;;(largest nLst) nLst -> Num
;;nLst is not empty
(define (largest nLst)
  (cond[(empty? (rest nLst)) (first nLst)]
      [(> (first nLst) (largest (rest nLst))) (first nLst)]
      [else (largest (rest nLst))]))

;;Tests
(check-expect (largest (cons 10.5 (cons 2 (cons -5 empty)))) 10.5)
(check-expect (largest (cons 2 (cons 10.5 (cons -5 empty)))) 10.5)

;;
;;Part C
;;

;;(max-diff nLst) produces the difference between the largest number and the smallest number in the
;;   list of numbers nLst

;;Examples
(check-expect (max-diff (cons -5 (cons 2 (cons 10.5 empty)))) 15.5)

;;(max-diff nLst) nLst -> Num
;;nLst is not empty
(define (max-diff nLst)
  (- (largest nLst) (smallest nLst)))

;;Tests
(check-expect (max-diff (cons 10.5 (cons 2 (cons -5 empty)))) 15.5)
(check-expect (max-diff (cons 2 (cons 10.5 (cons -5 empty)))) 15.5)