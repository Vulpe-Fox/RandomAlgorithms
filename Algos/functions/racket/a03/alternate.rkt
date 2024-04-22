;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname alternate) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;;
;;/----------------------------------\
;;    Cameron Carmichael (20881375)
;;    CS 135 Fall 2020
;;    Assignment 03, Problem 4
;;\----------------------------------/
;;

;;alt-nat-template: template for definition of natural numbers
;;(powers-of-2 Nat) produces the number of times a natural number can be divided by 2 before reaching
;;   0 or an odd number

;;Examples
(check-expect (powers-of-2 12) 2)
(check-expect (powers-of-2 0) 0)

;;alt-nat-template: Nat -> Any
(define (alt-nat-template nat)
  (cond[(zero? nat) ...]
       [(even? nat) (alt-nat-template (/ nat 2))]
       [(odd? nat) (alt-nat-template (/ (sub1 nat) 2))]))

;;(powers-of-2 Nat) Nat -> Num
(define (powers-of-2 nat)
  (cond[(or (= nat 0) (odd? nat)) 0]
       [else (add1 (powers-of-2 (/ nat 2)))]))

;;Tests
(check-expect (powers-of-2 13) 0)
(check-expect (powers-of-2 32) 5)
(check-expect (powers-of-2 34) 1)