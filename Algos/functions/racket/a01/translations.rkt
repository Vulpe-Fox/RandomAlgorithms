;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname translations) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;;
;;/----------------------------------\
;;    Cameron Carmichael (20881375)
;;    CS 135 Fall 2020
;;    Assignment 01, Problem 2
;;\----------------------------------/
;;

;;
;;Part A
;;

;;(volume radius) produces the volume of a sphere with a given radius
;;(cube n) proudces a value of n to the power of 3

;;Examples
(check-within (volume 0) 0 0.01)
(check-within (volume 1) 4.1888 0.01)

;;Functions

;;(volume radius) Num -> Num
(define (volume radius)
  (* (* 4/3 pi) (cube radius)))

;;(cube n) Num -> Num
(define (cube n) (* n (* n n)))

;;Tests
(check-within (volume 5/2) 65.4498 0.01)
(check-within (volume 10.5) 4849.0483 0.01)

;;
;;Part B
;;

;;(fib num) produces the approx value
;;    of a given Fiboncacci number at the position num

;;Examples
(check-within (fib 1) 1 0.01)
(check-within (fib 2) 1 0.01)
(check-within (fib 3) 2 0.01)
(check-within (fib 4) 3 0.01)

;;Constants
(define golden-ratio (/ (+ 1 (sqrt 5)) 2))

;;Functions

;;(fib num) Num -> Num
;;Requires:
;;   num > 0
(define (fib num)
  (/ (- (expt golden-ratio num) (expt (- golden-ratio) (- num)))
     (- (* 2 golden-ratio) 1)))

;;Tests
(check-within (fib 7) 13 0.01)
(check-within (fib 9) 34 0.01)