;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname relative) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;
;;/----------------------------------\
;;    Cameron Carmichael (20881375)
;;    CS 135 Fall 2020
;;    Midterm 2 Question 3
;;\----------------------------------/
;;

;;(relative->absolute ints) from a list of integers, produces the list of values which result from
;;   adding these values, one at a time, starting from an initial value of 0.

;;Examples
(check-expect (relative->absolute '(3 -2 4)) '(3 1 5))
(check-expect (relative->absolute empty) empty)

;;(relative->absolute ints) (ListOf int) -> (ListOf int)
(define (relative->absolute ints)
  (cond[(empty? ints) empty]
       [(empty? (rest ints)) (cons (first ints) empty)]
       [else (cons (first ints)
                   (relative->absolute (cons (+ (first ints) (first (rest ints)))
                                             (rest (rest ints)))))]))

;;Tests
(check-expect (relative->absolute '(4)) '(4))
(check-expect (relative->absolute '(4 0 -7)) '(4 4 -3))