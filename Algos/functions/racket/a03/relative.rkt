;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname relative) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;;
;;/----------------------------------\
;;    Cameron Carmichael (20881375)
;;    CS 135 Fall 2020
;;    Assignment 03, Problem 3
;;\----------------------------------/
;;

;;(differences nLst) produces a list of the differences between each number and the previous number

;;Examples
(check-expect (differences (cons 4 (cons 7 (cons 1 empty))))(cons 3 (cons -6 empty)))

;;(differences nLst) nLst -> nLst
;;nLst must be non-empty
(define (differences nLst)
  (cond[(empty? (rest nLst)) empty]
       [else (cons (- (first (rest nLst)) (first nLst)) (differences (rest nLst)))]))

;;Tests
(check-expect (differences (cons 4 empty)) empty)
(check-expect (differences (cons 4 (cons 7 (cons 9 empty))))(cons 3 (cons 2 empty)))