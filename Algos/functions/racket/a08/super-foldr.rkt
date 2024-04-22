;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname super-foldr) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;;
;;/----------------------------------\
;;    Cameron Carmichael (20881375)
;;    CS 135 Fall 2020
;;    Assignment 08, Problem 5
;;\----------------------------------/
;;

;; A nested list of X (nested-listof X) is one of:
;;*empty
;;*(cons (nested-listof X) (nested-listof X))
;;*(cons X (nested-listof X))

;;(super-foldr func base lst) Produces the result of foldr on nested lists
;;(magnitudes nl) Produces the sum of a list of numbers after applying abs to each
;;(super-filter pred? nl) Produces a filtered nested list where pred? results in true

;;Examples
(check-expect (super-foldr + 0 '(1 (5 5 (1 3)) (10 2) 2)) 29)
(check-expect (super-foldr - 0 '(1 (5 5 (1 3)) (10 2) 2)) 9)

(check-expect (magnitudes '(1 (-5 -5 (1 -3)) (-10 2) 2)) 29)

(check-expect(super-filter odd? '(1 (2 (2 3 4) 5 6 (7 8 9)) 10 11 12))
             '(1 ( (3) 5 (7 9)) 11))

;;(super-foldr func base lst) (X -> X) + X + (nested-listof X) -> X
(define (super-foldr func base lst)
  (foldr (lambda (x rror)
           (cond[(list? x) (func (super-foldr func base x) rror)]
                [else (func x rror)])) base lst))

;;(magnitudes nl) (nested-listof Num) -> Nat
(define (magnitudes nl)
  (super-foldr (lambda (x rror) (+ (abs x) rror)) 0 nl))

;;(super-filter pred? nl) (X -> Bool) + (nested-listof X) -> (nested-listof X)
;;Requires: All elements of lst must be able to be operated on by pred?
(define (super-filter pred? lst)
  (super-foldr (lambda (x rror) (cond[(or (list? x) (pred? x)) (cons x rror)]
                                     [else rror])) empty lst))

;;Tests
(check-expect (super-foldr + 0 '()) 0)
(check-expect (super-foldr + 0 '(1 2 3 4)) 10)
(check-expect (super-foldr cons empty '(1 (5 5 (1 3)) (10 2) 2)) '(1 (5 5 (1 3)) (10 2) 2))
(check-expect (super-foldr cons '(1 1) '(1 (5 5 (1 3)) (10 2) 2))
              '(1 (5 5 (1 3 1 1) 1 1) (10 2 1 1) 2 1 1))
(check-expect (super-foldr + 1 '(1 (5 5 (1 3)) (10 2) 2)) 33)

(check-expect (magnitudes '()) 0)
(check-expect (magnitudes '(-1 2 -3 4)) 10)


(check-expect (super-filter (lambda (val) (= val 1)) '(1 (2 (2 3 4) 5 6 (7 8 9)) 10 11 12))
              '(1 (()())))
(check-expect (super-filter odd? '())
              '())
(check-expect (super-filter odd? '(2))
              '())
(check-expect (super-filter odd? '(1))
              '(1))

