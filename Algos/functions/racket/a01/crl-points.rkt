;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname crl-points) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;;
;;/----------------------------------\
;;    Cameron Carmichael (20881375)
;;    CS 135 Fall 2020
;;    Assignment 01, Problem 4
;;\----------------------------------/
;;

;;(crl-points first-places second-places third-places standing)
;;   produces a score which is equal to
;;   50 * first-places + 20 * second-places + 10 * third-places
;;   The score is then divided by the standing divisor and
;;   rounded down to the nearest decimal. The multiplier symbols
;;   translated to divisor symbols are:
;;   'good-standing   -> 1    -> Full points
;;   'minor-violation -> 0.95 -> Nultiplied by 19, divided by 20
;;   'major-violation -> 0.75 -> Nultiplied by 3, divided by 4
;;   'disqualified    -> 0    -> 0 total
;;(sum num1 num2 num3) produces a sum of scores:
;;   50 * num1 + 20 * num2 + 10 * num3

;;Examples
(check-expect (crl-points 1 2 1 'good-standing) 100)
(check-expect (crl-points 1 2 1 'minor-violation) 95)
(check-expect (crl-points 1 2 1 'major-violation) 75)
(check-expect (crl-points 1 2 1 'disqualified) 0)

;;Functions

;;(crl-points first-places second-places third-places standing)
;;   Num + Num + Num + Sym -> Num
;;Requires:
;; first-places, second-places, third-places >= 0
;; standing = 'good-standing/'minor-violation/
;;            'major-violation/'disqualified
(define (crl-points first-places second-places third-places standing)
  (cond [(symbol=? standing 'good-standing)
         (sum first-places second-places third-places)]
        [(symbol=? standing 'minor-violation)
         (quotient (* (sum first-places second-places third-places) 19) 20)]
        [(symbol=? standing 'major-violation)
         (quotient (* (sum first-places second-places third-places) 3) 4)]
        [(symbol=? standing 'disqualified)
         0]))

;;(sum num1 num2 num3) Num + Num + Num -> Num
(define (sum num1 num2 num3)
  (cond [(>= (+ num1 num2 num3) 5)
         (+ (* 50 num1) (* 20 num2) (* 10 num3) 15)]
        [else (+ (* 50 num1) (* 20 num2) (* 10 num3))]))

;;Tests
(check-expect (crl-points 0 2 4 'minor-violation) 90)
(check-expect (crl-points 0 0 0 'good-standing) 0)
(check-expect (crl-points 0 0 0 'minor-violation) 0)
(check-expect (crl-points 0 0 0 'disqualified) 0)
(check-expect (crl-points 14 22 16 'major-violation) 986)