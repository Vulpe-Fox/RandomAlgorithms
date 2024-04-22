;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname factors) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;;
;;/----------------------------------\
;;    Cameron Carmichael (20881375)
;;    CS 135 Fall 2020
;;    Assignment 03, Problem 6
;;\----------------------------------/
;;

;;(all-factors nat) produces a list of all natural numbers x where 0 < x < Nat such that
;;   x divides n evenly
;;(list-factors nat current-number) produces a list of numbers which evenly divide a natural number
;;   iterating as long as current-number < nat
;;(is-prime? nat) produces a boolean value:
;;   true if it is prime
;;   false otherwise
;;(is-composite? nat) produces a boolean value:
;;   true if it is composite
;;   false otherwise

;;Examples
(check-expect(all-factors 30)(cons 1 (cons 2 (cons 3 (cons 5 (cons 6 (cons 10 (cons 15 empty))))))))
(check-expect (is-prime? 30) false)
(check-expect (is-prime? 7) true)
(check-expect (is-composite? 30) true)
(check-expect (is-composite? 7) false)

;;(all-factors nat) Nat -> natLst
(define (all-factors nat)
  (cond[(zero? nat) empty]
       [(= nat 1) (cons 1 empty)]
       [else (list-factors nat 1)]))

;;(list-factors nat current-number) Nat + Nat -> natLst
;;current-number>0
;;current-number <= nat
(define (list-factors nat current-number)
  (cond[(= nat current-number) empty]
       [(zero? (remainder nat current-number))
        (cons current-number (list-factors nat (add1 current-number)))]
       [else (list-factors nat (add1 current-number))]))

;;(is-prime? nat) Nat -> Boolean
(define (is-prime? nat)
  (cond[(or (zero? nat) (= nat 1)) false]
       [(equal? (all-factors nat) (cons 1 empty)) true]
       [else false]))

;;(is-composite? nat) Nat -> Boolean
(define (is-composite? nat)
  (cond[(or (zero? nat) (= nat 1)) false]
       [else (not (is-prime? nat))]))

;;Tests
(check-expect(all-factors 0) empty)
(check-expect(all-factors 1) (cons 1 empty))
(check-expect (is-prime? 0) false)
(check-expect (is-prime? 1) false)
(check-expect (is-composite? 0) false)
(check-expect (is-composite? 1) false)