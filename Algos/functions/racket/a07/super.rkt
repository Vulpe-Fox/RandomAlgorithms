;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname super) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;;
;;/----------------------------------\
;;    Cameron Carmichael (20881375)
;;    CS 135 Fall 2020
;;    Assignment 08, Problem 1
;;\----------------------------------/
;;

;; A nested list of X (nested-listof x) is one of:
;; * empty
;; * (cons (nested-listof X) (nested-listof X))
;; * (cons X (nested-listof X))

;;(super-filter pred? lst) filters lst to include only elements, e, by which (pred? e) produces true
;;(ruthless lst) filters lst to exclude any symbols 'ruth
;;(supersize n lst) filters lst to exclude any numbers less than n
;;(super-keeper pred? lst) filters lst to exclude any elements, e, such that (not (pred? e))

;;Examples
(check-expect
 (super-filter
  odd?
  (list 1 (list 2 (list 2 3 4) 5 6 (list 7 8 9)) 10 11 12))
 (list 1 (list (list 3) 5 (list 7 9)) 11))

(check-expect
 (ruthless
  (list 'rabbit
        (list 'apple 'pluto
              (list 'ruth 'blue) 'ruth) 'hello))
 (list 'rabbit
       (list 'apple 'pluto
             (list 'blue)) 'hello))

(check-expect
 (supersize 4 (list 8 1 (list 2 6 3) 10 1))
 (list 8 (list 6) 10))

(check-expect
 (super-keeper
  odd?
  (list 1 (list 2 (list 2 3 4) 5 6 (list 7 8 9)) 10 11 12))
 (list (list 2 (list 2 4) 6 (list 8)) 10 12))

;;(super-filter pred? lst) Predicate + (nested-listof Any) -> (nested-listof Any)
;;Requires: values in lst must be able to be operated on by pred?
(define (super-filter pred? lst)
  (cond[(empty? lst) empty]
       [(list? (first lst))
        (cons (super-filter pred? (first lst)) (super-filter pred? (rest lst)))]
       [(pred? (first lst))
        (cons (first lst) (super-filter pred? (rest lst)))]
       [else (super-filter pred? (rest lst))]))

;;(ruthless lst) (nested-listof Sym) -> (nested-listof Sym)
(define (ruthless lst)
  (local[(define (not-symbol-ruth? element) (not (symbol=? element 'ruth)))]
    (super-filter not-symbol-ruth? lst)))

;;(supersize n lst) Num + (nested-listof Num) -> (nested-listof Num)
(define (supersize n lst)
  (local[(define (not-less-than-n? num) (not (< num n)))]
    (super-filter not-less-than-n? lst)))

;;(super-keeper pred? lst) Predicate + (nested-listof Any) -> (nested-listof Any)
;;Requires: values in lst must be able to be operated on by pred?
(define (super-keeper pred? lst)
  (local[(define (not-pred? element) (not (pred? element)))]
    (super-filter not-pred? lst)))

;;Tests
(check-expect
 (super-filter
  even?
  (list 1 (list 2 (list 2 3 4) 5 6 (list 7 8 9)) 10 11 12))
 (list (list 2 (list 2 4) 6 (list 8)) 10 12))
(check-expect
 (super-filter
  string?
  (list 1 (list "Terrance" (list 2 "Eris" 4) "string" 6 (list 7 8 9)) 10 11 12))
 (list (list "Terrance" (list "Eris") "string" (list))))
(check-expect (super-filter string? (list 1)) empty)
(check-expect (super-filter string? empty) empty)

(check-expect
 (ruthless
  (list 'rabbit
        (list 'apple 'pluto
              (list 'blue)) 'hello))
 (list 'rabbit
       (list 'apple 'pluto
             (list 'blue)) 'hello))
(check-expect (ruthless (list 'ruth)) empty)
(check-expect (ruthless empty) empty)

(check-expect
 (supersize -2 (list -8 1 (list 2 6 3) 10 1))
 (list 1 (list 2 6 3) 10 1))
(check-expect
 (supersize -2 (list 8 1 (list 2 6 3) 10 1))
 (list 8 1 (list 2 6 3) 10 1))
(check-expect
 (supersize 100 (list 8 1 10 1))
 empty)
(check-expect
 (supersize 100 (list 8 1 (list 2 6 3) 10 1))
 (list empty))
(check-expect (supersize 100 empty) empty)

(check-expect
 (super-keeper
  even?
  (list 1 (list 2 (list 2 3 4) 5 6 (list 7 8 9)) 10 11 12))
 (list 1 (list (list 3) 5 (list 7 9)) 11))
(check-expect
 (super-keeper
  string?
  (list 1 (list "Terrance" (list 2 "Eris" 4) "string" 6 (list 7 8 9)) 10 11 12))
 (list 1 (list (list 2 4) 6 (list 7 8 9)) 10 11 12))
(check-expect (super-keeper string? (list "Terrance")) empty)
(check-expect (super-keeper string? empty) empty)