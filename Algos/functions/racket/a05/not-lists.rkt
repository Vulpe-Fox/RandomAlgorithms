;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname not-lists) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;;
;;/----------------------------------\
;;    Cameron Carmichael (20881375)
;;    CS 135 Fall 2020
;;    Assignment 05, Problem 1
;;\----------------------------------/
;;

(define-struct ls (first rest))
;; A (lsof X) is one of:
;; * 'nothing
;; * (make-ls X (lsof X))

;;(ls-length ls) produces the number of values in ls, a (lsof Any)
;;(ls-max ls) produces the maximum number value of a (lsof Num)

;;Examples
(check-expect (ls-length(make-ls "!" (make-ls 'huh (make-ls 42 'nothing)))) 3)

(check-expect (ls-max (make-ls 5 (make-ls 9 (make-ls 7 'nothing)))) 9)

;;(ls-length ls) (lsof Any) -> Num
;;ls must be non-empty
(define (ls-length ls)
  (cond[(equal? (ls-rest ls) 'nothing) 1]
       [else (+ 1 (ls-length (make-ls (ls-first (ls-rest ls)) (ls-rest (ls-rest ls)))))]))

;;(ls-max ls) (lsof Num) -> Num
;;ls must be non-empty
(define (ls-max ls)
  (cond[(equal? (ls-rest ls) 'nothing) (ls-first ls)]
       [else (max (ls-first ls) (ls-max (make-ls (ls-first (ls-rest ls)) (ls-rest (ls-rest ls)))))]))

;;Tests
(check-expect (ls-length(make-ls "!" (make-ls 'huh (make-ls 'noth 'nothing)))) 3)
(check-expect (ls-length(make-ls 2 (make-ls 45 (make-ls 43 'nothing)))) 3)

(check-expect (ls-max (make-ls 0 (make-ls 0 (make-ls 0 'nothing)))) 0)
(check-expect (ls-max (make-ls 1 (make-ls 1 (make-ls 0 'nothing)))) 1)
(check-expect (ls-max (make-ls -1 (make-ls -2 (make-ls -3 'nothing)))) -1)