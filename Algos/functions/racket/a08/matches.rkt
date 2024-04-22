;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname matches) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;;
;;/----------------------------------\
;;    Cameron Carmichael (20881375)
;;    CS 135 Fall 2020
;;    Assignment 08, Problem 4
;;\----------------------------------/
;;

;;(matches-func? func pairs) Produces true if each pair is in the form (x (func x)), and false
;;   otherwise

;;Examples
(check-expect (matches-func? sqr '((5 25) (2 4) (10 100)))true)
(check-expect (matches-func? add1 '((1 2) (3 5) (10 15)))false)

;;(matches-func? func pairs) (X -> Y) + (Listof (X Y)) -> Bool
;;Requires: Each member of pairs must be a pair of elements (X Y)
;;          func must be operatable on all elements X
(define (matches-func? func pairs)
  (cond[(empty? pairs) true]
       [else (foldl (lambda (value lst) (and value lst)) true
                    (map (lambda (value) (cond[(equal? (func (first value)) (second value)) true]
                                              [else false])) pairs))]))

;;Tests
(check-expect (matches-func? string->list '(("hey" (#\h #\e #\y)))) true)
(check-expect (matches-func? string->list '(("hey" (#\h #\y)))) false)
(check-expect (matches-func? symbol=? '(((true false) false))) true)
(check-expect (matches-func? list->string '()) true)