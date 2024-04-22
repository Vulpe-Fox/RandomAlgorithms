;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname enhancements) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;;
;;/----------------------------------\
;;    Cameron Carmichael (20881375)
;;    CS 135 Fall 2020
;;    Assignment 01, Enhancements
;;\----------------------------------/
;;

;;(my-check-expect value1 value2) produces a symbol:
;;   'Passed if value1 and value2 are equivalent (using Beginning Student Types)
;;   'Failed otherwise
;;(my-check-within value1 value2 tolerance) produces a symbol:
;;   'Passed if value1 is within the tolerance error of value2
;;   'Failed otherwise

;;Examples
(check-expect (my-check-expect 1 1) 'Passed)
(check-expect (my-check-expect true true) 'Passed)
(check-expect (my-check-expect 'Passed 'Passed) 'Passed)
(check-expect (my-check-expect (make-posn 1 5) (make-posn 1 5)) 'Passed)
(check-expect (my-check-expect #\c #\c) 'Passed)
(check-expect (my-check-expect "2f" "2f") 'Passed)
(check-expect (my-check-expect (make-list 3 "uwu") (make-list 3 "uwu")) 'Passed)
(check-expect (my-check-expect eof eof) 'Passed)
(check-expect (my-check-expect 2 6) 'Failed)

(check-expect (my-check-within 12 10 0.2) 'Passed)
(check-expect (my-check-within 13 10 0.2) 'Failed)

;;Functions

;;(my-check-expect value1 value2)
;;   (Num, Bool, Sym, Posn, Char, String, List, EoF Obj) +
;;   (Num, Bool, Sym, Posn, Char, String, List, EoF Obj) -> Sym
(define (my-check-expect value1 value2)
  (cond[(and (number? value1) (number? value2) (= value1 value2))       'Passed] ;Numbers
       [(and (boolean? value1) (boolean? value2) (boolean=? value1 value2))    'Passed] ;Booleans
       [(and (symbol? value1) (symbol? value2) (symbol=? value1 value2))       'Passed] ;Symbols
       [(and (posn? value1)                  (posn? value2)                     ;Positions
             (= (posn-x value1) (posn-x value2)) (equal? (posn-y value1) (posn-y value2)))
                                                                               'Passed] 
       [(and (char? value1) (char? value2) (char=? value1 value2))             'Passed] ;Characters
       [(and (string? value1) (string? value2) (string=? value1 value2))       'Passed] ;Strings
       [(and (list? value1) (list? value2) (equal? value1 value2))             'Passed] ;Lists
       [(and (eof-object? value1) (eof-object? value2) (equal? value1 value2)) 'Passed] ;EoF Object
       [else                                                                   'Failed])) 

;;(my-check-within value1 value2 tolerance) Num + Num + Num -> Sym
;;Requires:
;;   tolerance >= 0
(define (my-check-within value1 value2 tolerance)
  (cond[(and (<= (- value2 (* value2 tolerance)) value1)
             (<= value1 (+ value2 (* value2 tolerance)))) 'Passed]
       [else                                              'Failed]))

;;Tests
(check-expect (my-check-expect (char->integer #\a) 97) 'Passed)
(check-expect (my-check-expect 1 2) 'Failed)
(check-expect (my-check-expect true false) 'Failed)
(check-expect (my-check-expect 'Passed 'Failed) 'Failed)
(check-expect (my-check-expect (make-posn 1 5) (make-posn 1 true)) 'Failed)
(check-expect (my-check-expect #\c #\h) 'Failed)
(check-expect (my-check-expect "2f" "30vision") 'Failed)
(check-expect (my-check-expect (make-list 3 "uwu") (make-list 3 "owo")) 'Failed)
(check-expect (my-check-expect eof "conjunct") 'Failed)

(check-expect (my-check-within 10 10 0) 'Passed)