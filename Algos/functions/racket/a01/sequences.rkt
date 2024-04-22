;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname sequences) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;;
;;/----------------------------------\
;;    Cameron Carmichael (20881375)
;;    CS 135 Fall 2020
;;    Assignment 01, Problem 3
;;\----------------------------------/
;;

;;(sequence-type a b c d) produces a symbol:
;;   'arithmetic if a, b, c, d is an arithmetic sequence
;;   'geometric if a, b, c, d is a geometric sequence
;;   'both if a, b, c, d is both types of sequence
;;   'neither if a, b, c, d is neither sequences
;;(arithmetic? a b c d) produces a boolean:
;;   true if a, b, c, d is an arithmetic sequence
;;   false otherwise
;;(geometric? a b c d) produces a boolean:
;;   true if a, b, c, d is a geometric sequence
;;   false otherwise

;;Examples
(check-expect (sequence-type 1 2 3 4) 'arithmetic)
(check-expect (sequence-type 1 -1 1 -1) 'geometric)
(check-expect (sequence-type 1 1 1 1) 'both)
(check-expect (sequence-type 1 -7 2 -1) 'neither)

;;Functions

;;(sequence-type a b c d) Num + Num + Num + Num -> Symbol
(define (sequence-type a b c d)
  (cond
    [(and (arithmetic? a b c d) (geometric? a b c d)) 'both]
    [(arithmetic? a b c d)                            'arithmetic]
    [(geometric? a b c d)                             'geometric]
    [else                                             'neither]))

;;(arithmetic? a b c d) Num + Num + Num + Num -> boolean
(define (arithmetic? a b c d)
  (cond [(= (- b a) (- c b) (- d c)) true]
        [else                        false]))

;;(geometric? a b c d) Num + Num + Num + Num -> boolean
(define (geometric? a b c d)
  (cond [(and (= a 0) (= b 0) (= c 0) (= d 0)) true]
        [(or (= a 0) (= b 0) (= c 0) (= d 0))  false]
        [(= (/ b a) (/ c b) (/ d c))           true]
        [else                                  false]))

;;Tests
(check-expect (sequence-type 0 50 100 150) 'arithmetic)
(check-expect (sequence-type 1 -3 9 -27) 'geometric)
(check-expect (sequence-type 0 0 0 0) 'both)
(check-expect (sequence-type 1 -100 0 1) 'neither)