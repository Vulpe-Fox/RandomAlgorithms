;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname parity) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;;
;;/----------------------------------\
;;    Cameron Carmichael (20881375)
;;    CS 135 Fall 2020
;;    Assignment 02, Problem 5
;;\----------------------------------/
;;

;;(parity binary-string) produces the symbol 'even if there are an even number
;;   of 1's, otherwise produces the symbol 'odd
;;(calc-num-of-ones cLst) produces the number of 1's in a list of characters

;;Examples
(check-expect (parity "110101") 'even)
(check-expect (parity "1110011") 'odd)

;;(parity binary-string) String -> Symbol
;;   String must be non-empty and composed only of zeroes and ones
(define (parity binary-string)
  (cond[(= (remainder (calc-num-of-ones (string->list binary-string)) 2) 1) 'odd]
       [else 'even]))

;;(calc-num-of-ones nLst) cList -> Num
(define (calc-num-of-ones cLst)
  (cond[(empty? (rest cLst)) (cond[(equal? (first cLst) #\1) 1]
                                  [else 0])]
       [else (+ (calc-num-of-ones (rest cLst)) (cond[(equal? (first cLst) #\1) 1]
                                                    [else 0]))]))

;;Tests
(check-expect (parity "1110010") 'even) ;;Ends in 0
(check-expect (parity "1") 'odd) ;;1
(check-expect (parity "0") 'even) ;;0