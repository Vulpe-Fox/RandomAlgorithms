;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname sudoku) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;;
;;/----------------------------------\
;;    Cameron Carmichael (20881375)
;;    CS 135 Fall 2020
;;    Assignment 02, Problem 7
;;\----------------------------------/
;;

;;(sudoku-valid? nLst) produces a boolean true if nLst contains all numbers
;;   from 1-9 exactly once without anything else; false otherwise.
;;(spans-nat? lon n) produces true if lon contains each of the numbers from one to n exactly once
;;   without anything else; false otherwise
;;(find-number nLst-leftover sub-nLst num max-num) recurses to determine if all numbers up to a max
;;   are in a list returns true upon condition or false if an element is missing.
;;   The nLst leftover is a reference list to get rid of elements from and the sub-nLst is a list
;;   which is used to check the first number cascading down.
;;(annihilate-number nLst num) removes the first instance of a number from the list and produces a
;;   new list
;;(construct-rest-of-list nLst) after annihilate-number deletes the first type of a number, this
;;   function constructs and produces the remainder of the list unimpeded
;;(calc-length nLst) produces a number which is the length of a list of numbers

(define valid (cons 1 (cons 2 (cons 3 (cons 4 (cons 5(cons 9 (cons 8 (cons 7 (cons 6 empty))))))))))
(define valid-10 (cons 1 (cons 2 (cons 3 (cons 4 (cons 5(cons 9 (cons 8
                                                           (cons 7 (cons 6 (cons 10 empty)))))))))))

(define minNat 1)          ;;Lowest Nat
(define sudoku-length 9)   ;;Highest Nat for sudoku

;;Examples
(check-expect (sudoku-valid? valid) true)
(check-expect (sudoku-valid? (cons 1 valid)) false)
(check-expect (sudoku-valid? (cons 0 valid)) false)
(check-expect (spans-nat? valid-10 10) true)
(check-expect (spans-nat? (cons 1 valid-10) 10) false)
(check-expect (spans-nat? (cons 0 valid-10) 10) false)

;;(sudoku-valid? nLst) nLst -> Bool
(define (sudoku-valid? nLst)
  (cond[(empty? nLst) false]
       [(not (= (calc-length nLst) sudoku-length)) false]
       [else (find-number nLst nLst minNat sudoku-length)]))

;;(spans-nat? lon n) nLst + Nat -> Bool
(define (spans-nat? lon n)
  (cond[(empty? lon) false]
       [(not (= (calc-length lon) n)) false]
       [else (find-number lon lon minNat n)]))

;;(find-number nLst-leftover sub-nLst num max-num) nLst + nLst + Nat + Nat -> Bool
;;max-num>0
(define (find-number nLst-leftover sub-nLst num max-num)
  (cond[(= num (+ max-num 1)) true]    ;;Found all numbers
       [(empty? sub-nLst) false]       ;;Can't find number
       [(= (first sub-nLst) num)
        (find-number (annihilate-number nLst-leftover num)
                     (annihilate-number nLst-leftover num)
                     (+ num 1) max-num)]
       [else (find-number nLst-leftover (rest sub-nLst) num max-num)]))

;;(annihilate-number nLst num) nLst + Num -> nLst
(define (annihilate-number nLst num)
  (cond[(= (first nLst) num) (construct-rest-of-list (rest nLst))]
       [else (cons (first nLst) (annihilate-number (rest nLst) num))]))

;;(construct-rest-of-list nLst) nLst -> nLst
(define (construct-rest-of-list nLst)
  (cond[(empty? nLst) empty]
       [else (cons (first nLst) (construct-rest-of-list (rest nLst)))]))

;;(calc-length nLst) nLst -> Num
(define (calc-length nLst)
  (cond[(empty? (rest nLst)) 1]
       [else (+ 1 (calc-length (rest nLst)))]))

;;Tests
(check-expect (sudoku-valid? (cons 2 (cons 2 (cons 3 (cons 4 (cons 5(cons 9 (cons 8
                                               (cons 7 (cons 6 empty)))))))))) false)
(check-expect (sudoku-valid? empty) false)
(check-expect (spans-nat? empty 10) false)