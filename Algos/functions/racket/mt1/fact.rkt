;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname fact) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;;
;;/----------------------------------\
;;    Cameron Carmichael (20881375)
;;    CS 135 Fall 2020
;;    Midterm 1 Question 2
;;\----------------------------------/
;;

;;(factorialize nLst) produces a list of factorial numbers from a list of numbers
;;(factorial-recurse num) produces the factorial of a number

;;Examples
(check-expect (factorialize (cons 1 (cons 2 (cons 4 (cons 3 empty)))))
              (cons 1 (cons 2 (cons 24 (cons 6 empty)))))

;;(factorialize nLst) nLst -> nLst
(define (factorialize nLst)
  (cond[(empty? nLst) empty]
       [else (cons (factorial-recurse (first nLst)) (factorialize (rest nLst)))]))

;;(factorial-recurse num) Num -> Num
;;num >= 0
(define (factorial-recurse num)
  (cond[(or (= num 0) (= num 1)) 1]
       [else (* num (factorial-recurse (- num 1)))]))

;;Tests
(check-expect (factorialize empty)
              empty)
(check-expect (factorialize (cons 1 (cons 2 (cons 3 (cons 5 empty)))))
              (cons 1 (cons 2 (cons 6 (cons 120 empty)))))