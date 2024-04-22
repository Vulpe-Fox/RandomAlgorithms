;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname vowels) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;;
;;/----------------------------------\
;;    Cameron Carmichael (20881375)
;;    CS 135 Fall 2020
;;    Assignment 03, Problem 2
;;\----------------------------------/
;;

;;(total-vowels sLst) produces the number of vowels in all the strings in a string list
;;(sum-vowels-in-list cLst) produces the number of vowels in a character list

;;Examples
(check-expect (total-vowels (cons "test" (cons "look" empty))) 3)

(define list-of-vowels (list #\a #\e #\i #\o #\u #\A #\E #\I #\O #\U)) ;;List of vowel characters

;;(total-vowels sLst) sLst -> Num
(define (total-vowels sLst)
  (cond[(empty? sLst) 0]
       [else (+ (sum-vowels-in-list (string->list (first sLst))) (total-vowels (rest sLst)))]))

;;(sum-vowels-in-list cLst) cLst -> Num
(define (sum-vowels-in-list cLst)
  (cond[(empty? cLst) 0]
       [(member? (first cLst) list-of-vowels) (add1 (sum-vowels-in-list (rest cLst)))]
       [else (sum-vowels-in-list (rest cLst))]))

;;Tests
(check-expect (total-vowels (cons "Atest" (cons "look" empty))) 4)
(check-expect (total-vowels (cons "tst" (cons "lk" empty))) 0)