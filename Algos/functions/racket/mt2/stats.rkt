;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname stats) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;
;;/----------------------------------\
;;    Cameron Carmichael (20881375)
;;    CS 135 Fall 2020
;;    Midterm 2 Question 7
;;\----------------------------------/
;;

;;Middle term of an odd number of items is at position (n-1)/2, counting from 0
;;  Example, middle of 5 terms: 1 2 (3) 4 5, is at position 2 ((5-1)/2)
;;First middle term of an even number of items is at position (n/2)-1, counting from 0
;;  Example, first middle term of 6 terms: 1 2 (3 4) 5 6, is at position 2 ((6/2)-1)

;;(median numbers) produces the median of a list of numbers
;;(median-sorted numbers) produces the median of a sorted list of numbers
;;(insertion-sort numbers) produces a sorted list of numbers increasing numerically
;;(insert current-number numbers) produces a list of numbers such that current-number is placed in the
;;   correct position in numbers such that the list is increasing numerically
;;(find-term numbers position) produces the term at a position in a sorted list of numbers
;;(find-avg-subseq-term numbers position) produces the avergge of a term at a position and the next\
;;   term in a sorted list of numbers

;;Examples
(check-expect (median (list 9 32.4 -2 6 28 22 129332)) 22)
(check-expect (median (list 7 827 -23.2 -300)) -8.1)
(check-expect (median (list 7 7 7 7 7 89)) 7)

;;(median numbers) (listof Num) -> Num
(define (median numbers)
  (cond[(empty? numbers) 0]
       [else (median-sorted (insertion-sort numbers))]))

;;(median-sorted numbers) (listof Num) -> Num
(define (median-sorted numbers)
  (cond[(even? (length numbers))
        (find-avg-subseq-term numbers (sub1 (/ (length numbers) 2)))] ;;First middle term of an even
       [else (find-term numbers (/ (sub1 (length numbers)) 2))]))     ;;Middle term of an odd

;;(insertion-sort numbers) (listof Num) -> (listof Num)
(define (insertion-sort numbers)
  (cond[(empty? numbers) empty]
       [else (insert (first numbers) (insertion-sort (rest numbers)))]))

;;(insert current-number numbers) Num + (listof Num) -> (listof Num)
(define (insert current-number numbers)
  (cond[(empty? numbers) (cons current-number empty)]
       [(< current-number (first numbers)) (cons current-number numbers)]
       [else (cons (first numbers) (insert current-number (rest numbers)))]))

;;(find-term numbers position) (listof Num) + Num -> Num
(define (find-term numbers position)
  (cond[(zero? position) (first numbers)]
       [else (find-term (rest numbers) (sub1 position))]))

;;(find-avg-subseq-term numbers position) (listof Num) + Num -> Num
(define (find-avg-subseq-term numbers position)
  (cond[(zero? position) (/ (+ (first numbers) (first (rest numbers))) 2)]
       [else (find-avg-subseq-term (rest numbers) (sub1 position))]))

;;Tests
(check-expect (median empty) 0)
(check-expect (median (list 7 7 7 7 7)) 7)