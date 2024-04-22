;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname redux) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;;
;;/----------------------------------\
;;    Cameron Carmichael (20881375)
;;    CS 135 Fall 2020
;;    Assignment 08, Problem 2
;;\----------------------------------/
;;

;;(parity str) produces a symbol as to whether the number of 1's in a string is even or odd
;;(replace-word str repl-str lst) Replaces all the strings in a list of strings which match str
;;   to repl-str
;;(all-factors n) Generates a list of all natural numbers x where 0 < x < n and x divides n evenly
;;(mean-relative nlst) Generates a list which compares each number in nlst to the mean of nlst
;;   returns the symbol 'above-mean if above mean, 'mean if equal to mean, 'below-mean otherwise

;;Examples
(check-expect (parity "110101") 'even)
(check-expect (parity "1110011") 'odd)

(check-expect (replace-word "exam" "assessment" '("content" "exam" "assignment"))
              '("content" "assessment" "assignment"))

(check-expect (all-factors 30) '(1 2 3 5 6 10 15))

(check-expect (mean-relative '(5 7 9 12))'(below-mean below-mean above-mean above-mean))

;;(parity str) Str -> Sym
(define (parity str)
  (cond[(odd? (length (filter (lambda (char) (char=? #\1 char))
                              (string->list str)))) 'odd]
       [else 'even]))

;;(replace-word str repl-str lst) Str + Str + (Listof Str) -> (Listof Str)
(define (replace-word str repl-str lst)
  (map (lambda (curr-str) (cond[(equal? curr-str str) repl-str]
                               [else curr-str])) lst))

;;(all-factors n) Nat -> (Listof Nat)
;;Requires: n is non-negative
(define (all-factors n)
  (filter (lambda (num) (not (zero? num)))
          (build-list n (lambda (num) (cond[(zero? num) 0]
                                           [(= (remainder n num) 0) num]
                                           [else 0])))))

;;(mean-relative nlst) (Listof Num) -> (Listof Sym)
;;Requires: nlst must be non-empty
(define (mean-relative nlst)
  (local[(define total (/ (foldr + 0 nlst) (length nlst)))]
    (map (lambda (num) (cond[(> num total) 'above-mean]
                       [(= num total) 'mean]
                       [else 'below-mean])) nlst)))

;;Tests
(check-expect (parity "") 'even)
(check-expect (parity "1") 'odd)
(check-expect (parity "0") 'even)

(check-expect (replace-word "exam" "assessment" '("exam" "exam" "exam"))
              '("assessment" "assessment" "assessment"))
(check-expect (replace-word "nexam" "assessment" '("exam" "exam" "exam"))
              '("exam" "exam" "exam"))
(check-expect (replace-word "nexam" "assessment" '())
              '())

(check-expect (all-factors 0) '())
(check-expect (all-factors 1) '())
(check-expect (all-factors 5) '(1))

(check-expect (mean-relative '(7 7))'(mean mean))
(check-expect (mean-relative '(7 9 8))'(below-mean above-mean mean))