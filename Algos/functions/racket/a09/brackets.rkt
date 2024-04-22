;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname brackets) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;;
;;/----------------------------------\
;;    Cameron Carmichael (20881375)
;;    CS 135 Fall 2020
;;    Assignment 09, Problem 2
;;\----------------------------------/
;;

;;Examples
(check-expect (balanced? "(<)>") false)
(check-expect (balanced? "((<>[])<>)[]") true)

;;(balanced? str) Str -> Bool
;;Requires: str consists only of the following characters:
;;             #\< #\> #\( #\) #\[ #\]
(define (balanced? str)
  (local[(define open-brkt (list #\< #\( #\[))
         (define cls-brkt (list #\> #\) #\]))
         (define brkt-map (map (lambda (x y) (list x y)) open-brkt cls-brkt))
         (define (balance still-open remaining)
           (cond[(and (empty? remaining) (empty? still-open)) true]
                [(empty? remaining) false]
                [(foldr (lambda (bkt rror) (or (char=? bkt (first remaining)) rror)) false open-brkt)
                 (balance (cons (first remaining) still-open) (rest remaining))]
                [(foldr (lambda (bkt rror)
                          (or (and (char=? (first still-open) (first bkt))
                                   (char=? (first remaining) (second bkt))) rror))
                        false brkt-map)
                 (balance (rest still-open) (rest remaining))]
                [else false]))]
    (balance empty (string->list str))))

;;Tests
(check-expect (balanced? "") true)
(check-expect (balanced? "()()(") false)