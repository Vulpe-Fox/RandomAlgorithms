;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname warmup) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;;
;;/----------------------------------\
;;    Cameron Carmichael (20881375)
;;    CS 135 Fall 2020
;;    Assignment 09, Problem 1
;;\----------------------------------/
;;

;;(function-go-round fn-list data-list) Produces a list of elements which are elements in data-list  
;;   operated by functions in fn-list. Specifically, the n mod (length fn-list) element will be 
;;   operated on by the given function at position n mod (length fn-list).

;;Examples
(check-expect (flip-case '("Mr" "Goose!")) '("Mr" "Goose!"))
(check-expect(flip-case '("OnLiNE" "ClAsSEs" "ArE" "sOo" "mUcH" "FuN!"))
             '("OnLiNE" "ClAsSEs" "are" "SOO" "MUCH" "fun!"))

(check-expect(function-go-round (list string-length string-upcase
                                      (lambda(x) (string-append x "!!")))
                                '("joy" "anger" "disgust"  "sadness" "fear"))
             '(3 "ANGER" "disgust!!" 7 "FEAR"))
(check-expect(function-go-round
              (list even? odd? add1 (lambda(x) (> 3 x)) even?) '(8 9 2))
             (list true true 3))

;;(flip-case slst) (Listof Str) -> (Listof Str)
(define (flip-case slst)
  (local[(define (recursive-flip length1 length2 slst)
           (cond[(empty? slst) empty]
                [(even? (+ length1 length2))
                 (cons (string-upcase (first slst))
                       (recursive-flip length2 (string-length (first slst)) (rest slst)))]
                [else (cons (string-downcase (first slst))
                            (recursive-flip length2 (string-length (first slst)) (rest slst)))]))]
    (cond[(<= (length slst) 2) slst]
         [else (cons (first slst)
                     (cons (second slst) (recursive-flip (string-length (first slst))
                                                 (string-length (second slst))
                                                 (rest (rest slst)))))])))

;;(function-go-round fn-list data-list) (listof (X->Y)) + (Listof X) -> (Listof Y)
;;Requires: Each n mod (length fn-list) element of data-list must be able to be operated on
;;           by the related function in fn-list
(define (function-go-round fn-list data-list)
  (local[(define next-fn-list (foldr cons (list (first fn-list)) (rest fn-list)))]
    (cond[(empty? data-list) empty]
         [else (cons ((first fn-list) (first data-list))
                     (function-go-round next-fn-list (rest data-list)))])))

;;Tests
(check-expect(flip-case '("OnLiNE" "ClAsSEs" "ArE" "" "mUcH" "FuN!"))
             '("OnLiNE" "ClAsSEs" "are" "" "much" "FUN!"))
(check-expect (flip-case '("Mr")) '("Mr"))
(check-expect (flip-case '()) '())

(check-expect(function-go-round (list string-length string-upcase
                                      (lambda(x) (string-append x "!!")))
                                '())
             '())
(check-expect(function-go-round (list string-length string?
                                      (lambda(x) (string-append x "!!")))
                                '("joy" "anger" "disgust"  "sadness" 45))
             '(3 #true "disgust!!" 7 #false))