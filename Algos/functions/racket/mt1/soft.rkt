;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname soft) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;;
;;/----------------------------------\
;;    Cameron Carmichael (20881375)
;;    CS 135 Fall 2020
;;    Midterm 1 Question 5
;;\----------------------------------/
;;

;;(softmax lst) produces a list of the softmax of each element in a non-empty list of numbers
;;(apply-norm norm lst) produces a list of numbers which are e to the number divided by the
;;   normalization factor, norm
;;(compute-norm lst) produces the sum of numbers of e to the exponent of each number in a list
;;   called the normalization factor

;;Test variables
(define test0 (cons 1.0 (cons 2.0 (cons -1.0 (cons 3.0 empty)))))
(define test1 (cons 2.0 (cons 3.0 (cons -5.0 (cons 3.0 empty)))))
(define test2 (cons 1.0 (cons 6.0 (cons -1.0 (cons 5.0 empty)))))

;;Examples
(check-within (compute-norm test0) 30.56 0.01)

(check-within (first (apply-norm 30.56 test0)) 0.09 0.01)
(check-within (second (apply-norm 30.56 test0)) 0.24 0.01)

(check-within (first (softmax test0)) 0.09 0.01)
(check-within (second (softmax test0)) 0.24 0.01)

;;(softmax lst) nLst -> nLst
;;lst is non-empty
(define (softmax lst)
  (apply-norm (compute-norm lst) lst))

;;(apply-norm norm lst) Num + nLst -> nLst
;;lst is non-empty, norm > 0
(define (apply-norm norm lst)
  (cond[(empty? lst) empty]
       [else (cons (/ (exp (first lst)) norm) (apply-norm norm (rest lst)))]))

;;(compute-norm lst) nLst -> Num
(define (compute-norm lst)
  (cond[(empty? lst) 0]
       [else (+ (exp (first lst)) (compute-norm (rest lst)))]))

;;Tests
(check-within (compute-norm test1) 47.57 0.01)
(check-within (compute-norm test2) 554.93 0.01)

(check-within (first (apply-norm 47.57 test1)) 0.16 0.01)
(check-within (second (apply-norm 47.57 test1)) 0.42 0.01)
(check-within (first (apply-norm 554.93 test2)) 0 0.01)
(check-within (second (apply-norm 554.93 test2)) 0.72 0.01)

(check-within (first (softmax test1)) 0.16 0.01)
(check-within (second (softmax test1)) 0.42 0.01)
(check-within (first (softmax test2)) 0 0.01)
(check-within (second (softmax test2)) 0.72 0.01)