;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname vector) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;;
;;/----------------------------------\
;;    Cameron Carmichael (20881375)
;;    CS 135 Fall 2020
;;    Assignment 04, Problem 2
;;\----------------------------------/
;;

;;(cos-between a b) produces the cos of an angle between the two vectors a and b
;;(unit-vector v) produces a list of unit vectors from a list of vectors
;;(calc-unit-vectors v norm) divides the values by the norm
;;(euclidean-norm v) produces the Euclidean Norm using some list of numbers comprising a vector v
;;(sum-of-squares natLst) produces the sum of squares of a list of Natural numbers
;;(sum-of-products nLst1 nLst2) produces the sum of the products of same degree vectors
;;   e.g. dot product of vectors

;;Examples
(check-within (euclidean-norm (list 3 4)) 5 0.01)

(check-within (unit-vector (list 3 4)) (list 0.6 0.8) 0.01)

(check-within (cos-between (list 3 4) (list 0 6)) 0.8 0.01)

;;(cos-between a b) nLst + nLst -> Num
;;Both a,b have the same size
(define (cos-between a b)
  (/ (sum-of-products a b) (* (euclidean-norm a) (euclidean-norm b))))

;;(unit-vector v) nLst -> nLst
(define (unit-vector v)
  (calc-unit-vectors v (euclidean-norm v)))

;;(calc-unit-vectors v norm) nLst + Num -> nLst
(define (calc-unit-vectors v norm)
  (cond[(empty? v) empty]
       [else (cons (/ (first v) norm) (calc-unit-vectors (rest v) norm))]))

;;(euclidean-norm v) nLst -> Num
(define (euclidean-norm v)
  (sqrt (sum-of-squares v)))

;;(sum-of-squares natLst) natLst -> Num
(define (sum-of-squares natLst)
  (cond[(empty? natLst) 0]
       [else (+ (sqr (first natLst)) (sum-of-squares (rest natLst)))]))

;;(sum-of-products nLst1 nLst2) nLst + nLst -> Num
;;Both nLst have the same size
(define (sum-of-products nLst1 nLst2)
  (cond[(empty? nLst1) 0]
       [else (+ (* (first nLst1) (first nLst2)) (sum-of-products (rest nLst1) (rest nLst2)))]))

;;Tests
(check-within (euclidean-norm (list 6)) 6 0.01)
(check-within (euclidean-norm (list 5 12)) 13 0.01)
(check-within (euclidean-norm (list 5 12 15)) 19.85 0.01)

(check-within (unit-vector (list 6)) (list 1) 0.01)
(check-within (unit-vector (list 5 12)) (list 0.38 0.92) 0.01)
(check-within (unit-vector (list 5 12 15)) (list 0.25 0.60 0.76) 0.01)

(check-within (cos-between (list 5 12) (list 5 12)) 1 0.01)
(check-within (cos-between (list 5 12) (list 3 4)) 0.97 0.01)