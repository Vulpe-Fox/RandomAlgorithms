;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname mean-relative) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;;
;;/----------------------------------\
;;    Cameron Carmichael (20881375)
;;    CS 135 Fall 2020
;;    Assignment 02, Problem 6
;;\----------------------------------/
;;

;;(mean-relative nLst) produces a list of symbols; an sLst:
;;   If num is below mean of nLst -> 'below-mean
;;   If num is equal to mean of nLst -> 'mean
;;   If num is above mean of nLst -> 'above-mean
;;(z-scores nLst-full nLst-partial) produces a list of z-scores for a list of numbers
;;   Can not test due to inexact number calculations
;;(std-dev nLst mean) produces the standard deviation for a list of numbers
;;(square-of-difference nLst mean) produces a list of the squares of the difference of each
;;   number and the mean
;;(numbers->symbols nLst mean) changes a list of numbers to the list of symbols above
;;   by comparing to some mean
;;(calc-mean nLst) produces a number which is the mean of a list of numbers
;;(calc-sum nLst) produces a number which is the sum of a list of numbers
;;(calc-length nLst) produces a number which is the length of a list of numbers

;;Examples
(check-expect (mean-relative (cons 5 (cons 7 (cons 9 (cons 12 empty)))))
              (cons 'below-mean (cons 'below-mean (cons 'above-mean (cons 'above-mean empty)))))

;;(mean-relative nLst) nLst -> sLst
;;nLst must be non-empty
(define (mean-relative nLst)
  (numbers->symbols nLst (calc-mean nLst)))

;;(z-scores nLst-full nLst-partial) nLst + nLst -> nLst
;;nLst-full must be non-empty
(define (z-scores nLst-full nLst-partial)
  (cond[(empty? nLst-partial) empty]
       [else (cons (/ (- (first nLst-partial) (calc-mean nLst-full))
                      (std-dev nLst-full (calc-mean nLst-full)))
                   (z-scores nLst-full (rest nLst-partial)))]))

;;(std-dev nLst mean) nLst + Num -> Num
(define (std-dev nLst mean)
  (sqrt (calc-mean (square-of-difference nLst mean))))


;;(square-of-difference nLst mean) nLst + Num -> nLst
(define (square-of-difference nLst mean)
  (cond[(empty? nLst) empty]
       [else (cons (sqr (- (first nLst) mean)) (square-of-difference (rest nLst) mean))]))

;;(numbers->symbols nLst mean) nLst + Num -> sLst
(define (numbers->symbols nLst mean)
  (cond[(empty? nLst) empty]
       [(< (first nLst) mean) (cons 'below-mean (numbers->symbols (rest nLst) mean))]
       [(= (first nLst) mean) (cons 'mean (numbers->symbols (rest nLst) mean))]
       [(> (first nLst) mean) (cons 'above-mean (numbers->symbols (rest nLst) mean))]))

;;(calc-mean nLst) nLst -> Num
(define (calc-mean nLst)
  (/ (calc-sum nLst) (calc-length nLst)))

;;(calc-sum nLst) nLst -> Num
(define (calc-sum nLst)
  (cond[(empty? (rest nLst)) (first nLst)]
       [else (+ (first nLst) (calc-sum (rest nLst)))]))

;;(calc-length nLst) nLst -> Num
(define (calc-length nLst)
  (cond[(empty? (rest nLst)) 1]
       [else (+ 1 (calc-length (rest nLst)))]))

;;Tests
(check-expect (mean-relative (cons 5 (cons 5 (cons 5 (cons 5 empty)))))
              (cons 'mean (cons 'mean (cons 'mean (cons 'mean empty)))))