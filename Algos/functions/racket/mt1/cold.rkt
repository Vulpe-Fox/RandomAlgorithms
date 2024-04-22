;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname cold) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;;
;;/----------------------------------\
;;    Cameron Carmichael (20881375)
;;    CS 135 Fall 2020
;;    Midterm 1 Question 6
;;\----------------------------------/
;;

;;(weather nLst) produces a symbol using a list of temperature highs:
;;   'happy if any of the highs are above 20 but not below 8
;;   'sad if any of the highs are below 8
;;   'okay otherwise
;;(max-num current-max nLst) produces the highest number in a list of numbers
;;(min-num current-min nLst) produces the lowest number in a list of numbers

;;Test variables
(define test0 (cons 15.0 (cons 22.0 (cons -273.15 (cons 20.0 empty)))))
(define test1 (cons 8.0 (cons 12.0 (cons 35.15 (cons 9.0 empty)))))
(define test2 (cons 8.0 (cons 12.0 (cons 15.0 (cons 9.0 empty)))))
(define test3 (cons -100.0 (cons 600.0 (cons 35.15 (cons 9.0 empty)))))

;;Examples
(check-expect (weather test0) 'sad)
(check-expect (weather test1) 'happy)

;;(weather nLst) nLst -> Sym
;;nLst not empty
(define (weather nLst)
  (cond[(< (min-num (first nLst) (rest nLst)) 8) 'sad]
       [(> (max-num (first nLst) (rest nLst)) 20) 'happy]
       [else 'okay]))

;;(max-num current-max nLst) Num + nLst -> Num
;;nLst not empty
(define (max-num current-max nLst)
  (cond[(empty? nLst) current-max]
       [(> current-max (first nLst)) (max-num current-max (rest nLst))]
       [else (max-num (first nLst) (rest nLst))]))

;;(min-num current-min nLst) Num + nLst -> Num
;;nLst not empty
(define (min-num current-min nLst)
  (cond[(empty? nLst) current-min]
       [(< current-min (first nLst)) (min-num current-min (rest nLst))]
       [else (min-num (first nLst) (rest nLst))]))

;;Tests
(check-expect (max-num (first test0) (rest test0)) 22)
(check-expect (max-num (first test1) (rest test1)) 35.15)
(check-expect (max-num (first test2) (rest test2)) 15)
(check-expect (max-num (first test3) (rest test3)) 600)

(check-expect (min-num (first test0) (rest test0)) -273.15)
(check-expect (min-num (first test1) (rest test1)) 8.0)
(check-expect (min-num (first test2) (rest test2)) 8.0)
(check-expect (min-num (first test3) (rest test3)) -100.0)

(check-expect (weather test2) 'okay)
(check-expect (weather test3) 'sad)