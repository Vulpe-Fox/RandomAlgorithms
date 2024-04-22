;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname grades) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;;
;;/----------------------------------\
;;    Cameron Carmichael (20881375)
;;    CS 135 Fall 2020
;;    Assignment 02, Problem 3
;;\----------------------------------/
;;

;;(cs135-grade self-check assign mt1 mt2 final) produces the net course mark
;;(normal-calc self-check assign mt1 mt2 final) produces the precondition course mark
;;(exam-mark self-check assign mt1 mt2 final) produces the exam mark

;;Examples
(check-expect (cs135-grade 0.8 0.6 0.4 0.9 0.95) 68.3)

;;(cs135-grade self-check assign mt1 mt2 final)
;;Num + Num + Num + Num + Num -> Num
;;self-check,assign,mt1,mt2,final >= 0
(define (cs135-grade self-check assign mt1 mt2 final)
  (cond[(or (< (exam-mark mt1 mt2 final) 50) (< assign 0.50))
        (cond[(< (normal-calc self-check assign mt1 mt2 final) 46)
                 (normal-calc self-check assign mt1 mt2 final)]
             [else 46])]
       [else (normal-calc self-check assign mt1 mt2 final)]))


;;(normal-calc self-check assign mt1 mt2 final)
;;Num + Num + Num + Num + Num -> Num
(define (normal-calc self-check assign mt1 mt2 final)
  (* (+ (* self-check 0.10) (* assign 0.60) (* (+ mt1 mt2) 0.07) (* final 0.16)) 100))

;;(exam-mark self-check assign mt1 mt2 final)
;;Num + Num + Num + Num + Num -> Num+
(define (exam-mark mt1 mt2 final)
  (* (/ (+ (* mt1 0.07) (* mt2 0.07) (* final 0.16)) 0.30) 100))

;;Tests
(check-expect (cs135-grade 0.8 0.6 0.4 0.9 0.1) 46)
(check-expect (cs135-grade 0.8 0.2 0.2 0.9 0.6) 37.3)