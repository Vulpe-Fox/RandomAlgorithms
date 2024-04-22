;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname count) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;
;;/----------------------------------\
;;    Cameron Carmichael (20881375)
;;    CS 135 Fall 2020
;;    Exam Question 5
;;\----------------------------------/
;;

;; A (nested-listof X) is one of:
;; * empty
;; * (cons X (nested-listof X))
;; * (cons (nested-listof X) (nested listof X))

;;(count-starting-with c nest) Produces the number of lists in nest that contain at least one string 
;;   starting with the given character c

;;Examples
(check-expect (count-starting-with #\9 empty) 0)
(check-expect (count-starting-with #\9 '("9" "99")) 1)
(check-expect (count-starting-with #\a '("alpha" ("able") ("gamma" ("aaa")))) 3)

;;count-starting-with: Char + (nested-listof Str) -> Num
(define (count-starting-with c nest)
  (cond[(empty? nest) 0]
       [else (local[(define within-lists (foldr (lambda (x rror) (cond[(list? x) (cons x rror)]
                                                                      [else rror]))
                                                '() nest))
                    (define within-strings (foldr (lambda (x rror) (cond[(list? x) rror]
                                                                        [else (cons x rror)]))
                                                  '() nest))
                    (define sum-of-count-within
                      (foldr (lambda (x rror) (+ (count-starting-with c x) rror)) 0 within-lists))
                    (define starts-with-c?
                      (foldr (lambda (x rror) (cond[(char=? (first (string->list x)) c) true]
                                                   [else rror]))
                             false within-strings))]
               (cond[starts-with-c? (add1 sum-of-count-within)]
                    [else sum-of-count-within]))]))

;;Tests
(check-expect (count-starting-with #\a '("gloria" ("septum") ("gamma" ("aaa")))) 1)
(check-expect (count-starting-with #\a '("gloria" () ("gamma" ("eerie")))) 0)