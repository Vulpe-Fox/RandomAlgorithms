;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname wild) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;;
;;/----------------------------------\
;;    Cameron Carmichael (20881375)
;;    CS 135 Fall 2020
;;    Midterm 1 Question 4
;;\----------------------------------/
;;

;;(waterloo-wildlife symLst) produces a list of symbols with 'emu and 'penguin removed

;;Examples
(check-expect (waterloo-wildlife (cons 'car (cons 'emu (cons 'tree empty))))
              (cons 'car (cons 'tree empty)))

;;(waterloo-wildlife symLst) symLst -> symLst
(define (waterloo-wildlife symLst)
  (cond[(empty? symLst) empty]
       [(or (equal? (first symLst) 'emu) (equal? (first symLst) 'penguin))
        (waterloo-wildlife (rest symLst))]
       [else (cons (first symLst) (waterloo-wildlife (rest symLst)))]))

;;Tests
(check-expect (waterloo-wildlife (cons 'car (cons 'emu (cons 'penguin (cons 'tree empty)))))
              (cons 'car (cons 'tree empty)))

(check-expect (waterloo-wildlife (cons 'emu (cons 'penguin empty)))
              empty)