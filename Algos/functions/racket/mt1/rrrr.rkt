;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname rrrr) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;;
;;/----------------------------------\
;;    Cameron Carmichael (20881375)
;;    CS 135 Fall 2020
;;    Midterm 1 Question 7
;;\----------------------------------/
;;

;;(string->pirate String) replaces all instances of a with arrrr and A with Arrrr
;;(sLst->pirateLst sLst) adds four instances of r after each a or A

;;Examples
(check-expect (string->pirate "na") "narrrr")
(check-expect (string->pirate "Waterloo") "Warrrrterloo")
(check-expect (string->pirate "Aardvark") "Arrrrarrrrrdvarrrrrk")

;;(string->pirate String) String -> String
(define (string->pirate String)
  (list->string (sLst->pirateLst (string->list String))))

;;(sLst->pirateLst sLst) sLst -> sLst
(define (sLst->pirateLst sLst)
  (cond[(empty? sLst) empty]
       [(equal? (first sLst) #\a)
        (cons #\a (cons #\r (cons #\r (cons #\r (cons #\r (sLst->pirateLst (rest sLst)))))))]
       [(equal? (first sLst) #\A)
        (cons #\A (cons #\r (cons #\r (cons #\r (cons #\r (sLst->pirateLst (rest sLst)))))))]
       [else (cons (first sLst) (sLst->pirateLst (rest sLst)))]))

;;Tests
(check-expect (string->pirate "Cameron") "Carrrrmeron")
(check-expect (string->pirate "Carmichael") "Carrrrrmicharrrrel")