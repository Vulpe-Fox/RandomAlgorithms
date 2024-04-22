;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname future-a) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;
;;/----------------------------------\
;;    Cameron Carmichael (20881375)
;;    CS 135 Fall 2020
;;    Exam Question 2a
;;\----------------------------------/
;;

;;(hamming-distance lst1 lst2) produces the hamming distance between two lists

;;Test Variables
(define gene1 '(t a g a a g t t t))
(define gene2 '(t a g a a g g t t))
(define packet-a '(b a b a b a a a b a a a))
(define packet-b '(b a b a b b a a b a b b))
(define binary-a '(1 0 0 1))
(define binary-b '(1 0 1 1))

;;Examples
(check-expect (hamming-distance gene1 gene2) 1)
(check-expect (hamming-distance packet-a packet-b) 3)

;;hamming-distnace: (Listof Any) + (Listof Any) -> Nat
;;Requires: lst1 and lst2 are of the same size
(define (hamming-distance lst1 lst2)
  (foldr (lambda (x rror) (cond[(equal? (first x) (second x)) rror]
                               [else (add1 rror)]))
         0 (map (lambda (x y) (list x y)) lst1 lst2)))

;;Tests
(check-expect (hamming-distance binary-a binary-b) 1)
(check-expect (hamming-distance binary-a binary-a) 0)
(check-expect (hamming-distance '(1) '(0)) 1)
(check-expect (hamming-distance '() '()) 0)