;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname recipe-needed) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;
;;/----------------------------------\
;;    Cameron Carmichael (20881375)
;;    CS 135 Fall 2020
;;    Assignment 09, Recipe
;;\----------------------------------/
;;

;;(m1 a) Produces true when the string is a palendrome; false otherwise
;;(m2 a) Produces a predicate which returns true when a string is equal to any of the list of
;;   strings in 'a' after their individual characters are sorted lexicographically; false otherwise
;;(m3 a b) Produces a value based on the output of the function a on the difference between the
;;   highest and lowest integer values in a list of any element.

;;Examples
(check-expect (m1 "abca") false)
(check-expect (m1 "abba") true)

(check-expect ((m2 '("defintely" "alpha")) "aahlp") true)
(check-expect ((m2 '("defintely" "alpha")) "alpha") false)

(check-expect (m3 + '(6 e a 4 2)) 4)
(check-expect (m3 + '(6 e a)) 0)

;;(m1 a) String -> Bool
(define (m1 a)
  (local
    [(define b (string->list a))]
    (foldr
     (lambda (x y z)
       (and (char=? x y) z))
     true
     b
     (foldl cons empty b))))

;;(m2 a) (Listof String) -> (String -> Bool)
(define (m2 a)
  (lambda (s)
    (local
      [(define (r x) (quicksort (string->list x) char<?))
       (define u (map (lambda (x) (list x (r x))) a))
       (define (t x) (foldr (lambda (y z) (add1 z)) 0 x))]
      (foldr
       (lambda (x y)
         (cond
           [(string=? s (first x)) y]
           [(not (= (t (r s)) (t (second x)))) y]
           [else
            (or y (foldr
                   (lambda (x y)
                     (cond
                       [(and x y) y]
                       [else false]))
                   true
                   (map (lambda (a b) (char=? a b)) (r s) (second x))))]))
       false
       u))))

;;(m3 a b) (Int -> Any) + (Listof Any) -> Any
;;Requires: At least one element of b is an integer
(define (m3 a b)
   (local
     [(define c (filter integer? b))
      (define (d e f)
        (foldl (lambda (x y)
                 (cond
                   [(f x y) x]
                   [else y]))
               (first e)
               e))]
     (a (- (d c >) (d c <)))))

;;Tests
(check-expect (m1 "") true)

(check-expect ((m2 '("defintely" "alpha")) "aahlt") false)
(check-expect ((m2 '("defintely" "alpha")) "aahlpz") false)

(check-expect (m3 sqr '(6 4 2)) 16)
(check-expect (m3 sqr '(6 e a)) 0)
(check-expect (m3 + '(0)) 0)

