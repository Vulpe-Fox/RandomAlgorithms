;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname sillystring) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;;
;;/----------------------------------\
;;    Cameron Carmichael (20881375)
;;    CS 135 Fall 2020
;;    Assignment 05, Problem 3
;;\----------------------------------/
;;

(define-struct silly-string (first middle last))
;; A SillyStringStruct is a (make-silly-string Char SillyStr Char)

;; A SillyStr is one of:
;; * empty
;; * a Char
;; * a SillyStringStruct

;;(sillify s) produces the corresponding SillyStr for a Str
;;(find-last-character clst) produces the last character of a string's character list
;;(make-middle-terms-list clst) produces the all terms but the last of a string's character list
;;(unsillify ss) produces the corresponding Str for a SillyStr
;;(unsillify-list ss) produces the list of characters for unsillify to make a string out of
;;(palindrome? ss) produces a boolean regarding whether or not a SillyStr is a palindrome

;;Examples
(check-expect (sillify "Babbage")
              (make-silly-string
               #\B
               (make-silly-string
                #\a
                (make-silly-string
                 #\b
                 #\b
                 #\a)
                #\g)
               #\e))
(check-expect (sillify "Lovelace")
              (make-silly-string
               #\L
               (make-silly-string
                #\o
                (make-silly-string
                 #\v
                 (make-silly-string
                  #\e
                  empty
                  #\l)
                 #\a)
                #\c)
               #\e))

(check-expect (unsillify
               (make-silly-string
                #\L
                (make-silly-string
                 #\o
                 (make-silly-string
                  #\v
                  (make-silly-string
                  #\e
                  empty
                  #\l)
                  #\a)
                 #\c)
                #\e))
              "Lovelace")

(check-expect (palindrome?
               (make-silly-string
                #\r
                (make-silly-string #\a #\d #\a)
                #\r)) true)
(check-expect (palindrome?
               (make-silly-string
                #\s
                (make-silly-string #\o #\n #\a)
                #\r)) false)
(check-expect (palindrome? (sillify "racecar")) true)
(check-expect (palindrome? (sillify "Koenigsegg")) false)

;;Functions

;;(sillify s) Str -> SillyStr
;;String must be non-empty and at least length 2
(define (sillify s)
  (cond[(empty? (make-middle-terms-list (rest (string->list s))))
        (make-silly-string (first (string->list s))
                           empty
                           (find-last-character (string->list s)))]
       [(= 1 (length (make-middle-terms-list (rest (string->list s)))))
        (make-silly-string (first (string->list s))
                           (first (make-middle-terms-list (string->list s)))
                           (find-last-character (string->list s)))]
       [else (make-silly-string (first (string->list s))
                           (sillify (list->string (make-middle-terms-list (rest (string->list s)))))
                           (find-last-character (string->list s)))]))

;;(find-last-character clst) clst -> char
(define (find-last-character clst)
  (cond[(empty? (rest clst)) (first clst)]
       [else (find-last-character (rest clst))]))

;;(make-middle-terms-list clst) clst -> clst
(define (make-middle-terms-list clst)
  (cond[(empty? (rest clst)) empty]
       [else (cons (first clst) (make-middle-terms-list (rest clst)))]))

;;(unsillify ss) SillyStr -> Str
(define (unsillify ss)
 (list->string (unsillify-list ss)))

;;(unsillify-list ss) SillyStr -> clst
(define (unsillify-list ss)
  (cond[(silly-string? (silly-string-middle ss))
        (append (list (silly-string-first ss))
              (unsillify-list (silly-string-middle ss))
              (list (silly-string-last ss)))]
       [(char? (silly-string-middle ss))
        (list (silly-string-first ss)
              (silly-string-middle ss)
              (silly-string-last ss))]
       [else (list (silly-string-first ss)(silly-string-last ss))]))

;;(palindrome? ss) SillyStr -> Bool
(define (palindrome? ss)
  (cond[(and (not (silly-string? (silly-string-middle ss)))
             (char=? (silly-string-first ss) (silly-string-last ss)))
        true]
       [(char=? (silly-string-first ss) (silly-string-last ss))
        (palindrome? (silly-string-middle ss))]
       [else false]))

;;Tests
(check-expect (sillify "ad")
              (make-silly-string
               #\a
               empty
               #\d))
(check-expect (sillify "Bagels")
              (make-silly-string
               #\B
               (make-silly-string
                #\a
                (make-silly-string
                 #\g
                 empty
                 #\e)
                #\l)
               #\s))

(check-expect (unsillify
               (make-silly-string
               #\B
               (make-silly-string
                #\a
                (make-silly-string
                 #\b
                 #\b
                 #\a)
                #\g)
               #\e))
              "Babbage")
(check-expect (unsillify
               (make-silly-string
               #\B
               (make-silly-string
                #\a
                (make-silly-string
                 #\g
                 empty
                 #\e)
                #\l)
               #\s))
              "Bagels")

(check-expect (palindrome? (sillify "reallaer")) true)
(check-expect (palindrome? (sillify "realaer")) true)
(check-expect (palindrome? (sillify "Bagels")) false)