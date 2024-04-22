;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname future-b) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;
;;/----------------------------------\
;;    Cameron Carmichael (20881375)
;;    CS 135 Fall 2020
;;    Exam Question 2b
;;\----------------------------------/
;;

;; A Substitution Cipher (SCipher) is a (listof (list Char Char))
;; Requires: The first Char in each pair (the key) is unique

;;(substitute str cipher) produces a new string where each character which is a key of cipher
;;   is replaced by its corresponding value, where cipher is an SCipher
;;(reverse-cipher cipher) Reverses the substitutions of the first, but if two different keys
;;   correspond to the same value, instead produces false

;;Test Variables
(define vowel-shift '((#\i #\o) (#\e #\i) (#\a #\e)
                      (#\o #\u) (#\u #\y) (#\y #\a)))

;;Examples
(check-expect (substitute "I, plain text!9" vowel-shift) "I, pleon tixt!9")

(check-expect (reverse-cipher vowel-shift)
              '((#\o #\i) (#\i #\e) (#\e #\a)
                (#\u #\o) (#\y #\u) (#\a #\y)))
(check-expect (reverse-cipher '((#\a #\b) (#\c #\b))) false)

;;substitute: Str + SCipher -> Str
(define (substitute str cipher)
  (local[(define chars (string->list str))]
    (list->string (foldr (lambda (char rror)
                           (cons (foldr (lambda (cipher-pair rror)
                                          (cond[(char=? char (first cipher-pair))
                                                (second cipher-pair)]
                                               [else rror]))
                                        char cipher)
                                 rror))
                         '() chars))))

;;reverse-cipher: SCipher -> (Anyof Bool SCipher)
(define (reverse-cipher cipher)
  (local[(define cipher-vals
           (foldr (lambda (x rror) (cons (second x) rror)) '() cipher))
         (define sorted-vals (quicksort cipher-vals char<=?))
         (define (nopairs? vals)
           (cond[(or (empty? vals) (empty? (rest vals))) true]
                [(char=? (first vals) (second vals)) false]
                [else (nopairs? (rest vals))]))]
    (cond[(nopairs? sorted-vals)
          (foldr (lambda (x rror) (cons (list (second x) (first x)) rror)) '() cipher)]
         [else false])))

;;Tests
(check-expect (substitute "" vowel-shift) "")
(check-expect (substitute "aeiouy" vowel-shift) "eiouya")
(check-expect (substitute "Tsktsk" vowel-shift) "Tsktsk")