;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname cipher) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;
;;/----------------------------------\
;;    Cameron Carmichael (20881375)
;;    CS 135 Fall 2020
;;    Midterm 2 Question 5
;;\----------------------------------/
;;

;;A Substituttion Cipher (SCipher) is a (listof (list Char Char))
;;Requires: The first Char in each pair (the key) is unique

;;SCiphers:
(define vowel-shift '((#\a #\e) (#\e #\i) (#\i #\o)
                      (#\o #\u) (#\u #\y) (#\y #\a)))

;;(substitute string cipher) applies the SCipher cipher on the string and produces
;;   the result
;;(substitute-from-list charList cipher) applies the SCipher to a list of characters
;;   and produces the result
;;(replace-char char cipher) applies the SCipher to a single Char

;;Examples
(check-expect (substitute "I, plain text!9" vowel-shift) "I, pleon tixt!9")

;;(substitute string cipher) Str + SCipher -> Str
(define (substitute string cipher)
  (list->string (substitute-from-list (string->list string) cipher)))

;;(substitute-from-list charList cipher) (listof Char) + SCipher -> (listof Char)
(define (substitute-from-list charList cipher)
  (cond[(empty? charList) empty]
       [else (cons (replace-char (first charList) cipher)
                   (substitute-from-list (rest charList) cipher))]))

;;(replace-char char cipher) Char + SCipher -> Char
(define (replace-char char cipher)
  (cond[(empty? cipher) char]
       [(char=? char (first (first cipher))) (second (first cipher))]
       [else (replace-char char (rest cipher))]))

;;Tests
(check-expect (substitute "" vowel-shift) "")
(check-expect (substitute (string-append "Once upon a time there was a lovely princess. But she had "
                                  "an enchantment upon her of a fearful sort which could only be "
                                  "broken by love's first kiss.") vowel-shift)
                          (string-append "Onci ypun e tomi thiri wes e luvila pronciss. Byt shi hed"
                                         " en inchentmint ypun hir uf e fierfyl surt whoch cuyld unla"
                                         " bi brukin ba luvi's forst koss."))