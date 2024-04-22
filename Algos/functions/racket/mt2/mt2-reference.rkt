;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname mt2-reference) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;---- components of part 2a
"volleyball"
(make-ls 'hockey 'nothing)
empty
21
true
'not-possible

;;---- components of part 2b
'shirt
'mittens
'socks
'shoes
'boots
'sandals
'kilt
'pants
'dress
'jumper
'not-possible

;;---- data definitions for 4b

;; A HamPair is a (list Str (listof Sym))
    
;; A HamList is a (listof HamPair)
;; Requires: every (listof Sym) in the HamList has the 
;;   same length. Each Str in each HamPair is distinct.

;;---- data definitions for 5

;; A Substitution Cipher (SCipher) is a (listof (list Char Char))
;; Requires: The first Char in each pair (the key) is unique

;;---- data definitions for 6

;; A Resource is (anyof 'fire 'wood 'water)

(define-struct card (type power))
;; A Card is a (make-card Resource Nat)

;; An Outcome is (anyof 'player-1 'player-2 'tie-game)

;;----- Permitted functions

;; Here are the functions you are permitted to use. It should be 
;; identical to the ones listed on the second page of the midterm, 
;; but if there is a conflict the ones in the midterm PDF win.

define-struct error round
* + - ... / < <= = > >= abs add1 and append boolean? ceiling char=? char?
check-error check-expect check-within cond cons cons? define eighth else
empty? equal? even? exp expt fifth first floor fourth integer? length
list list->string list? max member? min negative? not number->string
number? odd? or positive? quotient remainder rest second seventh sixth
sqr sqrt string->list string-append string-length string-upcase string<=?
string<? string=? string>=? string>? string? sub1 substring symbol=?
symbol? third zero?
