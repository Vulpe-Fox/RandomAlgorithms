;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname mt2-shortans) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;; ***********************************
;;   YOUR NAME (your Student #)
;;   CS 135 Fall 2020
;;   MT2 Short Answer
;; ***********************************

;; =================
;; DO NOT EDIT ANYTHING IN THIS SECTION
;; (they are needed so you can "Run" this file.
(define-struct ls (first rest))
;; =================


;; -------------------------
;; Jumble List questions

;; Complete with the length of the list
(define jumble-length 5)

;; Complete with the list in list format
(define jumble-list-format
  (list "volleyball" (make-ls 'hockey 'nothing) empty 21 true))

;; Complete with the list in quote format
(define jumble-quote-format
  'not-possible)

;; Complete with the list in cons format
(define jumble-cons-format
  (cons "volleyball" (cons (make-ls 'hockey 'nothing) (cons empty (cons 21 (cons true empty))))))



;; -------------------------
;; Clothes list questions

;; Complete with the length of the list
(define clothes-length 7)

;; Complete with the list in list format
(define clothes-list-format
  (list 'shirt (list 'mittens) (list 'socks (list 'shoes 'boots 'sandals))
        'pants 'dress 'jumper (list 'kilt)))

;; Complete with the list in quoted list format
(define clothes-quote-format
  '(shirt (mittens) (socks (shoes boots sandals)) pants dress jumper (kilt)))

;; Complete the function body. You may only use
;;   parentheses, cons, first, rest, and the parameter c.
(define (clothes-rearrange c)
  (cons (first (rest (first (rest (first (rest (rest c)))))))
        (cons (first (rest (rest (rest (rest c)))))
              (rest (rest (rest (rest (rest (rest (rest c))))))))))

(check-expect (clothes-rearrange clothes-list-format)
              (list 'boots 'dress))

;; -------------------------
;; SpreadyInt Questions

;; Complete with the template definition
(define (spreadyint-template n)
  (cond [(and (< n 11) (> n -5)) false]
        [(or (= n 11) (= n -5)) true]
        [else (spreadyint-template (/ n 3))]))

;; Choose all that apply from
;; 'A 'B 'C 'D 'E 'F 'G 'H
(define spreadyint-members (list 'A 'E 'F))



;; -------------------------
;; Inspection (decreasing?) Questions

;; CAREFUL! Each one of these answers uses the same options.
;; Make sure you are filling in the proper questions!

;; Implementation 1: Choose one from 'A 'B 'C 'D 'E 'F
(define decreasing-result-v1 'E)

;; Implementation 2: Choose one from 'A 'B 'C 'D 'E 'F
(define decreasing-result-v2 'A)

;; Implementation 3: Choose one from 'A 'B 'C 'D 'E 'F
(define decreasing-result-v3 'C)



;; -------------------------
;; Thing questions

;; Most SPECIFIC definition. Choose one from
;; 'I 'J 'K 'L 'M 'N 'O
(define thing-specific 'N)

;; INVALID definitions. Choose all that apply from
;; 'P 'Q 'R 'S 'T 'U 'V
(define thing-invalid-defs (list 'Q 'R 'S 'T))
