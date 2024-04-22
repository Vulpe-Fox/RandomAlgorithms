;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname shortans) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;PART A;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (make-it-so x k)
  (foldr (lambda (x k)
           (cond [(odd? x) k]
                 [else (cons (* 2 x) k)]))
         empty
         (map (lambda (x) (+ x k)) x)))

;; Complete the function definition for make-it-so2 here
(define (make-it-so2 nlst const) ; you can change the parameter names if you want to
  (cond[(empty? nlst) empty]
       [(odd? (+ (first nlst) const))
        (make-it-so2 (rest nlst) const)]
       [else (cons (* 2 (+ (first nlst) const))
                   (make-it-so2 (rest nlst) const))]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;PART B;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Write your answer as a comment here
;;
;;    If we work our way down the chain of values which are given in the else, we get to a point where
;; the value being given to f is a single empty. Now given what we know about f, it seems to require
;; two values as inputs -- so regardless of whether it knows how to deal with empty, the contract
;; might not work with a single value.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;PART C;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-struct xnode (label children))
;; an exam tree node (XNode) is a (make-xnode Num (listof XTree))
;; an exam tree (XTree) is one of:
;; * a XNode or
;; * a Num.

;; Fill in the contract for g here
;; g: (Anyof Num XNode) -> Any

;;***Note***
;;This answer is due to the possible number or whatever h results in. h could make the string "cool"
;;   if the number is equal to the sum of its children for all function cares.


;; This is commented out because it cannot be run without a definition for h
;(define (g t)
;  (cond [(number? t) t]
;        [else
;         (h (xnode-label t)
;            (foldr
;             + 0 (map g
;                      (xnode-children t))))]))

;; Fill in the contract for h here
;; h: Num + Num -> Any

