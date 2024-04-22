;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname change) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;;
;;/----------------------------------\
;;    Cameron Carmichael (20881375)
;;    CS 135 Fall 2020
;;    Assignment 09, Problem 4
;;\----------------------------------/
;;

;; A ChangeList is a (listof (list Nat (listof Nat)))

;;(fewest-coins total denoms) Produces the list of coins required to use the least number of coins
;;   to make change for a total using a certain list of denominations, 'denoms'

;;Test Variables
(define canadian '(100 200 10 25 5 1))
(define zorkmid '(19 9 1))

;;Examples
(check-expect (fewest-coins 45 canadian) '(25 10 10))
(check-expect (fewest-coins 27 zorkmid) '(9 9 9))

;;(fewest-coins total denoms) Nat + (Listof Nat) -> ChangeList
;;Requires: must be able to make exact change for total using denoms
(define (fewest-coins total denoms)
  (local[(define (find-denoms curr-total curr-denoms)
           (local[(define valid-denoms (filter (lambda (val) (<= val curr-total)) curr-denoms))]
             (cond[(or (= curr-total 0) (empty? valid-denoms)) empty]
                  [(empty? (rest valid-denoms))
                   (cons (first valid-denoms)
                         (find-denoms (- curr-total (first valid-denoms)) valid-denoms))]
                  [else (local[(define take (find-denoms (- curr-total (first valid-denoms))
                                                         valid-denoms))
                               (define leave (find-denoms curr-total (rest valid-denoms)))]
                          (cond[(< (add1 (length take)) (length leave))
                                (cons (first valid-denoms) take)]
                               [else leave]))])))
         (define (insert-sort list-of-denoms)
           (cond[(empty? list-of-denoms) empty]
                [else (insert (first list-of-denoms) (insert-sort (rest list-of-denoms)))]))
         (define (insert denom list-of-denoms)
           (cond[(empty? list-of-denoms) (cons denom empty)]
                [(> denom (first list-of-denoms)) (cons denom list-of-denoms)]
                [else (cons (first list-of-denoms) (insert denom (rest list-of-denoms)))]))]
    (find-denoms total (insert-sort denoms))))

;;Tests
(check-expect (fewest-coins 0 canadian) '())
(check-expect (fewest-coins 0 zorkmid) '())
(check-expect (fewest-coins 108 '(2 73 54)) '(54 54))