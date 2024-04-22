;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname general) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;
;;/----------------------------------\
;;    Cameron Carmichael (20881375)
;;    CS 135 Fall 2020
;;    Exam Question 6
;;\----------------------------------/
;;

;; an (alistof X Y) is a (listof (list X Y))
;; Requires: each X is unique (no duplicates)

;;(alist-combine alst1 alst2 combine) Produces a new association list that contains all keys that are
;;   in either of the two consumed lists.
;;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;   If only one list contains the key, then the corresponding value can be used.
;;   If both lists contain the same key, then the value associated with that key will be
;;      (combine v1 v2)

;;Examples
(check-expect (alist-combine '((a 1)(b 2)) '((c 1)(a 3)) +) '((a 4) (b 2) (c 1)))
(check-expect (alist-combine '((1 1)(2 2)) empty +) '((1 1) (2 2)))
(check-expect (alist-combine '((1 "One") (3 "Three") (2 "Two"))
                             '((2 "Deux") (1 "Un") (3 "Trois"))
                             (lambda (s1 s2) (cond [(< (string-length s1)
                                                       (string-length s2))
                                                    s1]
                                                   [else s2])))
              '((1 "Un") (3 "Trois") (2 "Two")))

;;alist-combine: (alistof W X) + (alistof W Y) + (X + Y -> Z) -> (alistof W (Anyof X Y Z))
;;Requires: All keys of alst1 and alst2 are the same type
;;          combine can act on all elements of X,Y to create Z, but X,Y,Z can be the same type
(define (alist-combine alst1 alst2 combine)
  (local[(define (find-matching-keys matching? l1 l2)
           (foldr (lambda (x rror)
                    (local[(define match
                             (foldr (lambda (y rror) (cond[(equal? (first y) (first x)) y]
                                                          [else rror]))
                                    '() l2))]
                      (cond[(and matching? (empty? match)) rror]
                           [matching? (cons (list x match) rror)]
                           [(empty? match) (cons x rror)]
                           [else rror])))
                  '() l1))
         (define non-match-1 (find-matching-keys false alst1 alst2))
         (define non-match-2 (find-matching-keys false alst2 alst1))
         (define matching (find-matching-keys true alst1 alst2))
         (define combine-matching
           (foldr (lambda (x rror) (cons (list (first (first x))
                                               (combine (second (first x))
                                                        (second (second x))))
                                         rror))
                  '() matching))]
    (append combine-matching non-match-1 non-match-2)))

;;Tests
(check-expect (alist-combine empty '((1 1)(2 2)) +) '((1 1) (2 2)))
(check-expect (alist-combine empty empty +) '())
