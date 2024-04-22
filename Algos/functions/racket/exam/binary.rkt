;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname binary) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;
;;/----------------------------------\
;;    Cameron Carmichael (20881375)
;;    CS 135 Fall 2020
;;    Exam Question 4
;;\----------------------------------/
;;

(define-struct node (key left right))
;; A Node is a (make-node Nat BST BST)
;; Requires: key > every key in left BST
;;  key < every key in right BST

;; A Binary Search Tree (BST) is one of
;; * empty
;; * Node

;;(sorted-list->bst lst) Produces a balanced BST from a sorted list of natural numbers

;;Examples
(check-expect (sorted-list->bst empty) empty)
(check-expect (sorted-list->bst '(1 2 3 4 5))
              (make-node 3
                         (make-node 2
                                    (make-node 1 empty empty)
                                    empty)
                         (make-node 5
                                    (make-node 4 empty empty)
                                    empty)))

;;sorted-list->bst: (Listof Nat) -> BST
;;Requires: lst is sorted in strictly increasing order with no duplicates
(define (sorted-list->bst lst)
    (cond[(empty? lst) empty]
         [else (local[(define size (length lst))
                      (define middle-index (ceiling (/ (sub1 size) 2)))
                      (define (find-element curr-lst position)
                        (cond[(= 0 position) (first curr-lst)]
                             [else (find-element (rest curr-lst) (sub1 position))]))
                      (define middle-element (find-element lst middle-index))
                      (define left-elements (foldr (lambda (x rror) (cond[(< x middle-element)
                                                                          (cons x rror)]
                                                                         [else rror]))
                                                   '() lst))
                      (define right-elements (foldr (lambda (x rror) (cond[(> x middle-element)
                                                                           (cons x rror)]
                                                                          [else rror]))
                                                    '() lst))]
                 (make-node middle-element
                            (sorted-list->bst left-elements)
                            (sorted-list->bst right-elements)))]))

;;Tests
(check-expect (sorted-list->bst '(1 2 3 4 5 6))
              (make-node 4
                         (make-node 2
                                    (make-node 1 empty empty)
                                    (make-node 3 empty empty))
                         (make-node 6
                                    (make-node 5 empty empty)
                                    empty)))
(check-expect (sorted-list->bst '(1 2 4 6 8 12))
              (make-node 6
                         (make-node 2
                                    (make-node 1 empty empty)
                                    (make-node 4 empty empty))
                         (make-node 12
                                    (make-node 8 empty empty)
                                    empty)))
(check-expect (sorted-list->bst '(1))
              (make-node 1 empty empty))