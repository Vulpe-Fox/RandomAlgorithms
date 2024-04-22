;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname final-reference) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))

;; ---- Data definitions for Q2b
;; A Substitution Cipher (SCipher) is a (listof (list Char Char))
;; Requires: The first Char in each pair (the key) is unique



;; ---- Data definitions for Q4
(define-struct node (key left right))
;; A Node is a (make-node Nat BST BST)
;; Requires: key > every key in left BST
;;  key < every key in right BST

;; A Binary Search Tree (BST) is one of
;; * empty
;; * Node


;; ---- Data definitions for Q5
;; A (nested-listof X) is one of:
;; * empty
;; * (cons X (nested-listof X))
;; * (cons (nested-listof X) (nested listof X))


;; ---- Data definitions for Q6
;; an (alistof X Y) is a (listof (list X Y))
;; Requires: each X is unique (no duplicates)