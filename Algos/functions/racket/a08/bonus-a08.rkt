;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname bonus-a08) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;;
;;/----------------------------------\
;;    Cameron Carmichael (20881375)
;;    CS 135 Fall 2020
;;    Assignment 08, Bonus
;;\----------------------------------/
;;

;;The comments are as much to help you understand as to help me work through logic this bonus b/c I am
;;   still frustrated at ALFs due to question 3d)

(define (subsets1 set)
  (cond[(empty? set) (list empty)] ;;When we run out of combinations, will append the empty case
       [else (local[(define set-without-first (subsets1 (rest set)))] ;;Without this line, will be
                                                                   ;; exponential blowup in solution.
                                                                   ;;It functions to get rid of first
                                                                   ;; element after it has been
                                                                   ;; exhausted of combinations
               (append (map (lambda (value) (cons (first set) value)) set-without-first)
                       ;;Map determines all combinations formed from the first element then
                       ;;appends it to the later combinations which exclude the first element
                       set-without-first))]))

;;All in all, this solution will create the form:
;;'(a b c) ->
;;'((a b c) (a b) (a c) (a) (b c) (b) (c) ())
;;Notice that all subsets including a occur first, then a is ignored, and so forth
;;So increasing elements will end up with:
;;'(a b c d) ->
;;'((a b c d) (a b c) (a b d) (a b) (a c d) (a c) (a d) (a) ...)
;;So basically affixing a to the start and doing the three digit subroutine then removing a

;;Hopefully this helps ^^ - Cam

;;EDIT: I did it! There was a lot of trial and error b/c I forgot about the v value

(define (subsets2 set)
  (foldr (lambda(x rror) (append (map (lambda (v) (cons x v)) rror) rror)) '(()) set))