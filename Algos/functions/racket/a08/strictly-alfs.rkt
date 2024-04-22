;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname strictly-alfs) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;;
;;/----------------------------------\
;;    Cameron Carmichael (20881375)
;;    CS 135 Fall 2020
;;    Assignment 08, Problem 3
;;\----------------------------------/
;;

;;(occurrences num nlst) Produces the number of times a number appears in a list of numbers
;;(zip lst1 lst2) Produces a list of pairs where the ith element contains the ith element of the first
;;   list and the ith element of the second list, where each list has the same length
;;(unzip lst) Produces a list of two lists where the first list is the first element from each pair
;;   and the second list is the second element from each pair; in a list of pairs
;;(subsequence lst from to) Produces the subsequence from lst that begins at index from and ends just
;;   before index to -- starting from i=0

;;Examples
(check-expect (occurrences 2 '(1 2 1 2 2 3 1)) 3)

(check-expect (zip '(1 2 3) '(a b c)) '((1 a)(2 b)(3 c)))

(check-expect (unzip '((1 a)(2 b)(3 c))) '((1 2 3) (a b c)))
(check-expect (unzip '()) '(()()))

(check-expect (subsequence '(a b c d e f g) 1 4) '(b c d))

;;(occurrences num nlst) Num + (Listof Num) -> Num
(define (occurrences num nlst)
  (length (filter (lambda (curr-num) (= num curr-num)) nlst)))

;;(zip lst1 lst2) (Listof Any) + (Listof Any) -> (Listof (Any Any))
(define (zip lst1 lst2)
  (map (lambda (element1 element2)
         (cons element1 (cons element2 empty)))
       lst1 lst2))

;;(unzip lst) (Listof Any) -> (Listof (Listof Any))
(define (unzip lst)
  (cond[(empty? lst) '(()())]
       [else (cons (foldr (lambda (value list) (cons (first value) list)) '() lst)
                   (cons (foldr (lambda (value list) (cons (second value) list)) '() lst)
                         empty))]))

;;(subsequence lst from to) (Listof Any) + Nat + Nat -> (Listof Any)
(define (subsequence lst from to)
  (cond[(<= to from) '()]
       [else (foldr (lambda (value list) (cons (second value) list)) '()
                    (filter (lambda (curr-element) (not (equal? -1 (first curr-element))))
                            (map (lambda (element1 element2)
                                   (cond[(>= element1 to) (cons -1 (cons element2 empty))]
                                        [else (cons element1 (cons element2 empty))]))
                                 (build-list (length lst)
                                             (lambda (num) (cond[(< num from) -1]
                                                                [else num])))
                                 lst)))]))

;;Tests
(check-expect (occurrences 2 '(1 5 1 5 5 3 1)) 0)
(check-expect (occurrences 2 '()) 0)

(check-expect (zip '(1 2 3 a) '(a b c c)) '((1 a)(2 b)(3 c)(a c)))
(check-expect (zip '(1 2 3 b) '(a b c 2)) '((1 a)(2 b)(3 c)(b 2)))
(check-expect (zip '() '()) '())

(check-expect (unzip '((a a)(2 b)(3 c))) '((a 2 3) (a b c)))
(check-expect (unzip '((a a))) '((a) (a)))

(check-expect (subsequence '() 1 4) '())
(check-expect (subsequence '(a 1 2 -1) 1 2) '(1))