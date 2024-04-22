;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname dist) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;
;;/----------------------------------\
;;    Cameron Carmichael (20881375)
;;    CS 135 Fall 2020
;;    Midterm 2 Question 4
;;\----------------------------------/
;;

;;A HamPair is a (list Str (listof Sym))

;;A HamList is a (listof HamPair)
;;Requires: Every (listof Sym) in the HamList has the same length.
;;   Each Str in each HamPair is distinct.

;;(min-hamming list hamlist) produces a (listof Str) where each Str corresponds to the key of a
;;   HamPair whose value has the minimum Hamming distance to the consumed list of symbols.
;;(find-min-hamming-in-list list hamlist min) finds any HamPair in hamlist which have a matching 
;;   hamming distance of (listof Sym) and list to min
;;(calc-min-hamming list hamlist) produces the minimum hamming distance of a list
;;(hamming-distance list1 list2) produces the Hamming distance between two equally long lists of Sym:
;;   list1 and list2.

;;Test Variables
(define channel-list (list
    (list "channel1" '(b a b a b a a a b a a a))
    (list "channel2" '(b a b a b b a a b b b b))
    (list "channel3" '(b b b b b b b a b a b b))))

(define gene1 '(t a g a a g t t t))
(define gene2 '(t a g a a g g t t))

(define packet-a '(b a b a b a a a b a a a))
(define packet-b '(b a b a b b a a b a b b))

(define packet-c '(a a a a))
(define packet-d '(b b b b))

(define packet1 '(b a b a b a a a b a a a))
(define packet2 '(b a b a b b a a b a a b))
(define packet3 '(b b b b b b b a b a b b))

;;Examples
(check-expect (hamming-distance gene1 gene2) 1)
(check-expect (hamming-distance packet-a packet-b) 3)

(check-expect (min-hamming packet2 empty) empty)
(check-expect (min-hamming packet1 channel-list) '("channel1"))
(check-expect (min-hamming packet2 channel-list) '("channel1" "channel2"))

;;(min-hamming list hamlist) (listof Sym) + HamList -> (listof Str)
;;All (listof Sym) in each HamPair in hamlist has the same length as list
(define (min-hamming list hamlist)
  (find-min-hamming-in-list list hamlist (calc-min-hamming list hamlist)))

;;(find-min-hamming-in-list list hamlist min) (listof Sym) + HamList + Num -> (listof Str)
(define (find-min-hamming-in-list list hamlist min)
  (cond[(empty? hamlist) empty]
       [(= (hamming-distance list (second (first hamlist))) min)
        (cons (first (first hamlist)) (find-min-hamming-in-list list (rest hamlist) min))]
       [else (find-min-hamming-in-list list (rest hamlist) min)]))

;;(calc-min-hamming list hamlist) (listof Sym) + HamList -> Num
(define (calc-min-hamming list hamlist)
  (cond[(empty? hamlist) 0]
       [(empty? (rest hamlist)) (hamming-distance list (second (first hamlist)))]
       [else (min (hamming-distance list (second (first hamlist)))
                  (calc-min-hamming list (rest hamlist)))]))

;;(hamming-distance list1 list2) (ListOf Sym) + (ListOf Sym) -> Num
;;list1 and list2 must be of equal length
(define (hamming-distance list1 list2)
  (cond[(empty? list1) 0]
       [(symbol=? (first list1) (first list2))
        (hamming-distance (rest list1) (rest list2))]
       [else (add1 (hamming-distance (rest list1) (rest list2)))]))

;;Tests
(check-expect (hamming-distance '() '()) 0)
(check-expect (hamming-distance packet-c packet-c) 0)
(check-expect (hamming-distance packet-c packet-d) 4)
(check-expect (hamming-distance '(a) '(b)) 1)
(check-expect (hamming-distance '(a) '(a)) 0)

(check-expect (min-hamming packet-b channel-list) '("channel2"))
(check-expect (min-hamming packet3 channel-list) '("channel3"))