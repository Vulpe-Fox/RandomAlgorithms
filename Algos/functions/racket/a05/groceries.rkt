;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname groceries) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;;
;;/----------------------------------\
;;    Cameron Carmichael (20881375)
;;    CS 135 Fall 2020
;;    Assignment 05, Problem 2
;;\----------------------------------/
;;

(define-struct grocery (dept name cost mass))
;; A Grocery is a (make-grocery Str Str Num Num)
;; Requires: cost >= 0, mass > 0.
(define-struct interval (lo hi))
;; An Interval is a (make-interval (anyof 'dontcare Num)
;;                                 (anyof 'dontcare Num))
(define-struct query (dept name cost mass))
;; A GroceryQuery is a
;;   (make-query StrPatt StrPatt Interval Interval)

;; A Store is a (listof Grocery)
;; Requires: no two items have both the same dept and same name.

;; A StrPatt is a (anyof Str 'dontcare)

;;(in-interval? num interval) produces a boolean which is whether or not num is in interval
;;(find-matches store query) produces a list of all entries which match the query
;;(sort-dept-name glst) produces a list of groceries sorted first by dept, then by name
;;(sort-dept glst) produces a list of groceries sorted by dept
;;(sort-name glst) produces a list of groceries sorted by name from a list of groceries sorted
;;   by dept
;;(insert-dept glst grocery-to-insert) produces a list of groceries with grocery-to-insert placed in
;;   the right position by lexicographical heirarchy
;;(insert-name glst gti) produces a list of groceries with gti placed in the right position
;;   by lexicographical heirarchy
;;(overlap store1 store2) produces a Store consisting of overlapping items from multiple stores
;;(less-price-per-gram grocery1 grocery2) produces the grocery with the better deal on it
;;(scale-prices store query ratio) produces the Store such that all Grocery in store which match query
;;   will have their prices multiplied by the ratio

;;Types of Store:
(define student-shop
  (list (make-grocery "produce" "apple" 2.49 600)
        (make-grocery "produce" "banana" 0.95 1000)
        (make-grocery "grocery" "toilet paper" 4.99 850)
        (make-grocery "produce" "orange" 3.99 4000)
        (make-grocery "grocery" "tomato sauce" 3.99 400)
        (make-grocery "produce" "lemon" 2.49 500)
        (make-grocery "produce" "lime" 2.99 5000)
        (make-grocery "dairy" "eggs" 1.99 50)
        (make-grocery "produce" "potato" 1.99 250)
        (make-grocery "produce" "corn" 1.99 275)
        (make-grocery "dairy" "milk" 0.49 500)
        (make-grocery "grocery" "tissues" 5.99 1000)
        (make-grocery "dairy" "ice cream" 0.69 450)
        (make-grocery "dairy" "cheese" 6.49 900)
        (make-grocery "grocery" "chips" 2.99 800)
        (make-grocery "grocery" "peace tea" 0.99 100)))

(define try-n-save
  (list (make-grocery "produce" "apple" 2.49 600)
        (make-grocery "seed" "rice" 0.95 1000)
        (make-grocery "dairy" "milk" 3.99 4000)
        (make-grocery "seed" "pinto" 2.49 500)
        (make-grocery "produce" "potato" 2.99 5000)
        (make-grocery "chips" "potato" 1.99 250)
        (make-grocery "chips" "corn" 1.99 275)
        (make-grocery "seed" "wheat" 0.49 500)
        (make-grocery "produce" "banana" 0.69 450)
        (make-grocery "dairy" "cheese" 6.49 900)
        (make-grocery "chips" "banana" 1.99 50)
        (make-grocery "produce" "peach" 3.99 400)
        (make-grocery "seed" "lentil" 2.99 800)
        (make-grocery "produce" "corn" 0.99 100)
        (make-grocery "seed" "corn" 4.99 850)
        (make-grocery "dairy" "kefir" 5.99 1000)))

(define kwik-e-mart
  (list (make-grocery "seed" "rice" 0.38 400)
        (make-grocery "can" "corn" 4.00 400)
        (make-grocery "seed" "pinto" 2.49 500)
        (make-grocery "produce" "apple" 2.99 400)
        (make-grocery "can" "creamed eels" 2.19 350)
        (make-grocery "produce" "pineapple" 3.17 250)))

;;Examples
(check-expect (in-interval? 42
      (make-interval 'dontcare 'dontcare)) true)
(check-expect (in-interval? 34
      (make-interval 35 'dontcare)) false)
(check-expect (in-interval? 34
      (make-interval 'dontcare 35)) true)

(check-expect
 (find-matches try-n-save (make-query "seed" 'dontcare
                             (make-interval 'dontcare 'dontcare)
                             (make-interval 'dontcare 'dontcare)))
 (list
  (make-grocery "seed" "rice" 0.95 1000)
  (make-grocery "seed" "pinto" 2.49 500)
  (make-grocery "seed" "wheat" 0.49 500)
  (make-grocery "seed" "lentil" 2.99 800)
  (make-grocery "seed" "corn" 4.99 850)))
(check-expect
 (find-matches try-n-save (make-query 'dontcare "corn"
                             (make-interval 'dontcare 'dontcare)
                             (make-interval 'dontcare 'dontcare)))
 (list (make-grocery "chips" "corn" 1.99 275)
       (make-grocery "produce" "corn" 0.99 100)
       (make-grocery "seed" "corn" 4.99 850)))
(check-expect
 (find-matches try-n-save (make-query "seed" 'dontcare
                             (make-interval 'dontcare 3.00)
                             (make-interval 600 'dontcare)))
 (list
  (make-grocery "seed" "rice" 0.95 1000)
  (make-grocery "seed" "lentil" 2.99 800)))

(check-expect (sort-dept-name try-n-save)
              (list
               (make-grocery "chips" "banana" 1.99 50)
               (make-grocery "chips" "corn" 1.99 275)
               (make-grocery "chips" "potato" 1.99 250)
               (make-grocery "dairy" "cheese" 6.49 900)
               (make-grocery "dairy" "kefir" 5.99 1000)
               (make-grocery "dairy" "milk" 3.99 4000)
               (make-grocery "produce" "apple" 2.49 600)
               (make-grocery "produce" "banana" 0.69 450)
               (make-grocery "produce" "corn" 0.99 100)
               (make-grocery "produce" "peach" 3.99 400)
               (make-grocery "produce" "potato" 2.99 5000)
               (make-grocery "seed" "corn" 4.99 850)
               (make-grocery "seed" "lentil" 2.99 800)
               (make-grocery "seed" "pinto" 2.49 500)
               (make-grocery "seed" "rice" 0.95 1000)
               (make-grocery "seed" "wheat" 0.49 500)))

(check-expect
 (overlap kwik-e-mart try-n-save)
 (list
  (make-grocery "produce" "apple" 2.49 600) ; Buy cheaper.
  (make-grocery "seed" "pinto" 2.49 500)    ; Same price and size.
  (make-grocery "seed" "rice" 0.38 400)))   ; Same price; buy smaller.

(check-expect (scale-prices
               kwik-e-mart
               (make-query "can"
                           'dontcare
                           (make-interval 'dontcare 'dontcare)
                           (make-interval 'dontcare 'dontcare)) 1.10)
              (list (make-grocery "seed" "rice" 0.38 400)
                    (make-grocery "can" "corn" 4.40 400)
                    ;; corn goes from 4.00 to 4.40.
                    (make-grocery "seed" "pinto" 2.49 500)
                    (make-grocery "produce" "apple" 2.99 400)
                    (make-grocery "can" "creamed eels" 2.41 350)
                    ;; eels goes from 2.19 to 2.409, rounded to 2.41.
                    (make-grocery "produce" "pineapple" 3.17 250)))

;;Functions

;;(in-interval? num interval) Num + Interval -> Bool
(define (in-interval? num interval)
  (cond[(and (equal? (interval-hi interval) 'dontcare) (equal? (interval-lo interval) 'dontcare))
        true]
       [(and (equal? (interval-hi interval) 'dontcare) (not (equal? (interval-lo interval) 'dontcare))
             (>= num (interval-lo interval))) true]
       [(and (equal? (interval-hi interval) 'dontcare) (not (equal? (interval-lo interval) 'dontcare))
             (not (>= num (interval-lo interval)))) false]
       [(and (equal? (interval-lo interval) 'dontcare) (<= num (interval-hi interval))) true]
       [(and (equal? (interval-lo interval) 'dontcare) (not (<= num (interval-hi interval)))) false]
       [(and (>= num (interval-lo interval)) (<= num (interval-hi interval))) true]
       [else false]))

;;(find-matches store query) Store + GroceryQuery -> lst
(define (find-matches store query)
  (cond[(empty? store) empty]
       [(and (matching-str? (query-dept query) (grocery-dept (first store)))
             (matching-str? (query-name query) (grocery-name (first store)))
             (in-interval? (grocery-cost (first store)) (query-cost query))
             (in-interval? (grocery-mass (first store)) (query-mass query)))
        (cons (first store) (find-matches (rest store) query))]
       [else (find-matches (rest store) query)]))

;;(matching-str? strpatt str) StrPatt + Str -> Bool
(define (matching-str? strpatt str)
  (cond[(or (equal? strpatt str) (equal? strpatt 'dontcare)) true]
       [else false]))

;;The algorithm used for sorting will be insertion sort, meaning I need two functions:
;;   (sort glst)
;;   (insert slst grocery-to-insert)

;;(sort-dept-name glst) (listof Grocery) -> (listof Grocery)
(define (sort-dept-name glst)
  (sort-name (sort-dept glst)))

;;(sort-dept glst) (listof Grocery) -> (listof Grocery)
(define (sort-dept glst)
  (cond[(empty? glst) empty]
       [else (insert-dept (sort-dept (rest glst)) (first glst))]))

;;(sort-name glst) (listof Grocery) -> (listof Grocery)
;;grocery-dept must be sorted
(define (sort-name glst)
  (cond[(empty? glst) empty]
       [else (insert-name (sort-name (rest glst)) (first glst))]))

;;(insert-dept glst grocery-to-insert) (listof Grocery) + Grocery -> (listof Grocery)
(define (insert-dept glst grocery-to-insert)
  (cond[(empty? glst) (cons grocery-to-insert empty)]
       [else (cond[(string<? (grocery-dept grocery-to-insert)
                             (grocery-dept (first glst)))
                   (cons grocery-to-insert glst)]
                  [else (cons (first glst)
                              (insert-dept (rest glst) grocery-to-insert))])]))

;;(insert-name glst gti) (listof Grocery) + Grocery -> (listof Grocery)
(define (insert-name glst gti)
  (cond[(empty? glst) (cons gti empty)]
       [(string=? (grocery-dept gti)
                  (grocery-dept (first glst)))
        (cond[(string<? (grocery-name gti)
                        (grocery-name (first glst)))
              (cons gti glst)]
             [else (cons (first glst)
                         (insert-name (rest glst) gti))])]
       [else (cons gti (insert-name (rest glst) (first glst)))]))

;;(overlap store1 store2) Store + Store -> Store
(define (overlap store1 store2)
  (cond[(empty? store1) empty]
       [(empty? (find-matches store2 (make-query
                                      (grocery-dept (first (sort-dept-name store1)))
                                      (grocery-name (first (sort-dept-name store1)))
                                      (make-interval 'dontcare 'dontcare)
                                      (make-interval 'dontcare 'dontcare))))
        (overlap (rest (sort-dept-name store1)) store2)]
       [else (cons (less-price-per-gram (first (sort-dept-name store1))
                                            (first (find-matches store2 (make-query
                                      (grocery-dept (first (sort-dept-name store1)))
                                      (grocery-name (first (sort-dept-name store1)))
                                      (make-interval 'dontcare 'dontcare)
                                      (make-interval 'dontcare 'dontcare)))))
                   (overlap (rest (sort-dept-name store1)) store2))]))

;;(less-price-per-gram grocery1 grocery2) Grocery + Grocery -> Grocery
(define (less-price-per-gram grocery1 grocery2)
  (cond[(< (/ (grocery-cost grocery1) (grocery-mass grocery1))
           (/ (grocery-cost grocery2) (grocery-mass grocery2)))
        grocery1]
       [(> (/ (grocery-cost grocery1) (grocery-mass grocery1))
           (/ (grocery-cost grocery2) (grocery-mass grocery2)))
        grocery2]
       [else (cond[(<= (grocery-mass grocery1) (grocery-mass grocery2))
                   grocery1]
                  [else grocery2])]))

;;(scale-prices store query ratio) Store + GroceryQuery + Num -> Store
;;ratio must be non-negative
(define (scale-prices store query ratio)
  (cond[(empty? store) empty]
       [(empty? (find-matches (list (first store)) query))
        (cons (first store) (scale-prices (rest store) query ratio))]
       [else (cons (make-grocery (grocery-dept (first store))
                                 (grocery-name (first store))
                                 (/ (round (* 100 (* (grocery-cost (first store)) ratio))) 100)
                                 (grocery-mass (first store)))
                   (scale-prices (rest store) query ratio))]))

;;Tests
(check-expect (in-interval? 42
      (make-interval 40 'dontcare)) true)
(check-expect (in-interval? 42
      (make-interval 40 44)) true)
(check-expect (in-interval? 47
      (make-interval 'dontcare 44)) false)
(check-expect (in-interval? 47
      (make-interval 40 44)) false)

(check-expect
 (find-matches student-shop (make-query "produce" "apple"
                             (make-interval 'dontcare 'dontcare)
                             (make-interval 'dontcare 'dontcare)))
 (list (make-grocery "produce" "apple" 2.49 600)))

(check-expect (sort-dept-name student-shop)
  (list (make-grocery "dairy" "cheese" 6.49 900)
        (make-grocery "dairy" "eggs" 1.99 50)
        (make-grocery "dairy" "ice cream" 0.69 450)
        (make-grocery "dairy" "milk" 0.49 500)
        (make-grocery "grocery" "chips" 2.99 800)
        (make-grocery "grocery" "peace tea" 0.99 100)
        (make-grocery "grocery" "tissues" 5.99 1000)
        (make-grocery "grocery" "toilet paper" 4.99 850)
        (make-grocery "grocery" "tomato sauce" 3.99 400)
        (make-grocery "produce" "apple" 2.49 600)
        (make-grocery "produce" "banana" 0.95 1000)
        (make-grocery "produce" "corn" 1.99 275)
        (make-grocery "produce" "lemon" 2.49 500)
        (make-grocery "produce" "lime" 2.99 5000)
        (make-grocery "produce" "orange" 3.99 4000)
        (make-grocery "produce" "potato" 1.99 250)))
(check-expect (sort-dept-name kwik-e-mart)
  (list (make-grocery "can" "corn" 4.00 400)
        (make-grocery "can" "creamed eels" 2.19 350)
        (make-grocery "produce" "apple" 2.99 400)
        (make-grocery "produce" "pineapple" 3.17 250)
        (make-grocery "seed" "pinto" 2.49 500)
        (make-grocery "seed" "rice" 0.38 400)))

(check-expect
 (overlap kwik-e-mart student-shop)
 (list
  (make-grocery "produce" "apple" 2.49 600)))
(check-expect
 (overlap try-n-save kwik-e-mart)
 (list
  (make-grocery "produce" "apple" 2.49 600) ; Buy cheaper.
  (make-grocery "seed" "pinto" 2.49 500)    ; Same price and size.
  (make-grocery "seed" "rice" 0.38 400)))   ; Same price; buy smaller.

(check-expect (scale-prices
               student-shop
               (make-query "produce"
                           'dontcare
                           (make-interval 'dontcare 'dontcare)
                           (make-interval 'dontcare 'dontcare)) 0)
              (list (make-grocery "produce" "apple" 0 600)
                    (make-grocery "produce" "banana" 0 1000)
                    (make-grocery "grocery" "toilet paper" 4.99 850)
                    (make-grocery "produce" "orange" 0 4000)
                    (make-grocery "grocery" "tomato sauce" 3.99 400)
                    (make-grocery "produce" "lemon" 0 500)
                    (make-grocery "produce" "lime" 0 5000)
                    (make-grocery "dairy" "eggs" 1.99 50)
                    (make-grocery "produce" "potato" 0 250)
                    (make-grocery "produce" "corn" 0 275)
                    (make-grocery "dairy" "milk" 0.49 500)
                    (make-grocery "grocery" "tissues" 5.99 1000)
                    (make-grocery "dairy" "ice cream" 0.69 450)
                    (make-grocery "dairy" "cheese" 6.49 900)
                    (make-grocery "grocery" "chips" 2.99 800)
                    (make-grocery "grocery" "peace tea" 0.99 100)))
(check-expect (scale-prices
               student-shop
               (make-query 'dontcare
                           'dontcare
                           (make-interval 'dontcare 'dontcare)
                           (make-interval 'dontcare 'dontcare)) 0)
              (list (make-grocery "produce" "apple" 0 600)
                    (make-grocery "produce" "banana" 0 1000)
                    (make-grocery "grocery" "toilet paper" 0 850)
                    (make-grocery "produce" "orange" 0 4000)
                    (make-grocery "grocery" "tomato sauce" 0 400)
                    (make-grocery "produce" "lemon" 0 500)
                    (make-grocery "produce" "lime" 0 5000)
                    (make-grocery "dairy" "eggs" 0 50)
                    (make-grocery "produce" "potato" 0 250)
                    (make-grocery "produce" "corn" 0 275)
                    (make-grocery "dairy" "milk" 0 500)
                    (make-grocery "grocery" "tissues" 0 1000)
                    (make-grocery "dairy" "ice cream" 0 450)
                    (make-grocery "dairy" "cheese" 0 900)
                    (make-grocery "grocery" "chips" 0 800)
                    (make-grocery "grocery" "peace tea" 0 100)))
(check-expect (scale-prices
               student-shop
               (make-query 'dontcare
                           "apple"
                           (make-interval 'dontcare 'dontcare)
                           (make-interval 'dontcare 'dontcare)) 0)
              (list (make-grocery "produce" "apple" 0 600)
                    (make-grocery "produce" "banana" 0.95 1000)
                    (make-grocery "grocery" "toilet paper" 4.99 850)
                    (make-grocery "produce" "orange" 3.99 4000)
                    (make-grocery "grocery" "tomato sauce" 3.99 400)
                    (make-grocery "produce" "lemon" 2.49 500)
                    (make-grocery "produce" "lime" 2.99 5000)
                    (make-grocery "dairy" "eggs" 1.99 50)
                    (make-grocery "produce" "potato" 1.99 250)
                    (make-grocery "produce" "corn" 1.99 275)
                    (make-grocery "dairy" "milk" 0.49 500)
                    (make-grocery "grocery" "tissues" 5.99 1000)
                    (make-grocery "dairy" "ice cream" 0.69 450)
                    (make-grocery "dairy" "cheese" 6.49 900)
                    (make-grocery "grocery" "chips" 2.99 800)
                    (make-grocery "grocery" "peace tea" 0.99 100)))
(check-expect (scale-prices
               student-shop
               (make-query 'dontcare
                           'dontcare
                           (make-interval 0 1)
                           (make-interval 'dontcare 'dontcare)) 0)
              (list (make-grocery "produce" "apple" 2.49 600)
                    (make-grocery "produce" "banana" 0 1000)
                    (make-grocery "grocery" "toilet paper" 4.99 850)
                    (make-grocery "produce" "orange" 3.99 4000)
                    (make-grocery "grocery" "tomato sauce" 3.99 400)
                    (make-grocery "produce" "lemon" 2.49 500)
                    (make-grocery "produce" "lime" 2.99 5000)
                    (make-grocery "dairy" "eggs" 1.99 50)
                    (make-grocery "produce" "potato" 1.99 250)
                    (make-grocery "produce" "corn" 1.99 275)
                    (make-grocery "dairy" "milk" 0 500)
                    (make-grocery "grocery" "tissues" 5.99 1000)
                    (make-grocery "dairy" "ice cream" 0 450)
                    (make-grocery "dairy" "cheese" 6.49 900)
                    (make-grocery "grocery" "chips" 2.99 800)
                    (make-grocery "grocery" "peace tea" 0 100)))
(check-expect (scale-prices
               student-shop
               (make-query 'dontcare
                           'dontcare
                           (make-interval 'dontcare 'dontcare)
                           (make-interval 0 400)) 0)
              (list (make-grocery "produce" "apple" 2.49 600)
                    (make-grocery "produce" "banana" 0.95 1000)
                    (make-grocery "grocery" "toilet paper" 4.99 850)
                    (make-grocery "produce" "orange" 3.99 4000)
                    (make-grocery "grocery" "tomato sauce" 0 400)
                    (make-grocery "produce" "lemon" 2.49 500)
                    (make-grocery "produce" "lime" 2.99 5000)
                    (make-grocery "dairy" "eggs" 0 50)
                    (make-grocery "produce" "potato" 0 250)
                    (make-grocery "produce" "corn" 0 275)
                    (make-grocery "dairy" "milk" 0.49 500)
                    (make-grocery "grocery" "tissues" 5.99 1000)
                    (make-grocery "dairy" "ice cream" 0.69 450)
                    (make-grocery "dairy" "cheese" 6.49 900)
                    (make-grocery "grocery" "chips" 2.99 800)
                    (make-grocery "grocery" "peace tea" 0 100)))