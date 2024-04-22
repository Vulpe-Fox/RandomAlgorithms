;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname data-defs-05) (read-case-sensitive #t)
 (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; A (lsof X) is one of:
;; * 'nothing
;; * (make-ls X (lsof X))


(check-expect (ls-length
               (make-ls "!" (make-ls 'huh (make-ls 42 'nothing)))) 3)


(check-expect (ls-max (make-ls 5 (make-ls 9 (make-ls 7 'nothing)))) 9)


(define-struct grocery (dept name cost mass))
;; A Grocery is a (make-grocery Str Str Num Num)
;; Requires: cost >= 0, mass > 0.

;; A Store is a (listof Grocery)
;; Requires: no two items have both the same dept and same name.


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


(define-struct interval (lo hi))
;; An Interval is a (make-interval (anyof 'dontcare Num)
;;                                 (anyof 'dontcare Num))


(check-expect (in-interval? 42
      (make-interval 'dontcare 'dontcare)) true)
(check-expect (in-interval? 34
      (make-interval 35 'dontcare)) false)
(check-expect (in-interval? 34
      (make-interval 'dontcare 35)) true)


;; A StrPatt is a (anyof Str 'dontcare)


(define-struct query (dept name cost mass))
;; A GroceryQuery is a
;;   (make-query StrPatt StrPatt Interval Interval)


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


(define-struct silly-string (first middle last))
;; A SillyStringStruct is a (make-silly-string Char SillyStr Char)

;; A SillyStr is one of:
;; * empty
;; * a Char
;; * a SillyStringStruct


(check-expect (sillify "Babbage")
              (make-silly-string
               #\B
               (make-silly-string
                #\a
                (make-silly-string
                 #\b
                 #\b
                 #\a)
                #\g)
               #\e))
(check-expect (sillify "Lovelace")
              (make-silly-string
               #\L
               (make-silly-string
                #\o
                (make-silly-string
                 #\v
                 (make-silly-string
                  #\e
                  empty
                  #\l)
                 #\a)
                #\c)
               #\e))


(check-expect (unsillify
               (make-silly-string
                #\L
                (make-silly-string
                 #\o
                 (make-silly-string
                  #\v
                  (make-silly-string
                  #\e
                  empty
                  #\l)
                  #\a)
                 #\c)
                #\e))
              "Lovelace")


(check-expect (palindrome?
               (make-silly-string
                #\r
                (make-silly-string #\a #\d #\a)
                #\r)) true)
(check-expect (palindrome?
               (make-silly-string
                #\s
                (make-silly-string #\o #\n #\a)
                #\r)) false)
(check-expect (palindrome? (sillify "racecar")) true)
(check-expect (palindrome? (sillify "Koenigsegg")) false)


