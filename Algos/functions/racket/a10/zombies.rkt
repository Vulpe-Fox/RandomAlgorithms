#lang racket

;;
;;/----------------------------------\
;;    Cameron Carmichael (20881375)
;;    CS 135 Fall 2020
;;    Assignment 10
;;\----------------------------------/
;;

;; A Location is a Nat

;; A Town is a (listof (list Location (listof Location)))
;; Requires: Town represents a valid graph as defined in Module 16
(define waterloo '((0 (1 2 3))
                   (1 (2 3))
                   (2 (0 4))
                   (3 (1))
                   (4 (5))
                   (5 (3))))

;; A Horde is a (listof (list Location Nat))

;;Examples
(check-expect (infect waterloo 1000)
              (list (list 0 1000) (list 1 1000) (list 2 1000)
                    (list 3 1000) (list 4 1000) (list 5 1000)))

(check-expect (sink (infect waterloo 1000))
              (list 300 (list (list 0 950) (list 1 950)
                              (list 2 950) (list 3 950)
                              (list 4 950) (list 5 950))))

(check-expect (apportion 100 3) (list 34 33 33))

(check-expect (shamble waterloo (list (list 0 950) (list 1 950) (list 2 950)
                                      (list 3 950) (list 4 950) (list 5 950)))
              (list (list 0 475) (list 1 1266) (list 2 792)
                    (list 3 1742) (list 4 475) (list 5 950)))

(check-expect (rise 300 braaaaaaains)
              (list (list 0 525) (list 1 1316) (list 2 842) (list 3 1792) (list 4 525) (list 5 1000)))

(check-expect (night waterloo (infect waterloo 1000))
              (list (list 0 525) (list 1 1316) (list 2 842)
                    (list 3 1792) (list 4 525) (list 5 1000)))

(check-expect (apocalypse waterloo 1000 3)
              (list (list 0 449) (list 1 1893) (list 2 1104)
                    (list 3 1627) (list 4 450) (list 5 478)))

;;Functions

;;(infect town zombies) Town + Nat -> Horde
(define (infect town zombies)
  (cond[(empty? town) empty]
       [else (cons (list (first (first town)) zombies)
                   (infect (rest town) zombies))]))

;;(sink horde) Horde -> (list Nat Horde)
(define (sink horde)
  (local[(define calc-sunk
           (round (foldr (lambda (element rror) (+ (/ (second element) 20) rror)) 0 horde)))
         (define calc-new-values
           (foldr (lambda (element rror)
                    (cons (list (first element) (round (/ (* 19 (second element)) 20))) rror))
                  '() horde))]
    (list calc-sunk calc-new-values)))

;;(apportion zombies n) Num + Nat -> (Listof Num)
(define (apportion zombies n)
  (local[(define divided-val (cond[(zero? n) 0]
                                  [else (/ zombies n)]))
         (define divided-dec (- divided-val (floor divided-val)))
         (define divided-ciel (ceiling divided-val))
         (define divided-floor (floor divided-val))]
    (cond[(zero? n) empty]
         [(< divided-dec 0.5)
          (cons divided-ciel (apportion (- zombies divided-ciel) (- n 1)))]
         [else (cons divided-floor (apportion (- zombies divided-floor) (- n 1)))])))

;;(shamble town horde) Town + Horde -> Horde
;;Requires: Locations in town and horde are well-ordered
(define (shamble town horde)
  (local[(define apports
           (foldr (lambda (t h rror) (cons (map (lambda (x y) (list x y))
                                                (second t) (apportion (second h) (length (second t))))
                                           rror)) '() town horde))
         (define (add-apports-to-horde curr-apports horde)
           (cond[(empty? curr-apports) horde]
                [else (add-apports-to-horde (rest curr-apports)
                                            (add-set-to-horde (first curr-apports) horde))]))
         (define (add-set-to-horde set horde)
           (cond[(empty? set) horde]
                [else (add-set-to-horde (rest set)
                                        (add-to-horde (first set) horde))]))
         (define (add-to-horde element horde)
           (cond[(empty? horde) empty]
                [(zero? (first element)) (cons (list (first (first horde))
                                                     (+ (second (first horde)) (second element)))
                                               (add-to-horde (list (sub1 (first element))
                                                                   (second element))
                                                             (rest horde)))]
                [else (cons (first horde) (add-to-horde (list (sub1 (first element))
                                                              (second element))
                                                        (rest horde)))]))
         (define (subtract-original-horde origin new)
           (cond[(empty? origin) empty]
                [else (cons (list (first (first origin)) (- (second (first new))
                                                            (second (first origin))))
                            (subtract-original-horde (rest origin) (rest new)))]))]
    (subtract-original-horde horde (add-apports-to-horde apports horde))))

;;(rise zombies horde) Nat + Horde -> Horde
(define (rise zombies horde)
  (foldr (lambda (h app rror) (cons (list (first h) (+ (second h) app)) rror))
         '() horde (apportion zombies (length horde))))

;;(night town horde) Town + Horde -> Horde
;;Requires: Locations in town and horde are well-ordered
(define (night town horde)
  (local[(define sink-horde (sink horde))
         (define shamble-horde (shamble town (second sink-horde)))
         (define rise-horde (rise (first sink-horde) shamble-horde))]
    rise-horde))

;;(apocalypse town infection nights) Town + Nat + Nat -> Horde
;;Requires: Locations in town are well-ordered
(define (apocalypse town infection nights)
  (local[(define (next-horde horde) (night town horde))
         (define (night-passer curr-horde remaining-nights)
           (cond[(= remaining-nights 0) curr-horde]
                [else (night-passer (next-horde curr-horde) (sub1 remaining-nights))]))]
    (night-passer (infect town infection) nights)))

;;Tests variables
(define braaaaains (second (sink (infect waterloo 1000))))
(define braaaaaaains (shamble waterloo braaaaains))

;;Tests
(check-expect (infect waterloo 0)
              (list (list 0 0) (list 1 0) (list 2 0)
                    (list 3 0) (list 4 0) (list 5 0)))

(check-expect (sink (infect waterloo 0))
              (list 0 (list (list 0 0) (list 1 0)
                            (list 2 0) (list 3 0)
                            (list 4 0) (list 5 0))))

(check-expect (apportion 100 0) '())
(check-expect (apportion 0 3) '(0 0 0))

(check-expect (shamble waterloo (list (list 0 0) (list 1 0) (list 2 0)
                                      (list 3 0) (list 4 0) (list 5 0)))
              (list (list 0 0) (list 1 0) (list 2 0)
                    (list 3 0) (list 4 0) (list 5 0)))

(check-expect (rise 0 braaaaaaains)
              braaaaaaains)

(check-expect (night waterloo (infect waterloo 0))
              (list (list 0 0) (list 1 0) (list 2 0)
                    (list 3 0) (list 4 0) (list 5 0)))

(check-expect (apocalypse waterloo 1000 0)
              (infect waterloo 1000))
(check-expect (apocalypse waterloo 1000 31)
              (list (list 0 547) (list 1 1730) (list 2 1045)
                    (list 3 1586) (list 4 546) (list 5 569)))
