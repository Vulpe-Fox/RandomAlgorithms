;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname duck) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;;
;;/----------------------------------\
;;    Cameron Carmichael (20881375)
;;    CS 135 Fall 2020
;;    Midterm 1 Question 3
;;\----------------------------------/
;;

;; Important symbols used in Q3:

;; 'small 'medium 'large
;; 'duck 'goose 'gull 'penguin 'crow 'sparrow 'squirrel 'emu

;;(what-animal swims? flies? angry? weight) produces a symbol of which animal it is
;;   A goose swims, flies, is angry, and is large/
;;   A gull swims, flies, is angry, and isn't large/
;;   A duck swims, flies, and isn't angry/
;;   A penguin swims, can't fly/
;;   A sparrow flies and is small/
;;   A squirrel is small/
;;   An emu is not small/
;;   A crow can fly and isn't small/
;;(goose? swims? flies? angry? weight) produces a boolean indicating whether something is a goose
;;   A goose swims, flies, is angry, and is large
;;(animal-size weight) produces a symbol according to the animal's weight in grams:
;;   'large if >10 kg
;;   'small if less than 500g
;;   'medium otherwise

;;Examples
(check-expect (animal-size 200000) 'large)

(check-expect (goose? true true true 200000) true)

(check-expect (what-animal true true true 200000) 'goose)

;;(what-animal swims? flies? angry? weight) Bool + Bool + Bool + Nat -> Sym
(define (what-animal swims? flies? angry? weight)
  (cond[(goose? swims? flies? angry? weight) 'goose]
       [(and swims? flies? angry?) 'gull]
       [(and swims? (not flies?)) 'penguin]
       [swims? 'duck]
       [(and flies? (equal? (animal-size weight) 'small)) 'sparrow]
       [flies? 'crow]
       [(equal? (animal-size weight) 'small) 'squirrel]
       [else 'emu]))

;;(goose? swims? flies? angry? weight) Bool + Bool + Bool + Nat -> Bool
(define (goose? swims? flies? angry? weight)
  (cond[(and swims? flies? angry? (equal? (animal-size weight) 'large)) true]
       [else false]))

;;(animal-size weight) Nat -> Sym
(define (animal-size weight)
  (cond[(> weight 10000) 'large]
       [(< weight 500) 'small]
       [else 'medium]))

;;Tests
(check-expect (animal-size 600) 'medium)
(check-expect (animal-size 200) 'small)

(check-expect (goose? true true false 200000) false)

(check-expect (what-animal true true false 200000) 'duck)
(check-expect (what-animal true true true 500) 'gull)
(check-expect (what-animal true false true 500) 'penguin)
(check-expect (what-animal false true true 200) 'sparrow)
(check-expect (what-animal false true false 600) 'crow)
(check-expect (what-animal false false false 200) 'squirrel)
(check-expect (what-animal false false false 500) 'emu)