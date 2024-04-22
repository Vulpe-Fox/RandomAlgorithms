;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname resource) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;
;;/----------------------------------\
;;    Cameron Carmichael (20881375)
;;    CS 135 Fall 2020
;;    Midterm 2 Question 6
;;\----------------------------------/
;;

;;A Resource is (anyof 'fire 'wood 'water)

(define-struct card (type power))
;;A Card is a (make-card Resource Nat)

;;'wood beats 'water
;;'water beats 'fire
;;'fire beats 'wood

;;An Outcome is (anyof 'player-1 'player-2 'tie-game)

;;(beats? resource-1 resource-2) produces whether or not resource-1 beats resource-2, where they are
;;   both different Resource
;;(winner card-1 card-2) determines the winner of a card duel
;;(winner-table cards) produces the nxn table of winners of a list of cards of size n
;;(winner-column remaining-cards cards) produces the column list of winners for winner-table
;;(winner-row card cards) produces the row list of winners for each element of winner-column
;;(cheat cards) increments power of any card in a listof cards with power less than or equal to 5 by 2

;;Test variables
(define testcards
  (list (make-card 'fire 10)
        (make-card 'wood 23)
        (make-card 'water 3)
        (make-card 'fire 20)))

;;Examples
(check-expect (beats? 'fire 'water) false)

(check-expect (winner (make-card 'wood 8) (make-card 'fire 1)) 'player-2)
(check-expect (winner (make-card 'wood 8) (make-card 'wood 3)) 'player-1)

(check-expect (winner-table testcards)
              (list (list 'tie-game 'player-1 'player-2 'player-2)
                    (list 'player-2 'tie-game 'player-1 'player-2)
                    (list 'player-1 'player-2 'tie-game 'player-1)
                    (list 'player-1 'player-1 'player-2 'tie-game)))

(check-expect (cheat testcards)
              (list (make-card 'fire 10)
                    (make-card 'wood 23)
                    (make-card 'water 5)
                    (make-card 'fire 20)))

;;(beats? resource-1 resource-2) Resource + Resource -> Bool
;;resource-1 and resource-2 are different
(define (beats? resource-1 resource-2)
  (cond[(and (symbol=? resource-1 'wood) (symbol=? resource-2 'fire)) false]
       [(and (symbol=? resource-1 'fire) (symbol=? resource-2 'wood)) true]
       [(and (symbol=? resource-1 'fire) (symbol=? resource-2 'water)) false]
       [(and (symbol=? resource-1 'water) (symbol=? resource-2 'fire)) true]
       [(and (symbol=? resource-1 'water) (symbol=? resource-2 'wood)) false]
       [(and (symbol=? resource-1 'wood) (symbol=? resource-2 'water)) true]))

;;(winner card-1 card-2) Card + Card -> Outcome
(define (winner card-1 card-2)
  (cond[(symbol=? (card-type card-1) (card-type card-2))
        (cond[(< (card-power card-1) (card-power card-2)) 'player-2]
             [(> (card-power card-1) (card-power card-2)) 'player-1]
             [else 'tie-game])]
       [(beats? (card-type card-1) (card-type card-2)) 'player-1]
       [else 'player-2]))

;;(winner-table cards) (listof Card) -> (listof (listof Outcome))
(define (winner-table cards)
  (winner-column cards cards))

;;(winner-column remaining-cards cards) (listof Card) + (listof Card) -> (listof (listof Outcome))
(define (winner-column remaining-cards cards)
  (cond[(empty? remaining-cards) empty]
       [else (cons (winner-row (first remaining-cards) cards)
                   (winner-column (rest remaining-cards) cards))]))

;;(winner-row card cards) card (listof Card) -> (listof Outcome)
(define (winner-row card cards)
  (cond[(empty? cards) empty]
       [else (cons (winner card (first cards))
                   (winner-row card (rest cards)))]))

;;(cheat cards) (listof Card) -> (listof Card)
(define (cheat cards)
  (cond[(empty? cards) empty]
       [(<= (card-power (first cards)) 5)
        (cons (make-card (card-type (first cards))
                         (+ (card-power (first cards)) 2))
              (cheat (rest cards)))]
       [else (cons (first cards) (cheat (rest cards)))]))

;;Tests
(check-expect (beats? 'fire 'wood) true)
(check-expect (beats? 'wood 'water) true)
(check-expect (beats? 'wood 'fire) false)
(check-expect (beats? 'water 'wood) false)
(check-expect (beats? 'water 'fire) true)

(check-expect (winner (make-card 'fire 8) (make-card 'water 1)) 'player-2)
(check-expect (winner (make-card 'fire 8) (make-card 'wood 1)) 'player-1)
(check-expect (winner (make-card 'wood 8) (make-card 'water 1)) 'player-1)
(check-expect (winner (make-card 'water 8) (make-card 'wood 1)) 'player-2)
(check-expect (winner (make-card 'water 8) (make-card 'fire 1)) 'player-1)
(check-expect (winner (make-card 'wood 8) (make-card 'wood 8)) 'tie-game)
(check-expect (winner (make-card 'wood 3) (make-card 'wood 8)) 'player-2)

(check-expect (winner-table empty) empty)

(check-expect (cheat empty) empty)