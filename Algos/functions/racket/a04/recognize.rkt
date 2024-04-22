;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname recognize) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;;
;;/----------------------------------\
;;    Cameron Carmichael (20881375)
;;    CS 135 Fall 2020
;;    Assignment 04, Problem 3
;;\----------------------------------/
;;

;;
;; ***************************************************
;; Starter Code
;; ***************************************************
;;

(require "templates.rkt")

;; "templates.rkt" provides templates, a TemplateLibrary (see data definition)
;; It also provides the following test gestures for your recognizer: 
;;    testd testk tests testy testa testt


;; A Point is a (list Num Num)

;; A Gesture is a (listof (list Num Num))

;; A BoundingBox (BB) is a (list Point Point)
;; requires: the coordinate values in the first point are less than the
;;             respective values in the second point

;; A TemplateLibrary (TL) is a (listof (list Sym Gesture))
;; requires: the list is non-empty
;;           each Sym key is unqiue
;;           each Gesture value is not both vertical and horizontal
       



;; 3a)
;; These are helper functions. See assignment for design recipe requirements.

;;(translate-gesture gesture x-offset y-offset) produces a new Gesture translated by a
;;   specific amount determined by x-offset, y-offset
;;(scale-gesture gesture x-scale y-scale) produces a new gesture scaled by a
;;   specific amount determined by x-scale, y-scale
;;(calc-individual-point-translation point x-offset y-offset) translates an
;;   individual point determined by x-offset, y-offset
;;(scale-gesture gesture x-scale y-scale) scales an
;;   individual point determined by x-scale, y-scale
;;(get-b-box gesture) produces the bounding box of a Gesture
;;(calc-individual-point-translation point x-offset y-offset) produces the translation
;;   for a single point
;;(calc-individual-point-scale point x-scale y-scale) produces the scale for a single point
;;(get-x point) produces the first coordinate in a list of coordinate points (x)
;;(get-y point) produces the second coordinate in a list of coordinate points (y)
;;(min-position gesture) produces the point which is a combination of the minimum x and y
;;(max-position gesture) produces the point which is a combination of the maximum x and y
;;(calc-min-x gesture) produces the minimum x of a gesture
;;(calc-min-y gesture) produces the minimum y of a gesture
;;(calc-max-x gesture) produces the maximum x of a gesture
;;(calc-max-y gesture) produces the maximum y of a gesture

;;Examples
(check-expect (get-x (list 4 5)) 4)
(check-expect (get-y (list 4 5)) 5)

(check-expect (translate-gesture (list (list 4 5)) 1 1) (list (list 5 6)))
(check-expect (translate-gesture (list (list 4 5) (list 5 6)) 1 1)
              (list (list 5 6) (list 6 7)))

(check-expect (scale-gesture (list (list 4 5)) 2 1) (list (list 8 5)))
(check-expect (scale-gesture (list (list 4 5) (list 5 6)) 2 1)
              (list (list 8 5) (list 10 6)))

(check-expect (get-b-box (list (list 4 5))) (list (list 4 5) (list 4 5)))
(check-expect (get-b-box (list (list 4 5) (list 3 6))) (list (list 3 5) (list 4 6)))

;;(translate-gesture gesture x-offset y-offset)
;;Gesture + Num + Num -> Gesture
(define (translate-gesture gesture x-offset y-offset)
  (cond[(empty? gesture) empty]
       [else (cons (calc-individual-point-translation (first gesture) x-offset y-offset)
                   (translate-gesture (rest gesture) x-offset y-offset))]))

;;(scale-gesture gesture x-scale y-scale)
;;Gesture + Num + Num -> Gesture
(define (scale-gesture gesture x-scale y-scale)
  (cond[(empty? gesture) empty]
       [else (cons (calc-individual-point-scale (first gesture) x-scale y-scale)
                   (scale-gesture (rest gesture) x-scale y-scale))]))

;;(get-b-box gesture) Gesture -> (list Point Point)
;;gesture must be non-empty
(define (get-b-box gesture)
  (list (min-position gesture) (max-position gesture)))

;;(calc-individual-point-translation point x-offset y-offset)
;;Point + Num + Num -> Point
(define (calc-individual-point-translation point x-offset y-offset)
  (list (+ (get-x point) x-offset) (+ (get-y point) y-offset)))

;;(calc-individual-point-scale point x-scale y-scale)
;;Point + Num + Num -> Point
(define (calc-individual-point-scale point x-scale y-scale)
  (list (* (get-x point) x-scale) (* (get-y point) y-scale)))

;;(get-x point) Point -> Num
;;nLst contains 2 elements
(define (get-x point)
  (first point))

;;(get-y point) Point -> Num
;;nLst contains 2 elements
(define (get-y point)
  (first (rest point)))

;;(min-position gesture) Gesture -> Point
(define (min-position gesture)
  (list (calc-min-x gesture) (calc-min-y gesture)))

;;(max-position gesture) Gesture -> Point
(define (max-position gesture)
  (list (calc-max-x gesture) (calc-max-y gesture)))

;;(calc-min-x gesture) Gesture -> Num
(define (calc-min-x gesture)
  (cond[(empty? (rest gesture)) (get-x (first gesture))]
       [else (min (get-x (first gesture)) (calc-min-x (rest gesture)))]))

;;(calc-min-y gesture) Gesture -> Num
(define (calc-min-y gesture)
  (cond[(empty? (rest gesture)) (get-y (first gesture))]
       [else (min (get-y (first gesture)) (calc-min-y (rest gesture)))]))

;;(calc-max-x gesture) Gesture -> Num
(define (calc-max-x gesture)
  (cond[(empty? (rest gesture)) (get-x (first gesture))]
       [else (max (get-x (first gesture)) (calc-max-x (rest gesture)))]))

;;(calc-max-y gesture) Gesture -> Num
(define (calc-max-y gesture)
  (cond[(empty? (rest gesture)) (get-y (first gesture))]
       [else (max (get-y (first gesture)) (calc-max-y (rest gesture)))]))

;; 3b)
;; Full design recipe required.

;;(gesture-length gesture) produces the total length of a Gesture
;;(get-points g nats) produces the a new Gesture containing the points in g
;;(get-specific-point g index) produces the point at the index in g
;;(calc-distance point1 point2) produces the distance between two points in a Gesture

;;Test variables
(define testgest0 (list (list 100 0) (list 200 100) (list 100 200)(list 0 100) (list 100 50)))
(define testgest1 (list (list 0 0) (list 0 0) (list 0 0)(list 0 100) (list 0 0)))

;;Examples
(check-within (gesture-length (list (list 4 5))) 0 0.1)
(check-within (gesture-length (list (list 4 5) (list 5 6))) 1.41 0.1)

(check-expect (get-points (list (list 4 5)) (list 0)) (list (list 4 5)))
(check-expect (get-points (list (list 4 5) (list 5 6)) (list 1 0)) (list (list 5 6) (list 4 5)))

;;(gesture-length gesture) Gesture -> Num
(define (gesture-length gesture)
  (cond[(or (empty? gesture) (empty? (rest gesture))) 0]
       [else (+ (calc-distance (first gesture) (first (rest gesture)))
                (gesture-length (rest gesture)))]))

;;(get-points g nats) Gesture + natLst -> Gesture
;;values in nats must be in the range of [0...n-1] such that n is the size of g
;;g is not empty
(define (get-points g nats)
  (cond[(empty? nats) empty]
       [else (cons (get-specific-point g (first nats))
                   (get-points g (rest nats)))]))

;;(get-specific-point g index) Gesture + Num -> Point
(define (get-specific-point g index)
  (cond[(= index 0) (first g)]
       [else (get-specific-point (rest g) (- index 1))]))

;;(calc-distance point1 point2) Point + Point -> Num
(define (calc-distance point1 point2)
  (sqrt (+ (sqr (- (get-x point2) (get-x point1)))
           (sqr (- (get-y point2) (get-y point1))))))

;;Tests
(check-within (gesture-length (list (list 4 5) (list 5 6) (list 4 5))) 2.83 0.1)
(check-within (gesture-length (list (list 4 5) (list 4 5))) 0 0.1)
(check-within (gesture-length (list )) 0 0.1)

(check-expect (get-points testgest0 (list 0 1 2 2 4 4))
              (list (list 100 0) (list 200 100) (list 100 200)
                    (list 100 200) (list 100 50) (list 100 50)))
(check-expect (get-points testgest1 (list 0 1 2 4 3))
              (list (list 0 0) (list 0 0) (list 0 0)(list 0 0) (list 0 100)))

;; 3c) Starter code definitions

;; 3ci)

;;(five-sample gesture) produces a sampling of Gesture 5 points
;;  the first, n/4th, n/2th, 3n/4th, and last point.
;;(calc-size gesture) produces the size of a gesture

;; Examples:
(check-expect (five-sample (list (list 1 1) (list 2 2)))
              (list (list 1 1) (list 1 1) (list 2 2) (list 2 2) (list 2 2)))
(check-expect (five-sample (list (list 1 1) (list 2 2) (list 3 3) (list 4 4)
                                (list 5 5) (list 6 6) (list 7 7) (list 8 8)))
              (list (list 1 1) (list 3 3) (list 5 5) (list 7 7) (list 8 8)))

;; five-sample: Gesture -> Gesture
;; requires: gesture is non-empty
(define (five-sample gesture)
  (get-points gesture (list 0 (floor (* 0.25 (calc-size gesture))) (floor (* 0.5 (calc-size gesture)))
                      (floor (* 0.75 (calc-size gesture))) (- (calc-size gesture) 1))))

;;(calc-size gesture) Gesture -> Num
(define (calc-size gesture)
  (cond[(empty? gesture) 0]
       [else (+ (calc-size (rest gesture)) 1)]))

;; Tests:
(check-expect (five-sample (list (list 1 1) (list 2 2) (list 3 3) (list 4 4)))
              (list (list 1 1) (list 2 2) (list 3 3) (list 4 4) (list 4 4)))
(check-expect (five-sample (list (list 1 1)))
              (list (list 1 1) (list 1 1) (list 1 1) (list 1 1) (list 1 1)))
(check-expect (five-sample (list (list 1 1) (list 2 2) (list 3 3) (list 4 4)
                                 (list 5 5)))
              (list (list 1 1) (list 2 2) (list 3 3) (list 4 4) (list 5 5)))

;; 3cii)

;;(move-and-scale gesture x-scale y-scale) moves gesture to (0, 0) and
;;  scales it by (x-scale)x(y-scale)

;; Examples:
(check-expect (move-and-scale (list (list 1 1)) 1 1) (list (list 0 0)))
(check-expect (move-and-scale (list (list 1 5) (list 3 4)) 1 2)
              (list (list 0 2) (list 2 0)))

;; move-and-scale: Gesture Num Num -> Gesture
;; requires: gesture is non-empty
;;           x-scale > 0
;;           y-scale > 0
(define (move-and-scale gesture x-scale y-scale)
  (scale-gesture (translate-gesture gesture (- (get-x (first (get-b-box gesture))))
                                    (- (get-y (first (get-b-box gesture))))) x-scale y-scale))

;; Test:
(check-expect (move-and-scale (list (list 5 5) (list 2 2)) 3 0.5)
              (list (list 9 1.5) (list 0 0)))

;; 3ciii)

(define min-width 30)
(define min-height 30)
(define norm-size 200)

;;(normalize-gesture gesture) normalizes gesture to (0,0) and a standard size

;; Examples:
(check-within (normalize-gesture (list (list 0 0) (list 100 100)))
              (list (list 0 0) (list 200 200)) 0.01)
(check-within (normalize-gesture (list (list 100 0) (list 100 50) (list 200 50)))
              (list (list 0 0) (list 0 200) (list 200 200)) 0.01)

;; normalize-gesture: Gesture -> Gesture
;; requires: gesture is not both vertical and horizontal
;;           gesture is non-empty

(define (normalize-gesture gesture)
  (cond[(< (- (get-x (second (get-b-box gesture))) (get-x (first (get-b-box gesture)))) min-width)
        (move-and-scale gesture 1 (/ norm-size (- (get-y (second (get-b-box gesture)))
                                        (get-y (first (get-b-box gesture))))))]
       [(< (- (get-y (second (get-b-box gesture))) (get-y (first (get-b-box gesture)))) min-height)
        (move-and-scale gesture (/ norm-size (- (get-x (second (get-b-box gesture)))
                                        (get-x (first (get-b-box gesture))))) 1)]
       [else (move-and-scale gesture (/ norm-size (- (get-x (second (get-b-box gesture)))
                                        (get-x (first (get-b-box gesture)))))
                                     (/ norm-size (- (get-y (second (get-b-box gesture)))
                                        (get-y (first (get-b-box gesture))))))]))

;; Tests:
(check-within (normalize-gesture (list (list 0 0) (list 100 30)))
              (list (list 0 0) (list 200 200)) 0.01)
(check-within (normalize-gesture (list (list 0 0) (list 100 29)))
              (list (list 0 0) (list 200 29)) 0.01)
(check-within (normalize-gesture (list (list 0 0) (list 30 100)))
              (list (list 0 0) (list 200 200)) 0.01)
(check-within (normalize-gesture (list (list 0 0) (list 29 100)))
              (list (list 0 0) (list 29 200)) 0.01)
(check-within (normalize-gesture (list (list 0 0) (list 400 400)))
              (list (list 0 0) (list 200 200)) 0.01)

;; 3civ)

(define k0 5)

;;(geometric-5match gesture1 gesture2) produces the average distance between
;;  points in sub-sampled gesture1 and gesture2 after sub-sampling them with k0 points
;;(five-sampled-geometric-5match gesture1 gesture2) produces the average distance between points in 
;;   two normalized, five-sampled gestures
;;(distance-between point1 point2) produces the distance between two points by determining the
;;   Euclidean Norm

;; Examples:
(check-within (geometric-5match
               (list (list 10 10) (list 30 30) (list 50 50) (list 70 70) (list 80 80))
               (list (list 10 10) (list 20 20) (list 30 30) (list 40 40) (list 40 40)))
               16.16 0.01)

;; geometric-5match: Gesture Gesture -> Num
;; requires: gesture1 and gesture2 are each not both vertical and horizontal
(define (geometric-5match gesture1 gesture2)
  (/ (five-sampled-geometric-5match (normalize-gesture (five-sample gesture1))
                                    (normalize-gesture (five-sample gesture2))) k0))

;;(five-sampled-geometric-5match gesture1 gesture2) Gesture + Gesture -> Num
(define (five-sampled-geometric-5match gesture1 gesture2)
  (cond[(empty? gesture1) 0]
       [else (+ (distance-between (first gesture1) (first gesture2))
                (five-sampled-geometric-5match (rest gesture1) (rest gesture2)))]))

;;(distance-between point1 point2) Point + Point -> Num
(define (distance-between point1 point2)
  (sqrt (+ (sqr (- (get-x point2) (get-x point1))) (sqr (- (get-y point2) (get-y point1))))))

;; Tests:
(check-within (geometric-5match
               (list (list 10 10) (list 20 20) (list 30 30) (list 40 40) (list 40 40))
               (list (list 10 10) (list 20 20) (list 30 30) (list 40 40) (list 40 40)))
               0 0.01)
(check-within (geometric-5match
               (list (list 10 10) (list 20 20) (list 30 30) (list 40 40) (list 40 40))
               (list (list 10 10) (list 20 20) (list 30 30) (list 40 40) (list 40 40)))
               0 0.01)
;;No one needs to know how long the next one took me to solve, but at least the second list is just
;;   a diagonal line across the 200x200 grid, so the next case would have the same answer
(check-within (geometric-5match
               (list (list 0 0) (list 100 29))
               (list (list 10 10) (list 20 20) (list 30 30) (list 40 40) (list 40 40)))
               112.02 0.01)
(check-within (geometric-5match
               (list (list 0 0) (list 29 100))
               (list (list 10 10) (list 20 20) (list 30 30) (list 40 40) (list 40 40)))
               112.02 0.01)

;; 3cv)

;;(five-point-rec candidate template-library) produces the symbol in
;;  template-library closest to candidate
;;(compare-gesture-matches gesture0 gesture1 gesture2) produces the gesture with the closest match of
;;   gesture1/2 to gesture0

;; Examples:
(check-expect (five-point-rec testd templates) 'd)
(check-expect (five-point-rec testk templates) 'k)

;; five-point-rec Gesture TL -> Sym
;; requires: candidate is not both vertical and horizontal
(define (five-point-rec candidate template-list)
  (cond[(empty? (rest template-list)) (first (first template-list))]
       [else (five-point-rec candidate (cons (compare-gesture-matches candidate
                                                                    (first template-list)
                                                                    (first (rest template-list)))
                                           (rest (rest template-list))))]))

;;(compare-gesture-matches gesture0 gesture1 gesture2) Gesture + Gesture + Gesture -> Gesture
(define (compare-gesture-matches gesture0 gesture1 gesture2)
  (cond[(< (geometric-5match gesture0 (second gesture1))
           (geometric-5match gesture0 (second gesture2))) gesture1]
       [else gesture2]))

;; Tests
(check-expect (five-point-rec testt templates) 't)
(check-expect (five-point-rec testa templates) 'a)
(check-expect (five-point-rec tests templates) 's)
(check-expect (five-point-rec testy templates) 'y)

;; 3d)

;;(sub-sample gesture) produces a sampling of Gesture k points
;;   0, n/(k-1), 2n/(k-1),...,n
;;(create-sub-sample index size) produces the list of positions to sub-sample from
;;(geometric-match gesture1 gesture2 k) produces the average distance between
;;  points in sub-sampled gesture1 and gesture2 after sub-sampling them with k0 points
;;(k-sampled-geometric-match gesture1 gesture2) produces the average distance between points in 
;;   two normalized, k-sampled gestures
;;(k-point-rec candidate template-library k) produces the symbol in
;;  template-library closest to candidate using k points
;;(compare-gesture-matches-with-k gesture0 gesture1 gesture2 k) produces the gesture with the closest
;;   match of gesture1/2 to gesture0 using k points

;;Examples
(check-expect (sub-sample (list (list 1 1) (list 2 2)) 5)
              (list (list 1 1) (list 1 1) (list 2 2) (list 2 2) (list 2 2)))
(check-expect (sub-sample (list (list 1 1) (list 2 2) (list 3 3) (list 4 4)
                                (list 5 5) (list 6 6) (list 7 7) (list 8 8)) 5)
              (list (list 1 1) (list 3 3) (list 5 5) (list 7 7) (list 8 8)))

(check-within (geometric-match
               (list (list 10 10) (list 30 30) (list 50 50) (list 70 70) (list 80 80))
               (list (list 10 10) (list 20 20) (list 30 30) (list 40 40) (list 40 40)) 5)
               16.16 0.01)

(check-expect (k-point-rec testd templates 3) 'd)
(check-expect (k-point-rec testk templates 7) 'k)

;;(sub-sample gesture) Gesture + Nat -> Gesture
;; requires: gesture is non-empty
;; k > 2
(define (sub-sample gesture k)
  (get-points gesture (create-sub-sample 0 k (calc-size gesture))))

;;(create-sub-sample index size) Num + Nat + Num -> nLst
;;Requires index < size
(define (create-sub-sample index size-points size-gesture)
  (cond[(= index (- size-points 1)) (cons (- size-gesture 1) empty)]
       [else (cons (floor (* (/ index (- size-points 1)) size-gesture))
                   (create-sub-sample (+ index 1) size-points size-gesture))]))

;; geometric-match: Gesture + Gesture + Nat -> Num
;; requires: gesture1 and gesture2 are each not both vertical and horizontal
;; gesture1 and gesture2 are same size
;; k > 2
(define (geometric-match gesture1 gesture2 k)
  (/ (k-sampled-geometric-match (normalize-gesture gesture1)
                                (normalize-gesture gesture2)) k))

;;(k-sampled-geometric-match gesture1 gesture2) Gesture + Gesture -> Num
(define (k-sampled-geometric-match gesture1 gesture2)
  (cond[(empty? gesture1) 0]
       [else (+ (distance-between (first gesture1) (first gesture2))
                (k-sampled-geometric-match (rest gesture1) (rest gesture2)))]))

;; k-point-rec Gesture + TL + Nat -> Sym
;; requires: candidate is not both vertical and horizontal
;; k > 2
(define (k-point-rec candidate template-list k)
  (cond[(empty? (rest template-list)) (first (first template-list))]
       [else (k-point-rec candidate (cons (compare-gesture-matches-with-k candidate
                                                                    (first template-list)
                                                                    (first (rest template-list)) k)
                                          (rest (rest template-list))) k)]))

;;(compare-gesture-matches-with-k gesture0 gesture1 gesture2 k)
;;Gesture + Gesture + Gesture + Nat -> Gesture
(define (compare-gesture-matches-with-k gesture0 gesture1 gesture2 k)
  (cond[(< (geometric-match (sub-sample gesture0 k) (sub-sample (second gesture1) k) k)
           (geometric-match (sub-sample gesture0 k) (sub-sample (second gesture2) k) k)) gesture1]
       [else gesture2]))

;;Tests
(check-expect (sub-sample (list (list 1 1)) 8)
              (list (list 1 1) (list 1 1) (list 1 1) (list 1 1)
                    (list 1 1) (list 1 1) (list 1 1) (list 1 1)))

(check-within (geometric-match
               (list (list 0 0) (list 0 0) (list 100 29) (list 100 29) (list 100 29))
               (list (list 10 10) (list 20 20) (list 30 30) (list 40 40) (list 40 40)) 5)
               112.02 0.01)
(check-within (geometric-match
               (list (list 0 0) (list 0 0) (list 100 29) (list 100 29) (list 100 29))
               (list (list 10 10) (list 20 20) (list 30 30) (list 40 40) (list 40 40)) 5)
               112.02 0.01)
(check-within (geometric-match
               (list (list 0 0) (list 0 0) (list 0 0) (list 0 0) (list 29 100) (list 29 100)
                     (list 29 100) (list 29 100))
               (list (list 10 10) (list 10 10) (list 20 20) (list 30 30) (list 30 30) (list 40 40)
                     (list 40 40) (list 40 40)) 8)
               114.96 0.01)

(check-expect (k-point-rec testt templates 5) 't)
(check-expect (k-point-rec testa templates 26) 'a)
(check-expect (k-point-rec tests templates 4) 's)
(check-expect (k-point-rec testy templates 6) 'y)

;;Bonus

;;(spatial-rec candidate template-list k) produces the symbol in
;;   template-library closest to candidate using k equidistant points along candidate
;;(compare-gesture-matches-spatial gesture0 gesture1 gesture2 k) produces the gesture with the closest
;;   match of gesture1/2 to gesture0 using k equidistant points along the gestures
;;(spatial-sub-sample gesture k) produces a subsample of a Gesture based on distance with the number
;;   of points: k
;;(calc-distance-sub-points gesture step substep remaining-distance) produces the subpoints for
;;   spatial-sub-sample
;;   (if the remaining distance is 0, the process is done)
;;   (if distance between first two points and leftover distance is greater than a step, subtracts
;;   a step from the remaining distance and makes a new point to mark the location)
;;   (if equal, removes a point (at which point there is a double point) and subtracts a step from
;;   the remaining distance)
;;   (if less than, adds the last distance between points as a leftover substep distance and moves on
;;   with next points)
;;(calc-new-point gesture step substep) produces a new point which exists at the end of a step of
;;   distance
;;(distance-between-first-two-points gesture) produces the distance between the first two points
;;   in a Gesture

;;Test variables
(define perfect-square (list (list 50 100) (list 250 100)(list 250 300) (list 50 300) (list 50 100)))
(define perfect-square-diag
  (list (list 0 0) (list 50 50) (list 100 0) (list 50 -50) (list 0 0)))

;;Examples
(check-within (spatial-sub-sample perfect-square 4)
              (list (list 50 100) (list 250 166.66) (list 116.66 300)(list 50 100)) 0.01)

(check-expect (spatial-rec testa templates 26) 'a)
(check-expect (spatial-rec testk templates 7) 'k)

;; spatial-rec Gesture + TL + Nat -> Sym
;; requires: candidate is not both vertical and horizontal
;; k > 2
(define (spatial-rec candidate template-list k)
  (cond[(empty? (rest template-list)) (first (first template-list))]
       [else (spatial-rec candidate (cons (compare-gesture-matches-spatial candidate
                                                                    (first template-list)
                                                                    (first (rest template-list)) k)
                                          (rest (rest template-list))) k)]))

;;(compare-gesture-matches-spatial gesture0 gesture1 gesture2 k)
;;Gesture + Gesture + Gesture + Nat -> Gesture
(define (compare-gesture-matches-spatial gesture0 gesture1 gesture2 k)
  (cond[(< (geometric-match (spatial-sub-sample gesture0 k)
                            (spatial-sub-sample (second gesture1) k) k)
           (geometric-match (spatial-sub-sample gesture0 k)
                            (spatial-sub-sample (second gesture2) k) k)) gesture1]
       [else gesture2]))

;;(spatial-sub-sample gesture k) Gesture + Nat -> Gesture
;;gesture is not empty
;;k>2
(define (spatial-sub-sample gesture k)
  (cons (first gesture)
   (calc-distance-sub-points gesture
                             (/ (gesture-length gesture) (- k 1))
                             0
                             (gesture-length gesture))))

;;(calc-distance-sub-points gesture step substep remaining-distance)
;;Gesture + Num + Num + Num -> Gesture
(define (calc-distance-sub-points gesture step substep remaining-distance)
  (cond[(or (= remaining-distance 0) (empty? (rest gesture))) empty]
       [(> (+ (distance-between-first-two-points gesture) substep) step)
        (cons (calc-new-point gesture step substep)
              (calc-distance-sub-points (cons (calc-new-point gesture step substep)
                                              (rest gesture))
                                        step 0 (- remaining-distance step)))]
       [(= (+ (distance-between-first-two-points gesture) substep) step)
        (cons (list (get-x (first (rest gesture)))
                    (get-y (first (rest gesture))))
              (calc-distance-sub-points (rest gesture) step 0 (- remaining-distance step)))]
       [else (calc-distance-sub-points (rest gesture) step
                                       (+ substep (distance-between-first-two-points gesture))
                                       remaining-distance)]))

;;(calc-new-point gesture step substep) Gesture + Num + Num -> Point
(define (calc-new-point gesture step substep) ;;This is where the last issue exists (it's just math)
       (list (+ (* (- (get-x (first (rest gesture))) (get-x (first gesture)))
                (/ (- step substep)
                   (distance-between-first-two-points gesture)))
                (get-x (first gesture)))
             (+ (* (- (get-y (first (rest gesture))) (get-y (first gesture)))
                (/ (- step substep)
                   (distance-between-first-two-points gesture)))
                (get-y (first gesture)))))

;;(distance-between-first-two-points gesture) Gesture -> Num
;;gesture must be of size 2 or greater
(define (distance-between-first-two-points gesture)
  (gesture-length (list (first gesture) (first (rest gesture)))))

;;Tests
(check-within (spatial-sub-sample perfect-square 7)
              (list (list 50 100) (list 183.33 100) (list 250 166.66)(list 250 300)
                    (list 116.66 300) (list 50 233.33) (list 50 100)) 0.01)
(check-within (spatial-sub-sample perfect-square-diag 5)
              perfect-square-diag 0.01)
(check-within (spatial-sub-sample perfect-square-diag 9)
              (list (list 0 0) (list 25 25) (list 50 50) (list 75 25) (list 100 0)
                    (list 75 -25) (list 50 -50) (list 25 -25) (list 0 0)) 0.01)

(check-expect (spatial-rec tests templates 4) 's)
(check-expect (spatial-rec testy templates 6) 'y)