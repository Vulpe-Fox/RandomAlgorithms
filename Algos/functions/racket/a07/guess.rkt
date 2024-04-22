;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname guess) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;;
;;/----------------------------------\
;;    Cameron Carmichael (20881375)
;;    CS 135 Fall 2020
;;    Assignment 07, Problem 2
;;\----------------------------------/
;;

(require "animals.rkt")

;; An Example is a (cons Sym (listof Sym))
;; Requires: each attribute in the rest is unique

;; A Histogram is a (listof (list Sym Nat))
;; Requires: A symbol can appear in only one pair.

;; An Augmented Histogram (AH) is a (listof (list Sym Nat Nat))
;; Requires: A symbol can appear in only one triple.

;; An Entropy Association List (EAL) is a (listof (list Sym Num))
;; Requires: A symbol can appear in only one pair.

;; A Decision Tree (DT) is one of:
;; *Bool
;; *(list Sym DT DT)

;;(collect-attributes examples) Determines all the known attributes in a set of examples
;;(split-examples examples symbol) Creates a list of:
;;   a list of examples which contain symbol
;;   a list of examples which don't contain symbol
;;(histogram examples)
;;(augment-histogram histogram attributes total) produces an AH from a histogram using a total num of
;;   entries
;;(entropy positive-counts negative-counts) Determines the entropy when comparing two members of an AH
;;(entropy-attributes positive negative) Determines the entropy for all attributes in two AH
;;(best-attribute attribute-entropies) Determines the attribute with the lowest entropy
;;(build-dt examples label) Produces a Decision Tree for a set of data for a label
;;(train-classifier) produces a predicate which can be used to determine what something is based on
;;   test data

;;A list of all attributes
(define all-attributes (list 'small 'medium 'large 'angry 'flies 'swims))

;;Test variables
(define seen
  (list
   (list 'squirrel 'small 'angry)
   (list 'goose 'large 'swims 'flies 'angry)
   (list 'goose 'large 'swims 'flies 'angry)
   (list 'crow 'medium 'flies 'angry)))

(define seen-spec
  (list
   (list 'squirrel 'small 'angry)
   (list 'goose 'large 'swims 'flies 'angry)
   (list 'goose 'large 'swims 'flies 'angry)
   (list 'nid)))

;;Examples
(check-expect (collect-attributes seen) (list 'small 'angry 'large 'swims 'flies 'medium))

(check-expect (split-examples seen 'goose)
              (list
               (list
                (list 'goose 'large 'swims 'flies 'angry)
                (list 'goose 'large 'swims 'flies 'angry))
               (list
                (list 'squirrel 'small 'angry)
                (list 'crow 'medium 'flies 'angry))))

(check-expect (histogram seen)
              (list (list 'small 1) (list 'angry 4) (list 'large 2)
                    (list 'swims 2) (list 'flies 3) (list 'medium 1)))

(check-expect
 (augment-histogram
  (list (list 'a 100) (list 'c 50))
  (list 'a 'b 'c)
  200)
 (list (list 'a 100 100) (list 'c 50 150) (list 'b 0 200)))
(check-expect
 (augment-histogram empty (list 'x 'y) 10)
 (list (list 'x 0 10) (list 'y 0 10)))

(check-within (entropy (list 'large 126 59) (list 'large 146 669)) 0.5664 0.001)
(check-within (entropy (list 'small 17 168) (list 'small 454 361)) 0.5826 0.001)
(check-within (entropy (list 'a 0 100) (list 'b 100 0)) 0 0.001)

(check-within (entropy-attributes
               (list
                (list 'large 126 59) (list 'angry 161 24)
                (list 'small 17 168) (list 'flies 170 15)
                (list 'swims 162 23) (list 'medium 42 143))
               (list
                (list 'large 146 669) (list 'angry 469 346)
                (list 'small 454 361) (list 'flies 615 200)
                (list 'swims 365 450) (list 'medium 215 600)))
              (list
               (list 'large #i0.5663948489858) (list 'angry #i0.6447688190492)
               (list 'small #i0.5825593868115) (list 'flies #i0.6702490498564)
               (list 'swims #i0.6017998773730) (list 'medium #i0.6901071708677))
              0.001)

(check-expect
 (best-attribute
 (list
  (list 'large #i0.5663948489858) (list 'angry #i0.6447688190492)
  (list 'small #i0.5825593868115) (list 'flies #i0.6702490498564)
  (list 'swims #i0.6017998773730) (list 'medium #i0.6901071708677))) 'large)

;;(collect-attributes examples) (ListOf Example) -> (ListOf Sym)
(define (collect-attributes examples)
  (local [(define (add-but-ignore-duplicates lst1 lst2)
            (cond[(empty? lst2) (cons 'placeholder lst1)]
                 [(empty? (rest lst2))
                  (cons 'placeholder (add-to-list (first lst2) lst1))]
                 [else (add-but-ignore-duplicates
                        (add-to-list (first lst2) lst1)
                        (rest lst2))]))
          (define (add-to-list symbol lst)
            (cond[(empty? lst) (cons symbol empty)]
                 [(symbol=? symbol (first lst)) lst]
                 [else (cons (first lst) (add-to-list symbol (rest lst)))]))]
    
    (cond[(empty? examples) empty]
         [(empty? (rest examples)) (rest (first examples))]
         [(empty? (rest (rest examples)))
          (collect-attributes (cons (add-but-ignore-duplicates (rest (first examples))
                                                               (rest (second examples)))
                                    empty))]
         [else (collect-attributes (cons
                                    (add-but-ignore-duplicates (rest (first examples))
                                                               (rest (second examples)))
                                    (rest (rest examples))))])))

;;(split-examples examples symbol) (ListOf Example) + Sym -> (ListOf (ListOf Example))
(define (split-examples examples symbol)
  (local [(define (list-matching-examples pred? examples)
            (cond[(empty? examples) empty]
                 [(match? pred? (first examples))
                  (cons (first examples)
                        (list-matching-examples pred? (rest examples)))]
                 [else (list-matching-examples pred? (rest examples))]))
          (define (list-non-matching-examples pred? examples)
            (cond[(empty? examples) empty]
                 [(not (match? pred? (first examples)))
                  (cons (first examples)
                        (list-non-matching-examples pred? (rest examples)))]
                 [else (list-non-matching-examples pred? (rest examples))]))
          (define (match? pred? lst)
            (cond[(empty? lst) false]
                 [(pred? (first lst)) true]
                 [else (match? pred? (rest lst))]))]
    
    (list (list-matching-examples (lambda (sym) (symbol=? symbol sym)) examples)
          (list-non-matching-examples (lambda (sym) (symbol=? symbol sym)) examples))))

;;(histogram examples) (ListOf Example) -> Histogram
(define (histogram examples)
  (local[(define (split-by-attributes attributes)
           (cond[(empty? attributes) empty]
                [(empty? (rest attributes))
                 (cons (list (first attributes)
                             (count (first (split-examples examples (first attributes)))))
                       empty)]
                [else (cons (list (first attributes)
                                  (count (first (split-examples examples (first attributes)))))
                            (split-by-attributes (rest attributes)))]))
         (define (count lst)
           (cond[(empty? lst) 0]
                [else (add1 (count (rest lst)))]))]
    
    (split-by-attributes (collect-attributes examples))))

;;(augment-histogram histogram attributes total) Histogram + (ListOf Sym) + Num -> AH
(define (augment-histogram histogram attributes total)
  (local[(define (count-not-contains histogram total)
           (cond[(empty? histogram) empty]
                [else (cons (new-element (first histogram) total)
                            (count-not-contains (rest histogram) total))]))
         (define (new-element histogram-element total)
           (list (first histogram-element)
                 (second histogram-element)
                 (- total (second histogram-element))))
         (define (add-attributes-to-histogram histogram attributes)
           (cond[(empty? attributes) histogram]
                [else (add-attributes-to-histogram (add-attribute histogram (first attributes))
                                                   (rest attributes))]))
         (define (add-attribute histogram attribute)
           (cond[(empty? histogram) (cons (list attribute 0) empty)]
                [(symbol=? (first (first histogram)) attribute) histogram]
                [else (cons (first histogram) (add-attribute (rest histogram) attribute))]))]

    (count-not-contains (add-attributes-to-histogram histogram attributes) total)))

;;(entropy positive-counts negative-counts) (list Sym Nat Nat) + (list Sym Nat Nat) -> Num
(define (entropy positive-counts negative-counts)
  (local[(define (estimate-probability val1 val2)
           (cond[(= (+ val1 val2) 0) 0.5]
                [else (/ val1 (+ val1 val2))]))
         (define (calc-e probability)
           (cond[(= probability 0) 0]
                [else (* (- probability) (/ (log probability) (log 2)))]))]

    (+ (* (estimate-probability (+ (second positive-counts) (second negative-counts))
                                (+ (third positive-counts) (third negative-counts)))
          (+ (calc-e (estimate-probability (second positive-counts) (second negative-counts)))
             (calc-e (estimate-probability (second negative-counts) (second positive-counts)))))
       (* (estimate-probability (+ (third positive-counts) (third negative-counts))
                                (+ (second positive-counts) (second negative-counts)))
          (+ (calc-e (estimate-probability (third positive-counts) (third negative-counts)))
             (calc-e (estimate-probability (third negative-counts) (third positive-counts))))))))

;;(entropy-attributes positive negative) AH + AH -> EAL
(define (entropy-attributes positive negative)
  (local[(define (sort lst1 lst2)
           (cond[(empty? lst1) empty]
                [else (cons (find-element (first lst1) lst2)
                            (sort (rest lst1) lst2))]))
         (define (find-element element lst)
           (cond[(symbol=? (first element) (first (first lst))) (first lst)]
                [else (find-element element (rest lst))]))
         (define (calc-entropies positive negative)
           (cond[(empty? positive) empty]
                [else (cons (list (first (first positive))
                                  (entropy (first positive) (first negative)))
                            (calc-entropies (rest positive) (rest negative)))]))]

    (calc-entropies positive (sort positive negative))))

;;(best-attribute attribute-entropies) EAL -> Sym
;; Requires: EAL is not empty
(define (best-attribute attribute-entropies)
  (local[(define (min-list lst)
           (cond[(empty? (rest lst)) (second (first lst))]
                [else
                 (local[(define min-rest (min-list (rest lst)))]
                   (cond[(< (second (first lst)) min-rest) (second (first lst))]
                        [else min-rest]))]))
         (define (choose-matching-entropy entropy entropy-lst)
           (cond[(= (second (first entropy-lst)) entropy)
                 (first (first entropy-lst))]
                [else (choose-matching-entropy entropy (rest entropy-lst))]))]
    (choose-matching-entropy (min-list attribute-entropies) attribute-entropies)))

;;(build-dt examples label) (ListOf Example) + Sym -> DT
(define (build-dt examples label)
  (local[(define split (split-examples examples label))]
    (local[
           (define positive (first split))
           (define negative (second split))
           (define attributes (collect-attributes examples))]
      (local[(define aug-pos (augment-histogram (histogram positive)
                                                all-attributes
                                                (length examples)))
             (define aug-neg (augment-histogram (histogram negative)
                                                all-attributes
                                                (length examples)))]
        (local[(define root-attribute (best-attribute (entropy-attributes
                                                       aug-pos
                                                       aug-neg)))]
          (local[(define split-root (split-examples examples root-attribute))]
            (local[(define (remove-roots examples root)
                     (cond[(empty? examples) empty]
                          [else (cons (remove-root (first examples) root)
                                      (remove-roots (rest examples) root))]))
                   (define (remove-root example root)
                     (cond[(empty? example) empty]
                          [(symbol=? (first example) root) (remove-root (rest example) root)]
                          [else (cons (first example) (remove-root (rest example) root))]))]
              (local[(define contains-root (remove-roots (first split-root) root-attribute))
                     (define no-root (second split-root))]
                  (cond[(empty? positive) false]
                       [(empty? negative) true]
                       [(and (empty? attributes) (> (length positive) (length negative))) true]
                       [(and (empty? attributes) (<= (length positive) (length negative))) false]
                       [(equal? (build-dt contains-root label) (build-dt no-root label))
                        (build-dt contains-root label)]
                       [else (list root-attribute
                                   (build-dt contains-root label)
                                   (build-dt no-root label))])))))))))

;; train-classifier: (listof Example) Sym -> ((listof Sym) -> Bool)
(define (train-classifier examples label)
    (local[(define dt (build-dt examples label))
           (define (in-list? val lst)
           (cond[(empty? lst) false]
                [(symbol=? (first lst) val) true]
                [else (in-list? val (rest lst))]))
           (define (check-down-line current-dt attributes)
             (cond[(boolean? current-dt) current-dt]
                  [(in-list? (first current-dt) attributes)
                   (check-down-line (second current-dt) attributes)]
                  [else (check-down-line (third current-dt) attributes)]))
           (define (predicate attributes)
             (cond[(boolean? dt) dt]
                  [(empty? attributes) false]
                  [(in-list? (first dt) attributes) (check-down-line (second dt) attributes)]
                  [else (check-down-line (third dt) attributes)]))]
      predicate))

;;(performance pred? examples label)
;;   ((listof Sym) -> Bool) + (ListOf Example) + Sym -> (list Sym Nat Nat)
(define (performance pred? examples label)
  (local[(define (comb pred? lst)
           (cond[(empty? lst) 0]
                [(pred? (first (first lst))) (add1 (comb pred? (rest lst)))]
                [else (comb pred? (rest lst))]))
         (define total-pos (comb (lambda (sym) (symbol=? label sym))
                                 examples))
         (define total-neg (comb (lambda (sym) (not (symbol=? label sym)))
                                 examples))
         (define (compare example) (pred? example))
         (define (find-correct current-examples counter)
           (cond[(empty? current-examples) counter]
                [(and (compare (rest (first current-examples)))
                      (symbol=? (first (first current-examples)) label))
                 (find-correct (rest current-examples) (add1 counter))]
                [else (find-correct (rest current-examples) counter)]))
         (define (find-incorrect current-examples counter)
           (cond[(empty? current-examples) counter]
                [(and (not (compare (rest (first current-examples))))
                      (not (symbol=? (first (first current-examples)) label)))
                 (find-incorrect (rest current-examples) (add1 counter))]
                [else (find-incorrect (rest current-examples) counter)]))]

    (list label (round (* (/ (find-correct examples 0) total-pos) 100))
          (round (* (/ (find-incorrect examples 0) total-neg) 100)))))

;;Tests
(check-expect (collect-attributes empty) empty)
(check-expect (collect-attributes seen-spec) (list 'small 'angry 'large 'swims 'flies))

(check-expect (split-examples empty 'goose) (list empty empty))
(check-expect (split-examples seen 'hot) (list empty seen))
(check-expect (split-examples seen 'angry) (list seen empty))

(check-expect (histogram empty) empty)

(check-expect (augment-histogram empty empty 200) empty)
(check-expect (augment-histogram (histogram seen) all-attributes (length seen))
              (list
               (list 'small 1 3)
               (list 'angry 4 0)
               (list 'large 2 2)
               (list 'swims 2 2)
               (list 'flies 3 1)
               (list 'medium 1 3)))

(check-within (entropy (list 'a 0 100) (list 'b 0 100)) 1 0.001)

(check-expect
 (best-attribute
 (list
  (list 'angry #i0.6447688190492)(list 'large #i0.5663948489858)
  (list 'small #i0.5825593868115) (list 'flies #i0.6702490498564)
  (list 'swims #i0.6017998773730) (list 'medium #i0.6901071708677))) 'large)

(define goose? (train-classifier (random-animals 1000) 'goose))
(check-expect (goose? (list 'large 'angry 'flies 'swims)) true)
(check-expect (goose? (list 'small 'angry)) false)
(define squirrel? (train-classifier (random-animals 1000) 'squirrel))
(check-expect (squirrel? (list 'large 'angry 'flies 'swims)) false)
(check-expect (squirrel? (list 'small  'angry)) true)
(define crow? (train-classifier (random-animals 1000) 'crow))
(check-expect (crow? (list 'angry 'flies 'medium)) true)
(check-expect (crow? (list)) false)