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

;;(collect-attr ex) Determines all the known attr in a set of ex
;;(splt-ex ex symbol) Creates a list of:
;;   a list of ex which contain symbol
;;   a list of ex which don't contain symbol
;;(histogram ex)
;;(aug-histogram histogram attr total) produces an AH from a histogram using a total num of
;;   entries
;;(entropy positive-counts negative-counts) Determines the entropy when comparing two members of an AH
;;(entropy-attr positive negative) Determines the entropy for all attr in two AH
;;(best-attribute attribute-entropies) Determines the attribute with the lowest entropy
;;(build-dt ex label) Produces a Decision Tree for a set of data for a label
;;(train-classifier) produces a predicate which can be used to determine what something is based on
;;   test data

;;A list of all attr
(define all-attr (list 'sml 'med 'lrg 'angry 'fly 'swim))

;;Test variables
(define seen
  (list
   (list 'squirrel 'sml 'angry)
   (list 'goose 'lrg 'swim 'fly 'angry)
   (list 'goose 'lrg 'swim 'fly 'angry)
   (list 'crow 'med 'fly 'angry)))

(define seen-spec
  (list
   (list 'squirrel 'sml 'angry)
   (list 'goose 'lrg 'swim 'fly 'angry)
   (list 'goose 'lrg 'swim 'fly 'angry)
   (list 'nid)))

;;Examples
(check-expect (collect-attr seen) (list 'sml 'angry 'lrg 'swim 'fly 'med))

(check-expect (splt-ex seen 'goose)
              (list
               (list
                (list 'goose 'lrg 'swim 'fly 'angry)
                (list 'goose 'lrg 'swim 'fly 'angry))
               (list
                (list 'squirrel 'sml 'angry)
                (list 'crow 'med 'fly 'angry))))

(check-expect (histogram seen)
              (list (list 'sml 1) (list 'angry 4) (list 'lrg 2)
                    (list 'swim 2) (list 'fly 3) (list 'med 1)))

(check-expect
 (aug-histogram
  (list (list 'a 100) (list 'c 50))
  (list 'a 'b 'c)
  200)
 (list (list 'a 100 100) (list 'c 50 150) (list 'b 0 200)))
(check-expect
 (aug-histogram empty (list 'x 'y) 10)
 (list (list 'x 0 10) (list 'y 0 10)))

(check-within (entropy (list 'lrg 126 59) (list 'lrg 146 669)) 0.5664 0.001)
(check-within (entropy (list 'sml 17 168) (list 'sml 454 361)) 0.5826 0.001)
(check-within (entropy (list 'a 0 100) (list 'b 100 0)) 0 0.001)

(check-within (entropy-attr
               (list
                (list 'lrg 126 59) (list 'angry 161 24)
                (list 'sml 17 168) (list 'fly 170 15)
                (list 'swim 162 23) (list 'med 42 143))
               (list
                (list 'lrg 146 669) (list 'angry 469 346)
                (list 'sml 454 361) (list 'fly 615 200)
                (list 'swim 365 450) (list 'med 215 600)))
              (list
               (list 'lrg #i0.5663948489858) (list 'angry #i0.6447688190492)
               (list 'sml #i0.5825593868115) (list 'fly #i0.6702490498564)
               (list 'swim #i0.6017998773730) (list 'med #i0.6901071708677))
              0.001)

(check-expect
 (best-attribute
 (list
  (list 'lrg #i0.5663948489858) (list 'angry #i0.6447688190492)
  (list 'sml #i0.5825593868115) (list 'fly #i0.6702490498564)
  (list 'swim #i0.6017998773730) (list 'med #i0.6901071708677))) 'lrg)

;;(collect-attr ex) (ListOf Example) -> (ListOf Sym)
(define (collect-attr ex)
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
    
    (cond[(empty? ex) empty]
         [(empty? (rest ex)) (rest (first ex))]
         [(empty? (rest (rest ex)))
          (collect-attr (cons (add-but-ignore-duplicates (rest (first ex))
                                                               (rest (second ex)))
                                    empty))]
         [else (collect-attr (cons
                                    (add-but-ignore-duplicates (rest (first ex))
                                                               (rest (second ex)))
                                    (rest (rest ex))))])))

;;(splt-ex ex symbol) (ListOf Example) + Sym -> (ListOf (ListOf Example))
(define (splt-ex ex symbol)
  (local [(define (list-matching-examples pred? ex)
            (cond[(empty? ex) empty]
                 [(match? pred? (first ex))
                  (cons (first ex)
                        (list-matching-examples pred? (rest ex)))]
                 [else (list-matching-examples pred? (rest ex))]))
          (define (list-non-matching-examples pred? ex)
            (cond[(empty? ex) empty]
                 [(not (match? pred? (first ex)))
                  (cons (first ex)
                        (list-non-matching-examples pred? (rest ex)))]
                 [else (list-non-matching-examples pred? (rest ex))]))
          (define (match? pred? lst)
            (cond[(empty? lst) false]
                 [(pred? (first lst)) true]
                 [else (match? pred? (rest lst))]))]
    
    (list (list-matching-examples (lambda (sym) (symbol=? symbol sym)) ex)
          (list-non-matching-examples (lambda (sym) (symbol=? symbol sym)) ex))))

;;(histogram ex) (ListOf Example) -> Histogram
(define (histogram ex)
  (local[(define (split-by-attr attr)
           (cond[(empty? attr) empty]
                [(empty? (rest attr))
                 (cons (list (first attr)
                             (count (first (splt-ex ex (first attr)))))
                       empty)]
                [else (cons (list (first attr)
                                  (count (first (splt-ex ex (first attr)))))
                            (split-by-attr (rest attr)))]))
         (define (count lst)
           (cond[(empty? lst) 0]
                [else (add1 (count (rest lst)))]))]
    
    (split-by-attr (collect-attr ex))))

;;(aug-histogram histogram attr total) Histogram + (ListOf Sym) + Num -> AH
(define (aug-histogram histogram attr total)
  (local[(define (count-not-contains histogram total)
           (cond[(empty? histogram) empty]
                [else (cons (new-element (first histogram) total)
                            (count-not-contains (rest histogram) total))]))
         (define (new-element histogram-element total)
           (list (first histogram-element)
                 (second histogram-element)
                 (- total (second histogram-element))))
         (define (add-attr-to-histogram histogram attr)
           (cond[(empty? attr) histogram]
                [else (add-attr-to-histogram (add-attribute histogram (first attr))
                                                   (rest attr))]))
         (define (add-attribute histogram attribute)
           (cond[(empty? histogram) (cons (list attribute 0) empty)]
                [(symbol=? (first (first histogram)) attribute) histogram]
                [else (cons (first histogram) (add-attribute (rest histogram) attribute))]))]

    (count-not-contains (add-attr-to-histogram histogram attr) total)))

;;(entropy positive-counts negative-counts) (list Sym Nat Nat) + (list Sym Nat Nat) -> Num
(define (entropy positive-counts negative-counts)
  (local[(define (est-prob val1 val2)
           (cond[(= (+ val1 val2) 0) 0.5]
                [else (/ val1 (+ val1 val2))]))
         (define (calc-e probability)
           (cond[(= probability 0) 0]
                [else (* (- probability) (/ (log probability) (log 2)))]))]

    (+ (* (est-prob (+ (second positive-counts) (second negative-counts))
                                (+ (third positive-counts) (third negative-counts)))
          (+ (calc-e (est-prob (second positive-counts) (second negative-counts)))
             (calc-e (est-prob (second negative-counts) (second positive-counts)))))
       (* (est-prob (+ (third positive-counts) (third negative-counts))
                                (+ (second positive-counts) (second negative-counts)))
          (+ (calc-e (est-prob (third positive-counts) (third negative-counts)))
             (calc-e (est-prob (third negative-counts) (third positive-counts))))))))

;;(entropy-attr positive negative) AH + AH -> EAL
(define (entropy-attr positive negative)
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

;;(build-dt ex label) (ListOf Example) + Sym -> DT
(define (build-dt ex label)
  (local[(define split (splt-ex ex label))]
    (local[
           (define positive (first split))
           (define negative (second split))
           (define attr (collect-attr ex))]
      (local[(define aug-pos (aug-histogram (histogram positive)
                                                all-attr
                                                (length ex)))
             (define aug-neg (aug-histogram (histogram negative)
                                                all-attr
                                                (length ex)))]
        (local[(define root-attr (best-attribute (entropy-attr
                                                       aug-pos
                                                       aug-neg)))]
          (local[(define split-rt (splt-ex ex root-attr))]
            (local[(define (rm-rts ex root)
                     (cond[(empty? ex) empty]
                          [else (cons (rm-rt (first ex) root)
                                      (rm-rts (rest ex) root))]))
                   (define (rm-rt example root)
                     (cond[(empty? example) empty]
                          [(symbol=? (first example) root) (rm-rt (rest example) root)]
                          [else (cons (first example) (rm-rt (rest example) root))]))]
              (local[(define ctns-root (rm-rts (first split-rt) root-attr))
                     (define no-root (second split-rt))]
                  (cond[(empty? positive) false]
                       [(empty? negative) true]
                       [(and (empty? attr) (> (length positive) (length negative))) true]
                       [(and (empty? attr) (<= (length positive) (length negative))) false]
                       [(equal? (build-dt ctns-root label) (build-dt no-root label))
                        (build-dt ctns-root label)]
                       [else (list root-attr
                                   (build-dt ctns-root label)
                                   (build-dt no-root label))])))))))))

;; train-classifier: (listof Example) Sym -> ((listof Sym) -> Bool)
(define (train-classifier ex label)
    (local[(define dt (build-dt ex label))
           (define (in-list? val lst)
           (cond[(empty? lst) false]
                [(symbol=? (first lst) val) true]
                [else (in-list? val (rest lst))]))
           (define (check-down-line current-dt attr)
             (cond[(boolean? current-dt) current-dt]
                  [(in-list? (first current-dt) attr)
                   (check-down-line (second current-dt) attr)]
                  [else (check-down-line (third current-dt) attr)]))
           (define (predicate attr)
             (cond[(boolean? dt) dt]
                  [(empty? attr) false]
                  [(in-list? (first dt) attr) (check-down-line (second dt) attr)]
                  [else (check-down-line (third dt) attr)]))]
      predicate))

;;(performance pred? ex label)
;;   ((listof Sym) -> Bool) + (ListOf Example) + Sym -> (list Sym Nat Nat)
(define (performance pred? ex label)
  (local[(define (comb pred? lst)
           (cond[(empty? lst) 0]
                [(pred? (first (first lst))) (add1 (comb pred? (rest lst)))]
                [else (comb pred? (rest lst))]))
         (define total-pos (comb (lambda (sym) (symbol=? label sym))
                                 ex))
         (define total-neg (comb (lambda (sym) (not (symbol=? label sym)))
                                 ex))
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

    (list label (round (* (/ (find-correct ex 0) total-pos) 100))
          (round (* (/ (find-incorrect ex 0) total-neg) 100)))))

;;Tests
(check-expect (collect-attr empty) empty)
(check-expect (collect-attr seen-spec) (list 'sml 'angry 'lrg 'swim 'fly))

(check-expect (splt-ex empty 'goose) (list empty empty))
(check-expect (splt-ex seen 'hot) (list empty seen))
(check-expect (splt-ex seen 'angry) (list seen empty))

(check-expect (histogram empty) empty)

(check-expect (aug-histogram empty empty 200) empty)
(check-expect (aug-histogram (histogram seen) all-attr (length seen))
              (list
               (list 'sml 1 3)
               (list 'angry 4 0)
               (list 'lrg 2 2)
               (list 'swim 2 2)
               (list 'fly 3 1)
               (list 'med 1 3)))

(check-within (entropy (list 'a 0 100) (list 'b 0 100)) 1 0.001)

(check-expect
 (best-attribute
 (list
  (list 'angry #i0.6447688190492)(list 'lrg #i0.5663948489858)
  (list 'sml #i0.5825593868115) (list 'fly #i0.6702490498564)
  (list 'swim #i0.6017998773730) (list 'med #i0.6901071708677))) 'lrg)

(define goose? (train-classifier (random-animals 1000) 'goose))
(check-expect (goose? (list 'lrg 'angry 'fly 'swim)) true)
(check-expect (goose? (list 'sml 'angry)) false)
(define squirrel? (train-classifier (random-animals 1000) 'squirrel))
(check-expect (squirrel? (list 'lrg 'angry 'fly 'swim)) false)
(check-expect (squirrel? (list 'sml  'angry)) true)
(define crow? (train-classifier (random-animals 1000) 'crow))
(check-expect (crow? (list 'angry 'fly 'med)) true)
(check-expect (crow? (list)) false)