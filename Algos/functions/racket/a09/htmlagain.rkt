;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname htmlagain) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;;
;;/----------------------------------\
;;    Cameron Carmichael (20881375)
;;    CS 135 Fall 2020
;;    Assignment 09, Problem 3
;;\----------------------------------/
;;

;; An HTML-Item (HI) is one of
;;*Str
;;*Tag

;; A Tag is (cons Sym (listof HI))

;;(tokenize str) Produces a list of strings representing the opening tags, closing tags,
;;   and strings in the document
;;(string->html str) Produces an HI for a well-formed string, 'str'

;;Examples
(check-expect (tokenize "<p><h1>Heading</h1>Text</p>")
              '("<p>" "<h1>" "Heading" "</h1>" "Text" "</p>"))
(check-expect (tokenize "a b   c") '("a b   c"))
(check-expect (tokenize "") empty)

(check-expect (string->html"<p><h1>Heading</h1>Text</p>")'(p (h1 "Heading") "Text"))

;;(tokenize str) Str -> (Listof Str)
;;Requires: str must be well-formed corresponding to an HTML item
(define (tokenize str)
  (local[(define (brk-brkts loc)
           (cond[(empty? loc) (list empty)]
                [(and (empty? (rest loc)) (char=? #\> (first loc)))
                 (cons loc (list empty))]
                [else
                 (local[(define r (brk-brkts (rest loc)))]
                   (cond[(or (char=? #\> (first loc))
                             (and (not (empty? (rest loc))) (char=? #\< (first (rest loc)))))
                         (cons (cons (first loc) empty) r)]
                         [else (cons (cons (first loc) (first r)) (rest r))]))]))]
    (filter (lambda (s) (not (string=? "" s))) (map list->string (brk-brkts (string->list str))))))

;;(string->html str) Str -> HI
(define (string->html str)
  (local[(define tokenized-str (tokenize str))
         (define (filter-out-brkts curr-str)
           (list->string
            (filter (lambda (c) (not (or (char=? c #\<) (char=? c #\>))))
                    (string->list curr-str))))
         (define (break-into-nl slst)
           (cond[(empty? slst) empty]
                [else
                 (local[(define r (break-into-nl (rest slst)))]
                   (cond [(char=? (second (string->list (first slst))) #\/)
                          r]
                         [(char=? (first (string->list (first slst))) #\<)
                          (cons (string->symbol (filter-out-brkts (first slst)))
                                (cons (cons (first r) empty) (rest r)))]
                         [else (cons (first slst) r)]))]))
         (define (fix-nl nl)
           (cond[(empty? nl) empty]
                [(empty? (rest nl)) (cons (first nl) empty)]
                [(and (list? (first nl)) (list? (second nl))
                      (symbol? (first (first nl))) (not (symbol? (first (second nl)))))
                 (cons (append (first nl) (second nl)) (fix-nl (rest (rest nl))))]
                [else (cons (first nl) (fix-nl (rest nl)))]))]
    (fix-nl (break-into-nl tokenized-str))))

;;Tests
(check-expect (tokenize "a b   c <p> </p>") '("a b   c " "<p>" " " "</p>"))
(check-expect (tokenize "a b   c <p></p>") '("a b   c " "<p>" "</p>"))

(check-expect (string->html "") '())
(check-expect (string->html "text") '("text"))