;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname html) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;
;;/----------------------------------\
;;    Cameron Carmichael (20881375)
;;    CS 135 Fall 2020
;;    Assignment 06, Problem 2
;;\----------------------------------/
;;

;; An HTML-Item (HI) is one of
;; * Str
;; * Tag

;; A Tag is (cons Sym (listof HI))

;;(html->string html-item) produces the html text equivalent to an HI
;;(remove-tag sym html-item) produces the html item with the tag, sym, removed.
;;   If the root is removed, the function instead produces the list of child
;;   html items
;;(okay-tags? html-item) produces whether or not these two rules are followed:
;;   'li only follows an 'ol or 'ul
;;   'hr has no children
;;(list-item-with-parent? parent html-items) produces false if anything inside the
;;   list of html-items for a tag is 'li and the tag isn't 'ol or 'ul

;;Test variables
(define just-text "Hello, world!")
(define short-example '(p (h1 "Heading") "Text"))
(define html-example '(html (head (title "CS135"))
                            (body (h1 "Welcome")
                                  "More text...")))

;;Examples
(check-expect (html->string "text") "text")
(check-expect(html->string html-example)
             (string-append
              "<html><head><title>CS135</title></head>"
              "<body><h1>Welcome</h1>More text...</body></html>"))

(check-expect (remove-tag 'b html-example) html-example)
(check-expect (remove-tag 'b '(p "Hello, " (b "World") "!"))
              '(p "Hello, " "World" "!"))
(check-expect (remove-tag 'p '(p "Hello, " (b "World") "!"))
              '("Hello, " (b "World") "!"))

(check-expect (okay-tags? html-example) true)
(check-expect (okay-tags? '(body (hr "hello"))) false)
(check-expect (okay-tags? '(body (li "Q1") "text")) false)

;;(html->string html-item) HI -> Str
(define (html->string html-item)
  (cond[(empty? html-item) ""]
       [(not (list? html-item)) html-item]
       [(string? (first html-item)) (string-append
                                     (first html-item)
                                     (html->string (rest html-item)))]
       [(list? (first html-item))(string-append
                                  (html->string (first html-item))
                                  (html->string (rest html-item)))]
       [else (string-append
              "<"
              (symbol->string (first html-item))
              ">"
              (html->string (rest html-item))
              "</"
              (symbol->string (first html-item))
              ">")]))

;;(remove-tag sym html-item) Sym + HI -> (Oneof HI (list HI))
(define (remove-tag sym html-item)
  (cond[(empty? html-item) empty]
       [(string? html-item) html-item]
       [(and (symbol? (first html-item))(symbol=? (first html-item) sym))
        (remove-tag sym (rest html-item))]
       [(and (list? (first html-item)) (and (symbol? (first (first html-item)))
                                            (symbol=? (first (first html-item)) sym)))
        (append
         (remove-tag sym (first html-item))
         (remove-tag sym (rest html-item)))]
       [(list? (first html-item)) (cons
                                   (remove-tag sym (first html-item))
                                   (remove-tag sym (rest html-item)))]
       [else (cons (first html-item)
                   (remove-tag sym (rest html-item)))]))

;;(okay-tags? html-item) HI -> Bool
(define (okay-tags? html-item)
  (cond[(empty? html-item) true]
       [(string? html-item) true]
       [(and (equal? (first html-item) 'hr) (not (equal? (second html-item) empty)))
        false]
       [(list? (first html-item))
        (and
         (okay-tags? (first html-item))
         (okay-tags? (rest html-item)))]
       [(empty? (rest html-item)) (okay-tags? (rest html-item))]
       [else (and (list-item-with-parent? (first html-item) (first (rest html-item)))
                  (okay-tags? (rest html-item)))]))

;;(list-item-with-parent? parent html-items) Sym + (Listof HI) -> Bool
(define (list-item-with-parent? parent html-items)
  (cond[(not (list? html-items)) true]
       [(empty? html-items) true]
       [(and (equal? (first html-items) 'li)
             (not (or (symbol=? parent 'ol) (symbol=? parent 'ul))))
        false]
       [else (list-item-with-parent? parent (rest html-items))]))

;;Tests
(check-expect (html->string just-text) "Hello, world!")
(check-expect (html->string short-example)
              "<p><h1>Heading</h1>Text</p>")

(check-expect (remove-tag 'p '(p))
              empty)
(check-expect (remove-tag 'p '(p "Hello, world!"))
              (list "Hello, world!"))
(check-expect (remove-tag 'p '())
              empty)
(check-expect (remove-tag 'p "hello")
              "hello")

(check-expect (okay-tags? "hello") true)
(check-expect (okay-tags? short-example) true)
(check-expect (okay-tags? just-text) true)