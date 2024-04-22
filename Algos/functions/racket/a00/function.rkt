;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname function) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;; Here is a test case for the function f. It has the
;; wrong answer! Run the code, replace 100 with the right answer, and
;; confirm that the test case passes.  Save the file.
(check-expect (f 3) 13)

;; Here is the function definition. It computes f(x) = 3x + 4
(define (f x)
  (+ (* 3 x) 4))