;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname valid-date-cond) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;;
;;/----------------------------------\
;;    Cameron Carmichael (20881375)
;;    CS 135 Fall 2020
;;    Assignment 01, Problem 5 P4
;;\----------------------------------/
;;

;;Notes:
;;   The year is the quotient of the date-nat divided by year-pos
;;   The month is the quotient of the date-nat subtract year-pos times the year divided by month-pos
;;   The day is the date-nat subtract year-pos times the year and month-pos times the month

;;(valid-date? date-nat) produces a boolean using a date-nat in the form (YEAR)MMDD:
;;   if the month section is outside the range [january,december]                      -> false
;;   if the day section is outside the range of dates for a given month                -> false
;;      Jan,Mar,May,July,August,October,December -> [1,31]
;;      April,June,September,November            -> [1,30]
;;      if leap year Feb                         -> [1,29]
;;      if not leap year Feb                     -> [1,28]
;;   if the date-nat lies within the range [julian-nonexist-start,julian-nonexist-end] -> false
;;   otherwise                                                                         -> true
;;(year date-nat) produces a year number
;;(month date-nat) produces a month number regardless of range constraints
;;(day date-nat) produces a day number regardless of range constraints
;;(functional-month date-nat) produces a month number or 0:
;;   if month section is outside the range [january,december]                -> -1
;;   otherwise returns a number                                              -> Diff between max days
;;                                                                               in valid month and
;;                                                                               max for feb (no leap)
;;(functional-day date-nat) produces a number:
;;   if day section is outside the by month range [1,28-31]                  -> -1
;;   otherwise returns 1                                                     -> 1
;;(leap-year? date-nat) produces a number
;;   if the year is divisible by 400                                         -> 1
;;   if the year is divisible by 100, but not 400                            -> 0
;;   if otherwise, the year is divisible by 4                                -> 1
;;   otherwise                                                               -> 0
;;(check-Julian-existence date-nat) produces a number:
;;   if under bounds of range [julian-nonexist-start,julian-nonexist-end]    -> 1
;;   if in bounds of range [julian-nonexist-start,julian-nonexist-end]       -> -1
;;   if above bounds of range [julian-nonexist-start,julian-nonexist-end]    -> 1

;;Examples
(check-expect (valid-date? 20200922) true)
(check-expect (valid-date? 123456789) false)

(check-expect (leap-year? 8000922) 1)
(check-expect (leap-year? 11000922) 0)
(check-expect (leap-year? 20200922) 1)
(check-expect (leap-year? 20210922) 0)

;;Constants
(define january   1)
(define february  2)
(define march     3)
(define april     4)
(define may       5)
(define june      6)
(define july      7)
(define august    8)
(define september 9)
(define october   10)
(define november  11)
(define december  12)

(define Julian-nonexist-start 17520903) ;start of days which aren't part of the Julian calendar
(define Julian-nonexist-end 17520913)   ;end of days which aren't part of the Julian calendar

(define year-pos 10000) ;Base 10 position in date-nat of year
(define month-pos 100)  ;Base 10 position in date-nat of month

;;Functions

;;(valid-date? date-nat) Num -> Bool
;;Requires:
;;   date-nat >= 10101
(define (valid-date? date-nat)
  (cond[(< (functional-month date-nat) 0) false]
       [(< (functional-day date-nat) 0) false]
       [(< (check-Julian-existence date-nat) 0) false]
       [else true]))

;;(year date-nat) Num -> Num
(define (year date-nat)
  (quotient date-nat year-pos))

;;(month date-nat) Num -> Num
(define (month date-nat)
  (quotient (- date-nat (* year-pos (year date-nat))) month-pos))

;;(day date-nat) Num -> Num
(define (day date-nat)
  (- date-nat (+ (* year-pos (year date-nat)) (* month-pos (month date-nat)))))

;;(functional-month date-nat) Num -> Num
(define (functional-month date-nat)
  (cond[(= (month date-nat) january) 3]
       [(= (month date-nat) march) 3]
       [(= (month date-nat) may) 3]
       [(= (month date-nat) july) 3]
       [(= (month date-nat) august) 3]
       [(= (month date-nat) october) 3]
       [(= (month date-nat) december) 3]
       [(= (month date-nat) april) 2]
       [(= (month date-nat) june) 2]
       [(= (month date-nat) september) 2]
       [(= (month date-nat) november) 2]
       [(= (month date-nat) february) 0]
       [else -1]))

;;(functional-day date-nat) Num -> Num
(define (functional-day date-nat)
  (cond[(= (day date-nat) 0) -1]
       [(<= (day date-nat) (+ 28 (leap-year? date-nat))) 1]       ;Ensures less than 28,30,31
       [(<= (day date-nat) (+ 28 (functional-month date-nat))) 1] ;It could be 29 on feb in leap year
       [else -1]))

;;(leap-year? date-nat) Num -> Num
(define (leap-year? date-nat)
  (cond[(integer? (/ (year date-nat) 400)) 1]
       [(integer? (/ (year date-nat) 100)) 0]
       [(integer? (/ (year date-nat) 4))   1]
       [else                               0]))

;;(check-Julian-existence date-nat) Num -> Num
(define (check-Julian-existence date-nat)
  (cond[(< (- date-nat Julian-nonexist-end) (- Julian-nonexist-start Julian-nonexist-end)) 1]
       [(<= (- date-nat Julian-nonexist-end) 0) -1]
       [(> (- date-nat Julian-nonexist-end) 0) 1]))

;;Tests
(check-expect (valid-date? 20200229) true)  ;Leap Feb 29
(check-expect (valid-date? 20210229) false) ;Non leap Feb 29
(check-expect (valid-date? 20210228) true)  ;Non leap Feb 28
(check-expect (valid-date? 20210256) false) ;Too big day
(check-expect (valid-date? 20215728) false) ;Too big month
(check-expect (valid-date? 320210131) true) ;Big year
(check-expect (valid-date? 17520903) false) ;inside Julian-nonexist-start
(check-expect (valid-date? 7520903) true)   ;beneath Julian-nonexist-start
(check-expect (valid-date? 7520900) false)  ;0 day
(check-expect (valid-date? 7520007) false)  ;0 month
(check-expect (valid-date? 7520307) true)   ;mar
(check-expect (valid-date? 7520507) true)   ;may
(check-expect (valid-date? 7520707) true)   ;july
(check-expect (valid-date? 7520807) true)   ;august
(check-expect (valid-date? 7521007) true)   ;october
(check-expect (valid-date? 7521207) true)   ;december
(check-expect (valid-date? 7520407) true)   ;april
(check-expect (valid-date? 7520607) true)   ;june
(check-expect (valid-date? 7521107) true)   ;november