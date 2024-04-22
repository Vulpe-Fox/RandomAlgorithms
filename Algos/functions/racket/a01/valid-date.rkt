;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname valid-date) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;;
;;/----------------------------------\
;;    Cameron Carmichael (20881375)
;;    CS 135 Fall 2020
;;    Assignment 01, Problem 5 P1,2
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
;;   if month section is outside the range [january,december]                          -> 0
;;   otherwise returns month number                                                    -> month number
;;(functional-day date-nat) produces a day number:
;;   if day section is outside the by month range [1,28-31]                            -> 0
;;   otherwise returns day number                                                      -> day number
;;(leap-year? date-nat) produces a boolean:
;;   if the year is divisible by 400                                                   -> true
;;   if the year is divisible by 100, but not 400                                      -> false
;;   if otherwise, the year is divisible by 4                                          -> true
;;   otherwise                                                                         -> false

;;Examples
(check-expect (valid-date? 20200922) true)
(check-expect (valid-date? 123456789) false)

(check-expect (leap-year? 8000922) true)
(check-expect (leap-year? 11000922) false)
(check-expect (leap-year? 20200922) true)
(check-expect (leap-year? 20210922) false)

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
  (cond[(and (not (= (functional-month date-nat) 0))
             (not (= (functional-day date-nat) 0))
             (not (and (<= Julian-nonexist-start date-nat)
                       (<= date-nat Julian-nonexist-end))))
        true]
       [else false]))

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
  (cond[(and (<= january (month date-nat)) (<= (month date-nat) december))
       (month date-nat)]
       [else 0]))

;;(functional-day date-nat) Num -> Num
(define (functional-day date-nat)
  (cond[(and (<= 1 (day date-nat)) (<= (day date-nat) 31)
             (= (month date-nat) january))
        (day date-nat)]
       [(and (<= 1 (day date-nat)) (<= (day date-nat) 31)
             (= (month date-nat) march))
        (day date-nat)]
       [(and (<= 1 (day date-nat)) (<= (day date-nat) 31)
             (= (month date-nat) may))
        (day date-nat)]
       [(and (<= 1 (day date-nat)) (<= (day date-nat) 31)
             (= (month date-nat) july))
        (day date-nat)]
       [(and (<= 1 (day date-nat)) (<= (day date-nat) 31)
             (= (month date-nat) august))
        (day date-nat)]
       [(and (<= 1 (day date-nat)) (<= (day date-nat) 31)
             (= (month date-nat) october))
        (day date-nat)]
       [(and (<= 1 (day date-nat)) (<= (day date-nat) 31)
             (= (month date-nat) december))
        (day date-nat)]
       [(and (<= 1 (day date-nat)) (<= (day date-nat) 30)
             (= (month date-nat) april))
        (day date-nat)]
       [(and (<= 1 (day date-nat)) (<= (day date-nat) 30)
             (= (month date-nat) june))
        (day date-nat)]
       [(and (<= 1 (day date-nat)) (<= (day date-nat) 30)
             (= (month date-nat) september))
        (day date-nat)]
       [(and (<= 1 (day date-nat)) (<= (day date-nat) 30)
             (= (month date-nat) november))
        (day date-nat)]
       [(and (<= 1 (day date-nat)) (<= (day date-nat) 29)
             (= (functional-month date-nat) february)
             (leap-year? date-nat))
        (day date-nat)]
       [(and (<= 1 (day date-nat)) (<= (day date-nat) 28)
             (= (functional-month date-nat) february)
             (not (leap-year? date-nat)))
        (day date-nat)]
       [else 0]))

;;(leap-year? date-nat) Num -> Bool
(define (leap-year? date-nat)
  (cond[(integer? (/ (year date-nat) 400)) true]
       [(integer? (/ (year date-nat) 100)) false]
       [(integer? (/ (year date-nat) 4))   true]
       [else                               false]))

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