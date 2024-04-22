;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname roster) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;;
;;/----------------------------------\
;;    Cameron Carmichael (20881375)
;;    CS 135 Fall 2020
;;    Assignment 06, Problem 1
;;\----------------------------------/
;;

;; A StudentID is a Nat with at most 8 digits (i.e. 0 <= id <= 99999999)

;; A Grade is one of:
;; * false
;; * Nat
;;   Requires: Nat is between 0 and 100 (inclusive)

(define-struct student (id name grade))
;; A Student is a (make-student StudentID Str Grade)


(define-struct rnode (student left right))
;; A Roster Node (RN) is a (make-rnode Student Roster Roster)
;; Requires: all students in the left subtree have an ID < student's ID
;;           all students in the right subtree have an ID > student's ID

;; A Roster is one of 
;; * empty
;; * RN

;;(find-student id roster) produces the student in a roster who has the StudentID id.
;;   If none found, otherwise produces false
;;(class-average roster) produces either the average of the students' grades or, if unavailable, 'N/A
;;(sum-grades roster) produces the sum of all the students' grades in roster
;;(sum-appl-students roster) produces the sum of all the students with applicable grades in roster
;;(find-student/name name roster) produces the list of students in a roster whose names are name
;;(add-students student-list roster) runs through a list of paired student ids and names and adds them
;;   to the roster
;;(add-student student-id student-name roster) produces a new roster by adding a student to the end of
;;   a branch or replacing the name of a preexisting student

;;Test Variables
(define beth (make-student 12345678 "Beth" 96))
(define jenny (make-student 08675309 "Jenny" 81))
(define john1 (make-student 48975311 "John" 95))
(define jenny/new (make-student 08675309 "Jen" 81))
(define john2 (make-student 20488192 "John" false))
(define carlos (make-student 84388192 "Carlos" false))
(define carlos/new (make-student 84388191 "Carlos" false))
(define john/new (make-student 86753090 "John" false))

(define sample-roster
  (make-rnode beth ; root
              (make-rnode jenny empty empty)   ; left child
              (make-rnode john1 empty empty))) ; right child

(define sample-roster-2
  (make-rnode beth 
              (make-rnode jenny/new empty empty)
              (make-rnode john1
                          (make-rnode john2 empty empty)
                          empty)))

(define sample-roster-3
  (make-rnode carlos
              (make-rnode beth
                          (make-rnode jenny empty empty)
                          empty)
              (make-rnode jenny/new
                          (make-rnode john1
                                      (make-rnode john2 empty empty)
                                      empty)
                          empty)))


;;Examples
(check-expect (find-student 12345678 sample-roster) beth)
(check-expect (find-student 87654321 sample-roster) false)

(check-expect (class-average sample-roster) (+ 90 2/3))
(check-expect (class-average sample-roster-2) (+ 90 2/3))
(check-expect (class-average empty) 'N/A)

(check-expect (find-student/name "Beth" sample-roster) (list beth))
(check-expect (find-student/name "Dan" sample-roster) empty)

(check-expect (add-students (list (list 20488192 "John")
                                  (list 8675309 "Jen"))
                            sample-roster)
              sample-roster-2)

;;(find-student id roster) Num + Roster -> (OneOf Bool Student)
(define (find-student id roster)
  (cond[(not (rnode? roster)) false]
       [(= (student-id (rnode-student roster)) id) (rnode-student roster)]
       [(> (student-id (rnode-student roster)) id)
        (find-student id (rnode-left roster))]
       [else (find-student id (rnode-right roster))]))

;;(class-average roster) Roster -> (OneOf Num Sym)
(define (class-average roster)
  (cond[(zero? (sum-appl-students roster)) 'N/A]
       [else (/ (sum-grades roster) (sum-appl-students roster))]))

;;(sum-grades roster) Roster -> Num
(define (sum-grades roster)
  (cond[(not (rnode? roster)) 0]
       [(equal? (student-grade (rnode-student roster)) false)
        (+ (sum-grades (rnode-left roster)) (sum-grades (rnode-right roster)))]
       [else
        (+ (student-grade (rnode-student roster))
           (sum-grades (rnode-left roster))
           (sum-grades (rnode-right roster)))]))

;;(sum-appl-students roster) Roster -> Num
(define (sum-appl-students roster)
  (cond[(not (rnode? roster)) 0]
       [(equal? (student-grade (rnode-student roster)) false)
        (+ (sum-appl-students (rnode-left roster)) (sum-appl-students (rnode-right roster)))]
       [else
        (+ 1 (sum-appl-students (rnode-left roster))
           (sum-appl-students (rnode-right roster)))]))

;;(find-student/name name roster) Str + Roster -> (Listof Student)
(define (find-student/name name roster)
  (cond[(not (rnode? roster)) '()]
       [(string=? (student-name (rnode-student roster)) name)
        (append (find-student/name name (rnode-left roster))
                (list (rnode-student roster))
                (find-student/name name (rnode-right roster)))]
       [else (append (find-student/name name (rnode-left roster))
                     (find-student/name name (rnode-right roster)))]))

;;(add-students student-list roster) (Listof (list StudentID Str)) + Roster -> Roster
(define (add-students student-list roster)
  (cond[(empty? student-list) roster]
       [else (add-students (rest student-list)
                           (add-student (first (first student-list))
                                        (second (first student-list))
                                        roster))]))

;;(add-student student-id student-name roster) StudentID + Str + Roster -> Roster
(define (add-student id student-name roster)
  (cond[(not (rnode? roster))                                 ;;We've started with an empty roster
        (make-rnode (make-student id student-name false)
                    empty
                    empty)]             
       [(= (student-id (rnode-student roster)) id)            ;;Student already exists
        (make-rnode (make-student id student-name
                                  (student-grade (rnode-student roster)))
                    (rnode-left roster)
                    (rnode-right roster))]
       [(> (student-id (rnode-student roster)) id)            ;;New student to the left
        (cond[(not (rnode? (rnode-left roster)))              ;;Place for new student
              (make-rnode (rnode-student roster)
                          (make-rnode (make-student id student-name false)
                                      empty
                                      empty)
                          (rnode-right roster))]
             [else (make-rnode(rnode-student roster)          ;;Need to continue down chain
                              (add-student id student-name (rnode-left roster))
                              (rnode-right roster))])]
       [(< (student-id (rnode-student roster)) id)            ;;New student to the right
        (cond[(not (rnode? (rnode-right roster)))             ;;Place for new student
              (make-rnode (rnode-student roster)
                          (rnode-left roster)
                          (make-rnode (make-student id student-name false)
                                      empty
                                      empty))]
             [else (make-rnode(rnode-student roster)          ;;Need to continue down chain
                              (rnode-left roster)
                              (add-student id student-name (rnode-right roster)))])]))

;;Tests
(check-expect (find-student 08675309 sample-roster) jenny)
(check-expect (find-student 48975311 sample-roster-2) john1)
(check-expect (find-student 84388192 sample-roster-3) carlos)

(check-expect (class-average sample-roster) (+ 90 2/3))
(check-expect (class-average sample-roster-3) (+ 88 1/4))

(check-expect (find-student/name "John" sample-roster-2) (list john2 john1))
(check-expect (find-student/name "John" sample-roster-3) (list john2 john1))
(check-expect (find-student/name "Carlos" sample-roster-3) (list carlos))

(check-expect (add-students (list (list 84388191 "Carlos")
                                  (list 86753090 "John"))
                            empty)
              (make-rnode carlos/new
                          empty
                          (make-rnode john/new
                                      empty
                                      empty)))