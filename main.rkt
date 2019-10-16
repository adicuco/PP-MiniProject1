; name: Adrian-Alexandru Cucolaș
; study no.: 20191000
; email: adicucol19@student.aau.dk

; different algoriths of grouping students in groups
; using both associative lists and OOP simulation
; written in DrRacket with #lang racket

#lang racket

(require "utils.rkt")
(require "student.rkt")
(require "group.rkt")
(require "grouping.rkt")

(define students-list (car (file-read-all "students-list.rkt")))
(define students (create-students students-list))

; random grouping
(display "Grouping Random") (newline)
(define grouping-random (create-random-grouping students 7))
(send 'print grouping-random)

(newline)

; counting grouping
(display "Grouping Counting") (newline)
(define grouping-counting (create-counting students 7))
(send 'print grouping-counting)

(newline)

; counting grouping (alternative)
(display "Grouping Counting (alternative)") (newline)
(define grouping-counting-alternative (create-counting-grouping students 7))
(send 'print grouping-counting-alternative)

(newline)

; counting balanced grouping
(display "Grouping Counting Balanced") (newline)
(define grouping-balanced (create-balanced-grouping students 7))
(send 'print grouping-balanced)

(newline)

; random grouping with predicate (at least N students in a group are A years old, or older)
(define n 3)
(define a 25)
(display "Grouping Random (at leeast n students >= a age)") (newline)
(define grouping-random-pred-age
  (create-random-predicate-grouping
   (lambda (grp)
    (>= (length (filter (lambda (std)
                         (>= (send 'get-age std) a)) grp))
        n))
  students 7))
(send 'print grouping-random-pred-age)

(newline)

; random grouping with predicate (all students in the group are female)
(display "Grouping Random (all female)") (newline)
(define grouping-random-pred-female
  (create-random-predicate-grouping
   (lambda (grp)
    (= (length grp)
       (length (filter (lambda (std)
                         (equal? (send 'get-sex std) "female")) grp))))
  students 7))
(send 'print grouping-random-pred-female)

(newline)

; random grouping with predicate (no students in the group are of the same age)
(display "Grouping Random (not same age)") (newline)
(define grouping-random-pred-age-same
  (create-random-predicate-grouping
   (lambda (grp)
    (= (check-duplicates (map (lambda (std)
                         (send 'get-age std)) grp) #:default 0)
        0))
  students 7))
(send 'print grouping-random-pred-age-same)