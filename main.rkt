#lang racket

(require "utils.rkt")
(require "student.rkt")
(require "group.rkt")
(require "grouping.rkt")

; read from file
(define students-list (car (file-read-all "students-list.rkt")))
; parse to student obj
(define students (create-students students-list))

; random grouping
(display 'grouping-random) (newline)
(define grouping-random (create-random-grouping students 7))
(send 'print grouping-random)

(newline)

;counting grouping
(display 'grouping-counting) (newline)
(define grouping-counting (create-counting-grouping students 7))
(send 'print grouping-counting)

(define groups2 (send 'get-groups grouping-counting))
(define group2 (car groups2))

;(send 'get-name (send 'find-student group2 (lambda (student) (equal? (send 'get-sex student) "female"))))
;(length (send 'find-students group2 (lambda (student) (equal? (send 'get-sex student) "male"))))

(display "male: ") (length (find-all-in-list (lambda (student) (equal? (send 'get-sex student) "male")) students))
(display "ethnicity: ") (length (find-all-in-list (lambda (student) (equal? (send 'get-ethnicity student) "Danish")) students))