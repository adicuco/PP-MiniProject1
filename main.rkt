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
(define grouping-counting (create-counting students 7))
(send 'print grouping-counting)

(newline)

;counting grouping-alternative
(display 'grouping-counting-alternative) (newline)
(define grouping-counting-alternative (create-counting-grouping students 7))
(send 'print grouping-counting-alternative)

(newline)

;balanced grouping
(display 'grouping-balanced) (newline)
(define grouping-balanced (create-balanced-grouping students 7))
(send 'print grouping-balanced)

(define groups2 (send 'get-groups grouping-counting))
(define group2 (car groups2))

(newline)

; random grouping
(display 'grouping-random-pred-female) (newline)
(define grouping-random-pred-female
  (create-random-predicate-grouping
   (lambda (grp)
    (if (= (length grp) 
           (length (filter (lambda (std)
                             (equal? (send 'get-sex std) "female")) grp)))
        #t
        #f))
  students 7))
(send 'print grouping-random-pred-female)

;(send 'get-name (send 'find-student group2 (lambda (student) (equal? (send 'get-sex student) "female"))))
;(length (send 'find-students group2 (lambda (student) (equal? (send 'get-sex student) "male"))))

;(display "male: ") (length (find-all-in-list (lambda (student) (equal? (send 'get-sex student) "male")) students))
;(display "ethnicity: ") (length (find-all-in-list (lambda (student) (equal? (send 'get-ethnicity student) "Danish")) students))
                    
  