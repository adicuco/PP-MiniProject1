; name: Adrian-Alexandru Cucolaș
; study no.: 20191000
; email: adicucol19@student.aau.dk

#lang racket

(require "utils.rkt")

(provide student create-student create-students)

; Student object simulation
(define (student id name sex ethnicity age)
  (letrec ((get-id (lambda () id))
           (get-name (lambda () name))
           (get-sex (lambda () sex))
           (get-ethnicity (lambda () ethnicity))
           (get-age (lambda () age))
           (type-of (lambda () 'student))
           (print-info (lambda () (display-all
                                   "id: " (get-id) "\n"
                                   "name: " (get-name) "\n"
                                   "sex: " (get-sex) "\n"
                                   "age: " (get-age) "\n"
                                   "ethnicity: " (get-ethnicity) "\n")))
          )
    (lambda (message)
      (cond ((eq? message 'get-id) get-id)
            ((eq? message 'get-name) get-name)
            ((eq? message 'get-sex) get-sex)
            ((eq? message 'get-ethnicity) get-ethnicity)
            ((eq? message 'get-age) get-age)
            ((eq? message 'type-of) type-of)
            ((eq? message 'print) print-info)
            (else (error "Message not understood"))))))

; Student constuctor
; returns Student by parsing st
(define (create-student st)
  (student (parse-id st)
           (parse-name st)
           (parse-sex st)
           (parse-ethnicity st)
           (parse-age st)))

; returns list of Students
(define (create-students lst)
  (if (null? lst) '()
      (cons (create-student (car lst)) (create-students (cdr lst)))))

;------------------------------ Parsers ------------------------------

; returns value at position 0 of lst (id)
(define (parse-id lst)
  (list-ref lst 0))

; returns value at position 1 of lst (name)
(define (parse-name lst)
  (list-ref lst 1))

; returns value at position 2 of lst (sex)
(define (parse-sex lst)
  (list-ref lst 2))

; returns value at position 3 of lst (ethnicity)
(define (parse-ethnicity lst)
  (list-ref lst 3))

; returns value at position 4 of lst (age)
(define (parse-age lst)
  (list-ref lst 4))