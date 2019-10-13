#lang racket

(provide student create-student create-students)
(require "utils.rkt")

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
                                   "age: " (get-age) "\n")))
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

(define (parse-id lst)
  (list-ref lst 0))

(define (parse-name lst)
  (list-ref lst 1))

(define (parse-sex lst)
  (list-ref lst 2))

(define (parse-ethnicity lst)
  (list-ref lst 3))

(define (parse-age lst)
  (list-ref lst 4))

(define (create-student lst)
  (student (parse-id lst)
           (parse-name lst)
           (parse-sex lst)
           (parse-ethnicity lst)
           (parse-age lst)))

(define (create-students lst)
  (if (null? lst) '()
      (cons (create-student (car lst)) (create-students (cdr lst)))))
  