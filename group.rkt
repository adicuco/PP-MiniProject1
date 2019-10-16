; name: Adrian-Alexandru Cucolaș
; study no.: 20191000
; email: adicucol19@student.aau.dk

#lang racket

(require "utils.rkt")

(provide group)

; Group object simulation
(define (group id students)
  (letrec ((get-id (lambda () id))
           (get-students (lambda () students))
           (get-length (lambda () (length students)))
           (get-ethnicities (lambda () (get-eths students)))
           (find-student (lambda (func) (find-first-in-list func students)))
           (find-students (lambda (func) (filter func students)))
           (get-gender-ratio (lambda (gender)
                               (calculate-ratio
                                (length (find-students
                                         (lambda (student) (equal? (send 'get-sex student) gender))))
                                (length (get-students)))))
           (type-of (lambda () 'group))
           (print-info (lambda () (display-all
                                   "id: " (get-id) "\n"
                                   "count: " (get-length) "\n"
                                   "ethnicities: " (length (get-ethnicities)) "\n"
                                   "gender-ratio: (M)" (get-gender-ratio "male") "% | (F)"
                                   (get-gender-ratio "female") "%\n\n")))
          )
    (lambda (message)
      (cond ((eq? message 'get-id) get-id)
            ((eq? message 'get-students) get-students)
            ((eq? message 'get-length) get-length)
            ((eq? message 'get-ethnicities) get-ethnicities)
            ((eq? message 'get-gender-ratio) get-gender-ratio)
            ((eq? message 'find-student) find-student)
            ((eq? message 'find-students) find-students)
            ((eq? message 'print) print-info)
            (else (error "Message not understood"))))))


