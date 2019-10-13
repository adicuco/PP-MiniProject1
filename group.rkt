#lang racket

(require "utils.rkt")

(provide group)

(define (group id students)
  (letrec ((get-id (lambda () id))
           (get-students (lambda () students))
           (get-length (lambda () (length students)))
           (find-student (lambda (func) (find-first-in-list func students)))
           (find-students (lambda (func) (find-all-in-list func students)))
           (type-of (lambda () 'group))
          )
    (lambda (message)
      (cond ((eq? message 'get-id) get-id)
            ((eq? message 'get-students) get-students)
            ((eq? message 'get-length) get-length)
            ((eq? message 'find-student) find-student)
            ((eq? message 'find-students) find-students)
            (else (error "Message not understood"))))))