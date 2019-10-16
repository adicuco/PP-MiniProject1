; name: Adrian-Alexandru Cucolaș
; study no.: 20191000
; email: adicucol19@student.aau.dk

#lang racket

(provide send file-read-all display-all
         find-first-in-list
         first-n-elements last-n-elements nth-element
         maximum-group-size minimum-group-size
         get-eths calculate-ratio)

; used to send messages to objects
(define (send message obj . par)
  (let ((method (obj message)))
    (apply method par)))

; reads file by file-nale
(define (file-read-all file-name)
 (let* ((port (open-input-file file-name))
        (contents (file-read-all-1 port '())))
   (close-input-port port)
   (reverse contents)))

; read file helper
(define (file-read-all-1 port res)
 (let ((form (read port)))
   (if (eof-object? form)
       res
       (file-read-all-1 port (cons form res)))))

; returns the first element of lst for which pred produces a true value, else the empty list
(define (find-first-in-list pred lst)
    (cond ((null? lst) '())
          ((pred (car lst)) (car lst))
          (else (find-first-in-list pred (cdr lst)))))

; returns the first n (or available if lst lenght < n) elements of lst
(define (first-n-elements lst n)
  (if (or (zero? n) (null? lst))
      (list)
      (cons (car lst) (first-n-elements (cdr lst) (- n 1)))))

; returns the list after the first n elements of lst if list length > n, else lst
(define (last-n-elements lst n)
  (if (> (length lst) n)
      (list-tail lst n)
      lst))

; returns the maximum value max of a group length by recursively looping on lst
(define (maximum-group-size lst [max 0])
  (if (null? lst)
      max
      (if (> (send 'get-length (first lst)) max)
          (maximum-group-size (rest lst) (send 'get-length (first lst)))
          (maximum-group-size (rest lst) max))))

; returns the minimum value min of a group length by recursively looping on lst
(define (minimum-group-size lst min)
  (if (null? lst)
      min
      (if (< (send 'get-length (first lst)) min)
          (minimum-group-size (rest lst) (send 'get-length (first lst)))
          (minimum-group-size (rest lst) min))))

; returns the element of lst at position n
(define (nth-element lst n)
  (cond ((null? lst) '())
        ((= n 0) (first lst))
        (else (nth-element (rest lst) (- n 1)))))

; loops on vs to display multiple values
(define (display-all . vs)
  (for-each display vs))

; returns a list with distinct ethnecities within lst by filtering all ethenecities
(define (get-eths lst [ethnicities '()])
  (if (null? lst)
      ethnicities
      (let ((eth (send 'get-ethnicity (first lst))))
        (get-eths
         (filter (lambda (std)
                   (not (equal? (send 'get-ethnicity std) eth)))
                 lst) (cons eth ethnicities)))))

; returns the percentage of part in total
(define (calculate-ratio part total)
  (round (- 100 (* (/ (- total part) total) 100))))