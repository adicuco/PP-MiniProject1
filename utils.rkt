#lang racket

(provide send file-read-all display-all
         find-first-in-list find-all-in-list
         first-n-elements last-n-elements nth-element
         maximum-group-size minimum-group-size
         get-eths)

(define (send message obj . par)
  (let ((method (obj message)))
    (apply method par)))

(define (file-read-all file-name)
 (let* ((port (open-input-file file-name))
        (contents (file-read-all-1 port '())))
   (close-input-port port)
   (reverse contents)))

(define (file-read-all-1 port res)
 (let ((form (read port)))
   (if (eof-object? form)
       res
       (file-read-all-1 port (cons form res)))))

(define (find-first-in-list pred lst)
    (cond ((null? lst) #f)
          ((pred (car lst)) (car lst))
          (else (find-first-in-list pred (cdr lst)))))

(define (find-all-in-list pred lst)
    (cond ((null? lst) '())
          ((pred (car lst)) (cons (car lst) (find-all-in-list pred (cdr lst))))
          (else (find-all-in-list pred (cdr lst)))))

(define (first-n-elements lst n)
  (if (or (zero? n) (null? lst))
      (list)
      (cons (car lst) (first-n-elements (cdr lst) (- n 1)))))

(define (last-n-elements lst n)
  (if (> (length lst) n)
      (list-tail lst n)
      lst))

(define (maximum-group-size lst [max 0])
  (if (null? lst)
      max
      (if (> (send 'get-length (first lst)) max)
          (maximum-group-size (rest lst) (send 'get-length (first lst)))
          (maximum-group-size (rest lst) max))))

(define (minimum-group-size lst min)
  (if (null? lst)
      min
      (if (< (send 'get-length (first lst)) min)
          (minimum-group-size (rest lst) (send 'get-length (first lst)))
          (minimum-group-size (rest lst) min))))

(define (nth-element lst n)
  (cond ((null? lst) '())
        ((= n 0) (first lst))
        (else (nth-element (rest lst) (- n 1)))))

(define (display-all . vs)
  (for-each display vs))

(define (get-eths lst [ethnicities '()])
  (if (null? lst)
      ethnicities
      (let ((eth (send 'get-ethnicity (first lst))))
        (get-eths (filter (lambda (std) (not (equal? (send 'get-ethnicity std) eth))) lst) (cons eth ethnicities)))))

