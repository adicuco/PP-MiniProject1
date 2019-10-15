#lang racket

(require "utils.rkt")
(require "group.rkt")

(provide grouping create-random-grouping create-counting-grouping create-counting)

(define (grouping groups students-count)
  (letrec ((get-groups (lambda () groups))
           (get-groups-count (lambda () (length groups)))
           (get-maximum-group-size (lambda () (maximum-group-size groups)))
           (get-minimum-group-size (lambda () (minimum-group-size groups students-count)))
           (print-info (lambda () (display-all
                                   "count: " (get-groups-count) "\n"
                                   "max: " (get-maximum-group-size) "\n"
                                   "min: " (get-minimum-group-size) "\n")))
           (type-of (lambda () 'grouping))
          )
    (lambda (message)
      (cond ((eq? message 'get-groups) get-groups)
            ((eq? message 'get-groups-count) get-groups-count)
            ((eq? message 'get-maximum-group-size) get-maximum-group-size)
            ((eq? message 'get-minimum-group-size) get-minimum-group-size)
            ((eq? message 'print) print-info)
            (else (error "Message not understood"))))))

;------------------------------ Parsers ------------------------------
(define (parse-associative-helper lst stds)
  (map (lambda (std)
         (find-first-in-list (lambda (st) (equal? (cdr std) (send 'get-id st))) stds)) lst))

(define (parse-associative lst stds [index 1])
  (let ((new-group (filter (lambda (grp) (equal? (car grp) index)) lst)))
    (if (null? new-group)
        '()
        (cons (group index (parse-associative-helper new-group stds))
              (parse-associative lst stds (+ index 1))))))
 
;------------------------------ Ranndom Grouping ------------------------------
(define (create-random-grouping-helper sl gsl [index 1])
  (let* ((randomized-list (shuffle sl))
         (new-group (first-n-elements randomized-list gsl))
         (tail (last-n-elements randomized-list gsl))
        )
    (if (eq? (length sl) (length tail))
      (cons (group index tail) '())
      (cons (group index new-group) (create-random-grouping-helper tail gsl (+ index 1))))))

(define (create-random-grouping sl gsl)
  (grouping (create-random-grouping-helper sl gsl) (length sl)))
  

;------------------------------ Counting Grouping ------------------------------
(define (create-counting-helper stds k [index 1])
  (cond ((null? stds) '())
        ((equal? (+ k 1) index) (create-counting-helper stds k 1))
        (else (cons (cons index (send 'get-id (first stds))) (create-counting-helper (rest stds) k (+ index 1))))))

(define (create-counting sl gsl)
  (grouping (parse-associative (create-counting-helper sl gsl) sl) (length sl)))

(define (create-counting-group-helper sl gsl k index)
  (if (null? (nth-element sl k))
      '()
      (cons (nth-element sl k) (create-counting-group-helper sl gsl (+ k gsl) index))))

(define (create-counting-grouping-helper sl gsl [k 0] [index 1])
 (if (> index gsl)
     '()
     (cons
      (group index (create-counting-group-helper sl gsl k index))
      (create-counting-grouping-helper sl gsl (+ k 1) (+ index 1)))))

(define (create-counting-grouping sl gsl)
  (grouping (create-counting-grouping-helper sl gsl) (length sl)))
