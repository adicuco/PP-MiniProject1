; name: Adrian-Alexandru Cucolaș
; study no.: 20191000
; email: adicucol19@student.aau.dk

#lang racket

(require "utils.rkt")
(require "group.rkt")

(provide grouping create-random-grouping create-counting-grouping create-counting
         create-balanced-grouping create-random-predicate-grouping)

; Grouping object simulation
(define (grouping groups students-count [type 'grouping])
  (letrec ((get-groups (lambda () groups))
           (get-groups-count (lambda () (length groups)))
           (get-maximum-group-size (lambda () (maximum-group-size groups)))
           (get-minimum-group-size (lambda () (minimum-group-size groups students-count)))
           (print-groups (lambda () (for-each (lambda (grp) (send 'print grp)) groups)))
           (type-of (lambda () type))
           (print-info (lambda () (display-all
                                   "count: " (get-groups-count) "\n"
                                   "max: " (get-maximum-group-size) "\n"
                                   "min: " (get-minimum-group-size) "\n"
                                   "type: " (type-of) "\n")))
          )
    (lambda (message)
      (cond ((eq? message 'get-groups) get-groups)
            ((eq? message 'get-groups-count) get-groups-count)
            ((eq? message 'get-maximum-group-size) get-maximum-group-size)
            ((eq? message 'get-minimum-group-size) get-minimum-group-size)
            ((eq? message 'print) print-info)
            ((eq? message 'print-groups) print-groups)
            (else (error "Message not understood"))))))
 
;------------------------------ Ranndom Grouping ------------------------------
; Random Grouping constructor
; returns Grouping
(define (create-random-grouping sl gsl)
  (grouping (create-random-grouping-helper sl gsl) (length sl) 'grouping-random))

; returns a list of groups with incremented ids of index
; containing maximum gls randomized students from the students list sl
; if there is a predicate pred, try at most 100 times to produce a true value with new_group
(define (create-random-grouping-helper sl gsl [pred '()] [loop 0] [index 1])
  (letrec ((randomized-list (shuffle sl))
         (new-group (first-n-elements randomized-list gsl))
         (tail (last-n-elements randomized-list gsl))
         (add (lambda ()
                (cons
                 (group index new-group)
                 (create-random-grouping-helper tail gsl pred 0 (+ index 1)))))
        )
    (cond ((= (length sl) (length tail)) (cons (group index tail) '()))
          ((not (null? pred))
           (if (< loop 100)
               (if (pred new-group)
                   (add)
                   (create-random-grouping-helper sl gsl pred (+ loop 1) index))
               (add)))
          (else (add)))))

;------------------------------ Ranndom Predicate Grouping ------------------------------
; Random Predicate Grouping constructor
; returns Grouping
(define (create-random-predicate-grouping pred sl gsl)
  (grouping (create-random-grouping-helper sl gsl pred) (length sl) 'grouping-random-predicate))


;------------------------------ Counting Grouping ------------------------------
; Counting Grouping constructor
; returns Grouping by parsing an associtive list
(define (create-counting sl gsl)
  (grouping (parse-associative (create-counting-helper sl gsl) sl) (length sl) 'grouping-counting))

; returns an associative list of k groups with incremented ids of index
; cons groups of form (group-id . student-id) recursively increasing the index until index == k+1
; then decrease index to 1 and continue until stds empty
(define (create-counting-helper stds k [index 1])
  (cond ((null? stds) '())
        ((equal? (+ k 1) index) (create-counting-helper stds k 1))
        (else
         (cons
          (cons index
                (send 'get-id (first stds))) (create-counting-helper (rest stds) k (+ index 1))))))

;------------------------------ Counting Grouping Alternative ------------------------------
; Counting Grouping Alternative constructor
; returns Grouping
(define (create-counting-grouping sl gsl)
  (grouping (create-counting-grouping-helper sl gsl) (length sl) 'grouping-counting-alternative))

; returns a list of gsl groups with incremented ids of index
(define (create-counting-grouping-helper sl gsl [k 0] [index 1])
 (if (> index gsl)
     '()
     (cons
      (group index (create-counting-group-helper sl gsl k))
      (create-counting-grouping-helper sl gsl (+ k 1) (+ index 1)))))

; returns a list of students taken from sl at position k incremented by gsl
(define (create-counting-group-helper sl gsl k)
  (if (null? (nth-element sl k))
      '()
      (cons (nth-element sl k) (create-counting-group-helper sl gsl (+ k gsl)))))

;------------------------------ Counting Balanced Grouping ------------------------------
; Counting Balanced Grouping constructor
; returns Grouping by parsing an associative list
(define (create-balanced-grouping sl gsl)
  (grouping (parse-associative
             (create-balanced-grouping-helper sl gsl) sl) (length sl) 'grouping-counting-balanced))

; returns an associative list of k groups with incremented ids of index
; maps over ethnicities of both genders and incremently add stundets to groups by counting
(define (create-balanced-grouping-helper lst k [sex "male"] [index 1])
  (letrec ((eths (get-eths lst))
        (stds (filter (lambda (student) (equal? (send 'get-sex student) sex)) lst))
        (half-grp (car (map (lambda (eth)
                    (create-counting-helper stds k index)) eths))))
    (cond ((equal? sex "stop") '())
          ((equal? sex "female")
           (car (cons half-grp
                      (create-balanced-grouping-helper lst k "stop"))))
          ((equal? sex "male")
            (append half-grp
                  (create-balanced-grouping-helper lst k "female" (modulo (length stds) k)))))))

;------------------------------ Parsers ------------------------------
; returns a list of groups by filtering students of a group with id index and parsing into a Group
(define (parse-associative lst stds [index 1])
  (let ((new-group (filter (lambda (grp) (equal? (car grp) index)) lst)))
    (if (null? new-group)
        '()
        (cons (group index (parse-associative-helper new-group stds))
              (parse-associative lst stds (+ index 1))))))

; returns a list of students from all students stds which have the ids of students from lst
(define (parse-associative-helper lst stds)
  (map (lambda (std)
         (find-first-in-list (lambda (st) (equal? (cdr std) (send 'get-id st))) stds)) lst))