#lang racket

(define(sum-numbers numbers)
  (cond
    [(empty? numbers) 0]
    [(+ (car numbers) (sum-numbers (cdr numbers)))]))

(display "Testing sum-numbers")
(newline)
(display "Expected: 0, actual: ")
(sum-numbers empty)
(display "Expected: 21, actual: ")
(sum-numbers (list 1 2 3 4 5 6))
(newline)

(define(average numbers)
  (cond
    [(empty? numbers) 0]
    [ (/ (sum-numbers numbers) (+ 1 (average (cdr numbers))))]))

(display "Testing average")
(newline)
(display "Expected: 3.5, actual: ")
(average (list 1 2 3 4 5 6))
(newline)

(define (occurrences items target)
  (cond
    [(empty? items) 0]
    [(if (= (car items) target) (+ 1 (occurrences (cdr items) target)) (occurrences (cdr items) target))]))

(display "Testing occurrences")
(newline)
(display "Expected: 3, actual: ")
(occurrences '(1 3 5 2 7 5 8 9 5) 5)
(display "Expected: 0, actual: ")
(occurrences '(1 3 5 2 7 5 8 9 5) 6)
(display "Expected: 0, actual: ")
(occurrences empty 1)
(newline)

(define (convert numbers)
  (cond
    [(empty? numbers) 0]
    [(empty? (cdr numbers)) (car numbers)]
    [(+ (car numbers) (* 10 (convert (cdr numbers))))]))


(display "Testing convert")
(newline)
(display "Expected: 0, actual: ")
(convert empty)
(display "Expected: 3, actual: ")
(convert (list 3))
(display "Expected: 543, actual: ")
(convert (list 3 4 5))
(newline)


(define (FC number)
  (/(*(- number 32)5)9))

(define (convertFC numbers)
  (cond
    [(empty? numbers) (list)]
    [(cons (FC (car numbers)) (convertFC (cdr numbers)))]))


(display "Testing convertFC")
(newline)
(display "Expected: '(), actual: ")
(convertFC empty)
(display "Expected: '(0 100 37.0), actual: ")
(convertFC (list 32 212 98.6))
(newline)
(define (eliminate-threshold numbers threshold)
  (cond
    [(empty? numbers) (list)]
    [(if (<= (car numbers) threshold) (cons (car numbers) (eliminate-threshold (cdr numbers) threshold)) (eliminate-threshold (cdr numbers) threshold))]))


(display "Testing eliminate-threshold")
(newline)
(display "Expected: '(1 2 3 4 4 3 2 1), actual: ")
(eliminate-threshold (list 1 2 3 4 5 6 5 4 3 2 1 20) 4)
(display "Expected: '(), actual: ")
(eliminate-threshold (list 1 2 3 4 5 6 5 4 3 2 1 20) 0)
(display "Expected: '(1 2 3 4 5 6 5 4 3 2 1 20), actual: ")
(eliminate-threshold (list 1 2 3 4 5 6 5 4 3 2 1 20) 25)
(newline)