#lang racket

(define l '(0 1 2 3 4))

(define (last l)
  (if (null? (cdr l))
      (car l)
      (last (cdr l))
      )
  )

(last l)