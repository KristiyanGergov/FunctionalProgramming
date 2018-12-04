#lang racket

(define l '(0 1 2 3 4))

(define (sum l)
  (if (null? l)
      0
      (+ (car l) (sum (cdr l)))
      )
  )

(sum l)