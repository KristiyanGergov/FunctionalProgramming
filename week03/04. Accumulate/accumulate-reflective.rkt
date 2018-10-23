#lang racket
(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a)
                (accumulate combiner null-value term (next a) next b)
      )
      )
  )

(define (sum term a next b)
  (accumulate + 0 term a next b))


(require rackunit rackunit/text-ui)

(define (identity x) x)
(define (1+ x) (+ x 1))
(define (square x) (* x x))