#lang racket
(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a)
                (accumulate combiner null-value term (next a) next b)
      )
      )
  )

(define (count predicate a b)
  (accumulate (lambda (x acc)
                         (if (predicate x)
                             (+ acc 1)
                             acc))
              0
              (lambda (x) x)
              a
              (lambda (x) (+ x 1))
              b
              )
)

(require rackunit rackunit/text-ui)

(define count-tests
  (test-suite
   "Tests for count"

   (check = (count even? 1 5) 2)
   (check = (count even? 0 10) 6)

   (check = (count odd? 1 5) 3)
   (check = (count odd? 0 10) 5)))

(run-tests count-tests)