#lang racket
(define (accumulate-iter combiner null-value term a next b)
  (define (iter acc a)
    (if (> a b)
        acc
        (iter (combiner (term a) acc)
              (next a))))
  (iter null-value a)
 )

(define (sum term a next b)
  (accumulate-iter + 0 term a next b)
 )

(require rackunit rackunit/text-ui)

(define (identity x) x)
(define (1+ x) (+ x 1))
(define (square x) (* x x))

(define accumulate-iter-tests
  (test-suite
   "Tests for accumulate-iter"

   (check = (accumulate-iter + 0 identity 1 1+ 5) 15)
   (check = (accumulate-iter + 0 square 1 1+ 5) 55)

   (check = (accumulate-iter * 0 identity 1 1+ 5) 0)
   (check = (accumulate-iter * 1 identity 0 1+ 5) 0)
   (check = (accumulate-iter * 1 identity 1 1+ 5) 120)

   (check = (accumulate-iter (lambda (x acc)
                               (if (even? x) (1+ acc) acc))
                             0
                             identity
                             0 1+ 10)
          6)
   (check = (accumulate-iter +
                             0
                             (lambda (x)
                               (if (even? x) 1 0))
                             0 1+ 10)
          6)))

(run-tests accumulate-iter-tests)