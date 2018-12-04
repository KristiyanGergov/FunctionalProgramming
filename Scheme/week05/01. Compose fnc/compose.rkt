#lang racket

(define (identity x) x)

(define (compose . fns)
  (define (compose-two f g)
    (lambda (x)
      (f (g x))))

  (foldr compose-two identity fns))



(define (double x) (* 2 x))
(define (square x) (* x x))
(define (inc x) (+ x 1))

(define f (compose double square inc))

(f 3)