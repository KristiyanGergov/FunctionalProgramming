#lang racket
(require rackunit rackunit/text-ui)

(define (meetTwice? f g a b)
  (define (iter a found)
    (cond  [(= found 2) #t]
           [(> a b) #f]
           [(= (f a)(g a)) (iter (+ 1 a) (+ 1 found))]
           [else (iter (+ 1 a) found)]
           )
    )
  (iter a 0)
  )

(define (test x ) x)

(define meetTwice?-tests
  (test-suite
   "Tests for meetTwice"

   (check-true (meetTwice? (lambda(x)x) sqrt 0 5))
   (check-true (meetTwice? (lambda(x)x) (lambda(x)(* x x)) 0 5))

   (check-false (meetTwice? (lambda(x)x) (lambda(x)(* x x)) 1 5))
   (check-false (meetTwice? (lambda (x) x) sqrt 1 5))
   (check-false (meetTwice? (lambda(x)x) (lambda(x) (- x)) -3 1))

  )
)

(run-tests meetTwice?-tests)
