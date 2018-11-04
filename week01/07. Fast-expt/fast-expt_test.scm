#lang racket

(define (expt x n)
    (cond [(> n 0) (* x (expt x (- n 1)))]
          [(< n 0) (* (/ 1 x) (expt (/ 1 x) (- (- n) 1)))]
          [else 1]
          )
    )

(define (fast-expt x n)
    (if (= (remainder n 2) 0)
        (expt (expt x (/ n 2)) 2)
        (expt x n)
        )
    )

(require rackunit rackunit/text-ui)

(define fast-expt-tests
  (test-suite
   "Tests for fast-expt"

   (check = (fast-expt -2 -11) -1/2048)
   (check = (fast-expt -2 -10) 1/1024)
   (check = (fast-expt 2 -10) 1/1024)
   (check = (fast-expt 5 -3) 1/125)
   (check = (fast-expt 3 -2) 1/9)
   (check = (fast-expt 2 -2) 1/4)
   (check = (fast-expt 2 -1) 1/2)
   (check = (fast-expt 2 0) 1)
   (check = (fast-expt 2 1) 2)
   (check = (fast-expt 2 2) 4)
   (check = (fast-expt 3 2) 9)
   (check = (fast-expt 5 3) 125)
   (check = (fast-expt 2 10) 1024)
   (check = (fast-expt -2 10) 1024)
   (check = (fast-expt -2 11) -2048)))

(run-tests fast-expt-tests)