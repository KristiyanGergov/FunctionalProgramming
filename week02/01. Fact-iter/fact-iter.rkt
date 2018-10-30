#lang racket

(define (factorial-iter num)
  (define (fact-iter product counter max-count)
  (if (> counter max-count)
      product
      (fact-iter (* counter product)
                 (+ counter 1)
                 max-count))
    )
  
    (fact-iter 1 1 num)
  )

(require rackunit rackunit/text-ui)

(define factorial-iter-tests
  (test-suite
   "Tests for factorial-iter"

   (check = (factorial-iter 0) 1)
   (check = (factorial-iter 1) 1)
   (check = (factorial-iter 2) 2)
   (check = (factorial-iter 3) 6)
   (check = (factorial-iter 4) 24)
   (check = (factorial-iter 5) 120)
   (check = (factorial-iter 6) 720)
   (check = (factorial-iter 7) 5040)))

(run-tests factorial-iter-tests)