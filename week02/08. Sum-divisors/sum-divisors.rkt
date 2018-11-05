#lang racket
(define (sum-divisors n)
  (define (iter k sum)
    (cond
      [(= k 1) (+ sum 1)]
      [(= (remainder n k) 0) (iter (- k 1) (+ sum k))]
      [else (iter (- k 1) sum)]
      )
    )
  (iter n 0)
  )

(require rackunit rackunit/text-ui)

(define sum-divisors-tests
  (test-suite
   "Tests for sum-divisors"

   (check = (sum-divisors 1) 1) ; 1
   (check = (sum-divisors 3) 4) ; 1 3
   (check = (sum-divisors 12) 28) ; 1 2 3 4 6 12
   (check = (sum-divisors 15) 24) ; 1 3 5 15
   (check = (sum-divisors 19) 20) ; 1 19
   (check = (sum-divisors 42) 96) ; 1 2 3 6 7 14 21 42
   (check = (sum-divisors 661) 662) ; 1 661
   (check = (sum-divisors 666) 1482) ; 1 2 3 6 9 18 37 74 111 222 333 666
   (check = (sum-divisors 1337) 1536) ; 1 7 191 1337
   (check = (sum-divisors 65515) 78624) ; 1 5 13103 65515
   (check = (sum-divisors 1234567) 1244416))) ; 1 127 9721 1234567

(run-tests sum-divisors-tests)