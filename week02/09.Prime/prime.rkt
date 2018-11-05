#lang racket
(define (prime? n)
  (define (iter k)
    (cond
      [(= n 1) #f]
      [(= k (- n 1)) #t]
      [(= (remainder n k) 0) #f]
      [else (iter (+ k 1))]
      )
    )
  (iter 2)
  )

(require rackunit rackunit/text-ui)

(define prime?-tests
  (test-suite
   "Tests for prime?"

   (check-true (prime? 3))
   (check-true (prime? 19))
   (check-true (prime? 599))
   (check-true (prime? 661))
   (check-true (prime? 2221))
   (check-true (prime? 7879))

   (check-false (prime? 1))
   (check-false (prime? 12))
   (check-false (prime? 15))
   (check-false (prime? 42))
   (check-false (prime? 666))
   (check-false (prime? 1337))
   (check-false (prime? 65515))
   (check-false (prime? 1234567))))

(run-tests prime?-tests)