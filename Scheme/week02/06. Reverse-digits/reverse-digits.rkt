#lang racket

(define (count-digits n)
    (if (< n 10)
        1
        (+ 1 (count-digits (/ n 10)))
    )
 )


(define (reverse-digits n)
  (if (= n 0)
      0
  (+
   (*
      (expt 10 (- (count-digits n) 1))
      (remainder n 10))
   (reverse-digits (quotient n 10))
  )
  )
  )

(require rackunit rackunit/text-ui)

(define reverse-digits-tests
  (test-suite
   "Tests for reverse-digits"

   (check = (reverse-digits 0) 0)
   (check = (reverse-digits 3) 3)
   (check = (reverse-digits 12) 21)
   (check = (reverse-digits 42) 24)
   (check = (reverse-digits 666) 666)
   (check = (reverse-digits 1337) 7331)
   (check = (reverse-digits 65510) 1556)
   (check = (reverse-digits 1234567) 7654321)
   (check = (reverse-digits 8833443388) 8833443388)
   (check = (reverse-digits 100000000000) 1)))

(run-tests reverse-digits-tests)