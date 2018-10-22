#lang racket
(define (count-digits n)
    (define (for num digits)
      (if (< num 10)
          digits
          (for (/ num 10) (+ digits 1))
          )
      )
    (for n 1)
)
        