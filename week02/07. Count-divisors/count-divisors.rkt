#lang racket

(define (count-divisors n)
  (define (for start k divisors)
    (cond [(or (> start 9) (> start n)) divisors]
          [(or (> start k) (> (remainder k start) 0)) (for (+ start 1) n divisors)]
          [(= start 1) (for (+ start 1) k (+ divisors 1))]
          [else (for start (/ k start) (+ divisors 1))]
          )
    )
  (for 1 n 0)
  )

(count-divisors 15)
   