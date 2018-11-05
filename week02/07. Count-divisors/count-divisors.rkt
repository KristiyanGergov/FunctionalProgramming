#lang racket

(define (count-divisors n)
  (define (for start k divisors)
    (cond
          [(= start 4) (for (+ start 1) k divisors)]
          [(and (> start 8) (> n 9)) (+ divisors 1)]
          [(or (> start 8) (> start n)) divisors]
          [(or (> start k) (> (remainder k start) 0)) (for (+ start 1) n divisors)]
          [(= start 1) (for (+ start 1) k (+ divisors 1))]
          [else (for start (/ k start) (+ divisors 1))]
          )
    )
  (for 1 n 0)
  )

; Works only for numbers less than 20