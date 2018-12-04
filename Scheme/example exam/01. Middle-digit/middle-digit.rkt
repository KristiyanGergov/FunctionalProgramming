#lang racket

(define (count-digits n)
  (define (iter start num)
    (if (= num 0)
        start
        (iter (+ start 1) (quotient num 10))
        )
    )
  (iter 0 n)
  )

(define (middle-digit n)
  (define (iter k index digits)
    (cond
      [(= (remainder digits 2) 0) -1]
      [(= index (quotient digits 2)) (abs (remainder k 10))]
      [else (iter (quotient k 10) (+ index 1) digits)]
    )
    )
  (iter n 0 (count-digits n)) 
  )

(middle-digit -1234567)
  