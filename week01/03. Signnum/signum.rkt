(define (signum x)
    (cond
      [(> x 0) 1]
      [(< x 0) -1]
      [(= x 0) 0])
    )