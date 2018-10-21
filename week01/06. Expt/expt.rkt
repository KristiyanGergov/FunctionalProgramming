#lang racket
 (define (expt x n)
    (cond
      [(> n 0) (* x (expt x (- n 1)))]
      [(< n 0) (* (/ 1 x) (expt x (+ n 1)))]
      [(= n 0) 1]
      )
    )