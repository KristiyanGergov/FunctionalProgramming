#lang racket
(define (factorial n)
    (cond
      [(< n 0) #f]
      [(= n 1) n]
      (else ( * n (factorial (- n 1))))))