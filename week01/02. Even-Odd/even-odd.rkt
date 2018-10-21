#lang racket
(define (even? n)
    (= (remainder n 2) 0))
(define (odd? n)
    (= (remainder n 2) 1))