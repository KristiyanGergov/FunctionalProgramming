#lang racket
(define (count-digits n)
    (if (< n 10)
        1
        (+ 1 (count-digits (/ n 10)))
    )
 )