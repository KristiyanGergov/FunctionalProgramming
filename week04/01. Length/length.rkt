#lang racket

(define l '(0 1 2 3 4))

(define (length l)
    (if (null? (cdr l))
        1
        (+ 1 (length (cdr l)))
        )
)

