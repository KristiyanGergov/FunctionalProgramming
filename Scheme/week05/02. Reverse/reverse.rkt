#lang racket

(define (flip fn)

  (lambda args
    (apply fn (reverse args))))

(define list^ (flip list))
(list^ 1 2 3)