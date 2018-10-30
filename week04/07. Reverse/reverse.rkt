#lang racket

(define l '(0 1 2 3 4))

(define (reverse l)

  (define (for l res)
    (if (null? l)
        res
        (for (cdr l) (cons (car l) res))
        )
    )
  (for l '())
  )

(reverse l)