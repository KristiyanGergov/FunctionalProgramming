#lang racket

(define l '(0 1 2 3 4))

(define (member? x l)
  (cond
    ([null? l] #f)
    ([= (car l) x] #t)
    (else (member? x (cdr l)))
  )
)
(member? 4 l)
