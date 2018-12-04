#lang racket
(cons 0 (cons 1 (cons 2 (cons 3 (cons 4 '())))))
'(1 . 2)

'(1 2 quote ())

(define l '(0 1 2 3 4))

(car l)

(cdr l)

(car (cdr l))

(cdr (cdr (cdr (cdr (cdr l)))))

(cadddr l)

(define l2 (cons 1 (cons 2 (cons 3 4))))
