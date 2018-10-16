#lang racket
(define (fact n)
    (if (= n 0)
        1
    {* n (fact (- n 1))}))

(define (pow x n)
    (if (= n 0)
        1
        (* x (pow x (- n 1)))))

(define (approximate_e n x)
       (if (= n 0)
          1
          (+
            (/ (pow x n) (fact n))
            (approximate_e (- n 1) x)
          )
       )
    )