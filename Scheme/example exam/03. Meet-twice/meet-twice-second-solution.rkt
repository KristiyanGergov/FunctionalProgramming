#lang racket

(define (exist? predicate a b)
  (and (<= a b)
       (or (predicate a)
           (exist? predicate (+ a 1) b))))

(define (meet-twice? f g a b)
  (exist? (lambda (x)
            (exist? (lambda (y)
                      (and (not (= x y))
                           (= (f x) (g x))
                           (= (f y) (g y))))
                    a
                    b
                    )
            )
            a
            b
          )
  )
                      
      