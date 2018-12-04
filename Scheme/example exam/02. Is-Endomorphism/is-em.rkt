#lang racket

(define (every? p l)
  (foldl (lambda (x acc)
           (and (p x) acc))
         #t
         l))

(define (any? p l)
  (foldl (lambda (x acc)
           (or (p x) acc))
         #f
         l))

(define (endomorphism? l op f)
  (and (every? (lambda (x)
                 (member (f x) l))
               l)
       (every? (lambda (x)
                 (every? (lambda (y)
                           (= (op (f x) (f y))
                              (f (op x y))))
                         l))
                 l)))
  