#lang racket

(define the-empty-stream '())

(define-syntax cons-stream
  (syntax-rules ()
    ((cons-stream h t)
     (cons h (delay t)))))

(define (empty-stream? s)
  (equal? s the-empty-stream))

(define head car)

(define (tail s)
  (force (cdr s)))


(define (stream-take n s)
  (if (or (= n 0)
      (empty-stream? s))
  '()
  (cons (head s)
        (stream-take (- n 1) (tail s)))))