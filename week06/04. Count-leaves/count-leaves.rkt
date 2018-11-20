#lang racket

(define root car)
(define left cadr)
(define right caddr)
(define empty? null?)

(define (leaf? tree)
  (and (not (empty? tree))
       (empty? (left tree))
       (empty? (right tree)))
  )


(define (count-leaves tree)

  (cond ((empty? tree) 0 )
        ((leaf? tree) 1 )
        (else (+ (count-leaves (left tree))
              (count-leaves (right tree)))
          ))
)
     
  