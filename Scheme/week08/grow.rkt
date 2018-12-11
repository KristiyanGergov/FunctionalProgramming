#lang racket

(define the-empty-tree '())

(define (make-tree root left right)
  (list root left right))

(define (make-leaf x)
  (make-tree x the-empty-tree the-empty-tree))


(define root car)
(define left cadr)
(define right caddr)
(define empty? null?)

(define (leaf? tree)
  (and (not (empty? tree))
       (empty? (left tree))
       (empty? (right tree))))

(define (grow t x)
  (cond [empty? t the-empty-tree]
        [(leaf? t) (make-tree (root t)
                              (make-leaf x)
                              (make-leaf x))]
        [else (make-tree (root t)
                         (grow (left t) x)
                         (grow (right t) x))]))

(define (growing-trees)
  (define (generate t level)
    (stream-cons t
                 (generate (grow t level)
                           (+ level 1))))
  (generate (make-leaf 0) 1))
