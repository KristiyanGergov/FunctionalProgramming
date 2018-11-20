#lang racket

(define (flatmap fn l)
  (apply append (map fn l)))


(define (edges g)
  (flatmap (lambda (vertex)
         (map  (lambda (child)
               (list vertex child))
               (children vertex g)) 
         )
   (vertices g)))

(define (make-graph) '())

(define (invert g)
  (foldl (lambda (edge result-graph)
           (add-edge (cadr edge)
                     (car edge)
                     result-graph))
         (make-graph)
         (edges g)))