#lang racket

(define (selection-sort l)

  (define (remove x l)
    (define (!=x y) (not (= y x)))
    (append (take-while !=x l)
            (cdr (drop-while !=x l))))
  
    (let ((min-in-l (apply min l)))
    (cons min-in-l
          (selection-sort (remove min-in-l l)))))

  