#lang racket
(define (tree? t)
  (or (null? t)
      (and (list? t)
           (= (length t) 3)
           (tree? (cadr t))
           (tree? (caddr t)))))

(define (make-tree root left right)
  (list root left right))

(define root car)
(define left cadr)
(define right caddr)
(define empty? null?)

(define (leaf? tree)
  (and (not (empty? tree))
       (empty? (left tree))
       (empty? (right tree))))

(define (toBinary num)

  (define (iter number power binary)
    (cond [(= (/ number 2) 0) binary]
          [else (iter (quotient number 2)
                      (* power 10)
                      (+ binary (* power (remainder number 2))))]
          ))
  (iter num 1 0))

(define (sameAsCode tree)
  
  (define (iter tree path)
    (cond [(and (leaf? tree) (= path (toBinary (root tree)))) (root tree)]
          [(= path (toBinary (root tree))) (root tree)]
          [(and (not (empty? (left tree))) (not (empty? (right tree))))
           (+ (iter (left tree) (+ (* 10 path) 0))
              (iter (right tree) (+ (* 10 path) 1)))]
          [(not (empty? (left tree))) (iter (left tree) (+ (* 10 path) 0))]
          [(not (empty? (right tree))) (iter (right tree) (+ (* 10 path) 1))]
          [else 0]))
  
  (iter tree 1))

(sameAsCode `(5 (3 () (2 () ())) (4 (6 () ()) ())))

            