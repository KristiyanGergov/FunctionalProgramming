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
  (if (or (= n 0) (empty-stream? s))
      '()
      (cons (head s)
             (stream-take (- n 1) (tail s)))))

; T A S K 1

(define (repeat value)
  (cons-stream value (repeat value)))

;(stream-take 1 (repeat 1))

; T A S K 2

(define (cycle l)
  (define (iter remaining)
    (if (null? remaining)
        (iter l)
        (cons-stream (car remaining) (iter (cdr remaining)))))
  (if (null? l)
  the-empty-stream
  (iter l)))

; T A S K 3

(define (iterate f x)
  (cons-stream x (iterate f (f x))))

;(stream-take 10 (iterate (lambda (x) (+ x x)) 2))



; T A S K 4

(define (1+ x) (+ x 1))

(define (integers-from n)
  (iterate 1+ n))

(define (range-stream from to)
  (define (take n s)
    (if (or (= n 0) (empty-stream? s))
        the-empty-stream
        (cons-stream (head s)
                     (take (- n 1) (tail s)))))
       
  (take (+ to (- from) 1)
               (integers-from from))
  )

(define (filter-stream p s)
  (cond ((empty-stream? s) the-empty-stream)
        ((p (head s)) (cons-stream (head s)
                                   (filter-stream p (tail s))))
        (else (filter-stream p (tail s)))))


(define (map-stream f s)
  (if (empty-stream? s)
      the-empty-stream
      (cons-stream (f (head s))
                   (map-stream f (tail s)))))

(define (append-streams s1 s2)
  (if (empty-stream? s1)
      s2
      (cons-stream (head s1)
                   (append-streams (tail s1) s2))))


(define (concat-streams ss)
  (cond ((empty-stream? ss) the-empty-stream)
        ;((empty-stream? (head ss)) (concat-streams (tail ss)))
        (else (append-streams (head ss)
                              (concat-streams (tail ss))))))

(define (flatmap-stream f s)
  (concat-streams (map-stream f s)))


(define triples
  (flatmap-stream (lambda (c)
                (flatmap-stream (lambda (b)
                              (map-stream (lambda (a)
                                            (list a b c))
                                          (range-stream 1 b)))
                            (range-stream 1 c)))
              (range-stream 1 100)))

(define (square x) (* x x))

(define pythagorean-triples
  (filter-stream (lambda (triple)
                         (= (+ (square (car triple))
                               (square (cadr triple)))
                            (square (caddr triple))))
                 
                 triples))
                               