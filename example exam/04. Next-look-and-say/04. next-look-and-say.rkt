#lang racket


(define (reverse l)
  (define (iter l res)
    (if (null? l)
        res
       (iter (cdr l) (cons (car l) res)))
    )
  (iter l '())
)

(define (next-look-and-say l)
  (define (iter lastNum count l res)
    (cond [(null? l) (cons lastNum (cons (+ count 1) res))]
          [(= (car l) lastNum) (iter lastNum (+ count 1) (cdr l) res)]
          [else (iter (car l) 0 (cdr l) (cons lastNum (cons (+ count 1) res)))]
    )
  )
 (reverse (iter (car l) -1 l '()))
)

(next-look-and-say '(1 1 2 3 3))