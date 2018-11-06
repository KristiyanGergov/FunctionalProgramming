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
  (if (null? l)
      l
 (reverse (iter (car l) -1 l '())))
)

(require rackunit rackunit/text-ui)

(define next-look-and-say-tests
  (test-suite
   "Tests for next-look-and-say"

   (check-equal? (next-look-and-say '()) '())
   (check-equal? (next-look-and-say '(1)) '(1 1))
   (check-equal? (next-look-and-say '(1 1 2 3 3)) '(2 1 1 2 2 3))
   (check-equal? (next-look-and-say '(1 1 2 2 3 3 3 3)) '(2 1 2 2 4 3))))

(run-tests next-look-and-say-tests)