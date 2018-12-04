#lang racket

(define (take-while p l)
  (if (or (null? l)
          (not (p (car l))))
      l
      (cons (car l)
            (take-while p (cdr l)))))

(define (drop-while p l)
  (if (or (null? l)
          (not (p (car l))))
      l
      (drop-while p (cdr l))))

(define (next-look-and-say l)
  (cons (length (take-while (lambda (x)
                (= x (car l)))
              l
              ))
        (cons (car l)
              (next-look-and-say (drop-while (lambda (x)
                                               (= x (car l)))
                                             l)))))