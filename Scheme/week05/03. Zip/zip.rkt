#lang racket

(define (zip x y)
  (if (or (null? x) (null? y))
          '()
          (cons (list (car x) (car y)) (zip (cdr x) (cdr y)))
          )
      )

(define (zip-with fn x y)
  (if (or (null? x) (null? y))
  '()
  (cons (fn (car x) (car y)) (zip-with fn (cdr x) (cdr y)))
  )
)


(define (zip-with-args fn . ls)
  (if (or (null? ls)
          (not (null? (filter null? ls))))
      '()
      (cons (apply fn (map car ls))
            (apply zip-with fn (map cdr ls)))))
            

(zip '(1 3 5) '(2 4 6 8))

(zip-with cons '(1 3 5) '(2 4 6))

(zip-with-args + '(1 3 5) '(2 4 6))

          
          