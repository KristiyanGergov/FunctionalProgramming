#lang racket

(define (juxt . fns)
  (lambda args
    (if (null? fns)
        '()
        (cons (apply (car fns) args)
              (apply (apply juxt (cdr fns)) args)
          
          )
    )
  )
)

(define g (juxt + *))
(g 3 4 5) ; => (12 60)