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

(define (repeat value)
  (cons-stream value (repeat (+ 1 value))))

(define (stream-take n s)
  (if (or (= n 0)
      (empty-stream? s))
  '()
  (cons (head s)
        (stream-take (- n 1) (tail s)))))


(define (generateExpontents k l)

  (define (s)
    (if (< k l)
        k
        l))

  (define (b)
    (if (> k l)
        k
        l))
  
  (define (iter x y lastNumber res)

    (if (= (expt x (s)) (expt y (b)))
       (cons-stream (expt x (s))
                    (iter (+ x 1)
                          (+ y 1)
                          (expt x (s))
                          (cons-stream (expt x (s)) res)))


           (if (and (> (expt x (s)) lastNumber)
             (<= (expt x (s)) (expt y (b))))
      
               (cons-stream (expt x (s))
                            (iter (+ x 1)
                                  y
                                  (expt x (s))
                                  (cons-stream (expt x (s)) res)))
                                            
               (cons-stream (expt y (b))
                            (iter x
                                  (+ y 1)
                                  (expt y (b))
                                  (cons-stream (expt y (b)) res))))))
   
    
  (iter 1 1 0 the-empty-stream))