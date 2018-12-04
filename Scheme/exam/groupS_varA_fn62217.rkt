#lang racket

; F I R S T    T A S K

; a)
(define (count-digits n)
  (define (iter n digits)
    (cond [(= n 0) digits]
          [else (iter (quotient n 10) (+ digits 1))]
          )
    )
  (iter n -1)
  )

(define (diffReverse n)
  (define (reverse n k)
    (if (= (count-digits n) 0)
    (+ k n)
    (reverse (quotient n 10) (+ k (* (remainder n 10) (expt 10 (count-digits n)))))
    )
    )
  (- n (reverse n 0))
  )
; a)

; b)
(define (min n lastMin)
  (define (iter n currMin)
    (cond [(= currMin lastMin) -1]
          [(< currMin 0) (iter (quotient n 10) (remainder n 10))]
          [(and (= n 0) (< currMin lastMin)) -1]
          [(= n 0) currMin]
          [(and (< (remainder n 10) currMin) (> (remainder n 10) lastMin)) (iter (quotient n 10) (remainder n 10))]
          [else (iter (quotient n 10) currMin)]
          )
    )
  (iter n -1)
  )

(define (count-min n min)
  (define (iter n count)
           (cond [(= n 0) count]
                 [(= (remainder n 10) min) (iter (quotient n 10) (+ count 1))]
                 [else (iter (quotient n 10) count)]
                 )
    )
  (iter n 0)
  )

(define (sortDigits n)
  (define (iter count lastMin res)
    (cond [(= lastMin -1) res]
          [else (iter (+ count 1) lastMin (+ (expt 10 count) 
    
    )
)
    (iter (count-min n (min n 0)) (min n 0))
  )


  

; b)


; T A S K 2

(define (sum f g x)
  (define (iter start res)
    (cond [(> start x) res]
          [else (iter (+ start 2) (f (g x)))]
  )
    )
  (iter 0 0)
  )
                  

(define (permutable? a b f g)
  (define (iter x res)
    (cond [(not res) #f]
          [(> x b) #t]
          [(odd? x) (iter (+ x 1) res)]
          [else (iter (+ x 1) (= (sum f g x) (sum g f x)))]
          )
    )
    (iter a #t)
 )
  
; T A S K 2

; T A S K 3


;(define (find-longest-interval l)
;  (define (iter-l l res)
;    (if (null? l)
;        res
;        (cons
;         (define (iter longest-interval l)
;          (cond [(null? l) longest-interval]
;            []))))))
         
  

;(define (longest-interval-subsets l)
  

