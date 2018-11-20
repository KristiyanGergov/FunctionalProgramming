#lang racket

(define (keys alist)
  (map car alist))

(define (values alist)
  (map cdr alist))

(define (del-assoc key alist)
  (filter (lambda (key-value-pair)
            (not (equal? (car key-value-pair) key)))
          alist)
  )

(define (add-assoc key value alist)
  (let ((pair (assoc key alist)))
    (if (and pair
             (equal? (cdr pair) value))
        alist
    (cons (cons key value)
          alist))))

(add-assoc 1 2 '((3 . 4)))