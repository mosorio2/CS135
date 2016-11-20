#lang eopl
(define (Sum n)
  (cond
    [(eq? n 1)1]
    [(eq? n 0) 0]
    [else (+ n (Sum (- n 1)))]))