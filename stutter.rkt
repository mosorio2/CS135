#lang eopl

(define (stutter lst)
  (cond
    [(null? lst)'()]
    [else (cons(car lst)(cons(car lst)(stutter(cdr lst))))]))
