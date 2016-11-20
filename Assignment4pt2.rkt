#lang eopl
(define (build-seq start step end)
  (cond
    [(> start end)'()]
    [(negative? step) '()]
    [else (cons start(build-seq (+ start step) step end))]))
