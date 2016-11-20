#lang eopl

(define (nand p q)
  (not (and p q)))


(define (if->then p q)  
  (or (not p) q))

(define (xor p q)
  (or (and p (not q)) (and (not p) q)))

(define (nand-commute? p q)
  (equal? (nand p q) (nand q p)))

(define (if->then-commute? p q)     
  (and (equal? (if->then #t #t) (if->then #t #t))
       (equal? (if->then #t #f) (if->then #f #t))
       (equal? (if->then #f #t) (if->then #f #t))
       (equal? (if->then #f #f) (if->then #f #f))))


(define (if->then-com? p q)     
  (equal? (if->then #t #f) (if->then #f #t)))


(define (commutes? operator)
  (equal? (operator #t #f) (operator #f #t)))

(commutes? nand)
(commutes? if->then)

(define (or2 p q)
  (or p q))

(define (and2 p q)
  (and p q))

(define (law-binary? left right)
  (and (equal? (left #t #t) (right #t #t))
       (equal? (left #t #f) (right #t #f))
       (equal? (left #f #t) (right #f #t))
       (equal? (left #f #f) (right #f #f))))

(define (not-p-or-not-q p q)
  (or (not p) (not q)))


(define (nor p q)
  (not (or p q)))

(define (not-p-and-not-q p q)
  (and (not p) (not q)))

(law-binary? nor not-p-and-not-q)
(law-binary? nand nor)

(define (3majority p q r)
  (and (or p q) (or r q) (or p r)))

(define (isosceles p q r)
  (and (or (or (and p q) (and q r)) (and p r)) (not(and p q r))))

;Golden Ticket

;(define (law-ternary? (p->(q->r)) ((p->q)->r)))
;(and (equal? (left #t #t #t) (right #t #t #t))
 ;      (equal? (left #t #t #f) (right #t #t #f))
  ;     (equal? (left #t #f #t) (right #t #f #t))
   ;    (equal? (left #f #f #f) (right #f #f #f))
    ;   (equal? (left #f #f #t) (right #f #f #t))
     ;  (equal? (left #f #t #f) (right #f #t #f))
      ; (equal? (left #f #t #t) (right #f #t #t))
       ;(equal? (left #t #f #f) (right #t #f #f)))

