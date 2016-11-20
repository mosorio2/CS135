#lang eopl

(define (keep-evens list-of-ints)
  (cond
    [(equal? list-of-ints '())'()]
    [(equal? (modulo (car list-of-ints) 2) 0)(append (list(car list-of-ints)) (keep-evens (cdr list-of-ints)))]
    [else(keep-evens (cdr list-of-ints))]))


(define (filter predicate? list-of-things)
  (cond
    [(equal? list-of-things '())'()]
    [(predicate? (car list-of-things))(append (list (car list-of-things)) (filter predicate? (cdr list-of-things)))]
    [else(filter predicate? (cdr list-of-things))]))
   

(define (EuclidAlgo a d)
	(if (equal? (modulo a d) 0)
    	d
    	(EuclidAlgo d (modulo a d))))

(define (Euclid-list a d)
  (define (ELH a d)
    (cond
      [(equal? (modulo a d) 0) '()]
      [else(append (list (modulo a d)) (ELH d (modulo a d)))]))
  (append (list a d)(ELH a d)))
  
  
    

    
(define (integers n)
  (cdr (reverse (Integers n))))

(define (Integers n)
  (if (= 0 n)
  	'()
  	(cons n (Integers (- n 1)))))

(define (sieve n)
  (Sieve (integers n)))

(define (Sieve list-of-ints)
  (if (null? list-of-ints)
  	'()
  	(cons (car list-of-ints) (Sieve (cross-out (car list-of-ints) (cdr list-of-ints))))))

(define (cross-out prime list-of-ints)
  (cond
    [(equal? list-of-ints '())'()]
    [(equal? (modulo (car list-of-ints) prime) 0)(cross-out prime (cdr list-of-ints))]
    [else(cons (car list-of-ints) (cross-out prime (cdr list-of-ints)))]))

(define (pi n)
  (length (sieve n)))

(define (sieve-prime? n)
  (if (member n (sieve n))
  	#t
  	#f))

(define (prime? n)
  (Prime? 2 n))

(define (Prime? divisor n)
  (if (equal? divisor n)
  	#t
  	(if (equal? (modulo n divisor) 0)
      	#f
      	(Prime? (+ divisor 1) n))))

(define (factor n)
  (Factor 2 n))

(define (Factor divisor n)
  (if (> divisor n)
  	'()
  	(if (equal? (modulo n divisor) 0)
      	(cons divisor (Factor divisor (/ n divisor)))
      	(Factor (+ divisor 1) n))))

(define pal-dro-beans '(1111111 1113111 1117111 1131311 1133311 1137311 1171711 1173711 1177711 1311131 1313131 1317131 1331331 1333331 1337331 1371731 1373731 1377731 1711171 1713171 1717171 1731371 1733371 1737371 1771771 1773771 1777771 3111113 3113113 3117113 3131313 3133313 3137313 3171713 3173713 3177713 3311133 3313133 3317133 3331333 3333333 3337333 3371733 3373733 3377733 3711173 3713173 3717173 3731373 3733373 3737373 3771773 3773773 3777773 7111117 7113117 7117117 7131317 7133317 7137317 7171717 7173717 7177717 7311137 7313137 7317137 7331337 7333337 7337337 7371737 7373737 7377737 7711177 7713177 7717177 7731377 7733377 7737377 7771777 7773777 7777777))
(define (primes list)
  (filter prime? list))



(display(Euclid-list 100 51))
(display(sieve 100))
(display(pi 100))
(display(sieve-prime? 103))
(display(factor 126))
(display(primes '(1111111 1113111 1117111 1131311 1133311 1137311 1171711 1173711 1177711 1311131 1313131 1317131 1331331 1333331 1337331 1371731 1373731 1377731 1711171 1713171 1717171 1731371 1733371 1737371 1771771 1773771 1777771 3111113 3113113 3117113 3131313 3133313 3137313 3171713 3173713 3177713 3311133 3313133 3317133 3331333 3333333 3337333 3371733 3373733 3377733 3711173 3713173 3717173 3731373 3733373 3737373 3771773 3773773 3777773 7111117 7113117 7117117 7131317 7133317 7137317 7171717 7173717 7177717 7311137 7313137 7317137 7331337 7333337 7337337 7371737 7373737 7377737 7711177 7713177 7717177 7731377 7733377 7737377 7771777 7773777 7777777)))