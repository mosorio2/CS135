#lang eopl

(map abs '(1 -1 2 -2 3 -5)) 
(map even? '(0 1 2 3 4 5)) 

(define happy '(h a p p y))
(define birthday '(b i r t h d a y))
(define ay '(a y))

;(define (pig-latin wordlist)
  ;(append (cdr wordlist)(cons(car wordlist)'(ay))))

(define (pig-latin word)
  (if (null? word)
      '()
      (equal? 'q (car word)))
      (append (cdr(cdr word)) (append'(q u) '(a y))))




(define (BMI height weight)
  (define bmi(/(* 704.5 weight)(* height height)))
  (cond ((< bmi 16) (display "Severely underweight"))
        ((< bmi 18.5) (display "considered underweight"))
        ((> bmi 18.5) (display "normal"))
        ((> bmi 25)(display "overweight"))
        ((bmi > 30) (display "obese"))
        (else (display "Morbidly obese"))))



(define (arithmetic-prog a_0 difference iNdex)
  (cond
    [(equal? iNdex 0) '()]
    [else (cons a_0 (arithmetic-prog (+ a_0 difference) difference (- iNdex 1)))]))


(define (geometric-prog a_0 ratio iNdex)
  (cond
    [(equal? iNdex 0) '()]
    [else (cons a_0 (geometric-prog (* a_0 ratio) ratio (- iNdex 1)))]))
  

(define (sum list-of-nums)
  (cond
    [(null? list-of-nums)0]
    [else (+ (car list-of-nums) (sum(cdr list-of-nums)))]))

(define (arith-sum a_0 difference iNdex)
  (+(* a_0 iNdex) (/(*(* difference iNdex)(- iNdex 1))2)))

(define (geo-sum a_0 ratio iNdex)
  (* a_0 (/(-(expt ratio iNdex) 1) (- ratio 1))))

(define (make-duples item list-of-items)
  (cond
    [(null? list-of-items)'()]
    [else( cons(cons item (list (car list-of-items))) (make-duples item (cdr list-of-items)))]))

(define (cart-prod setA setB)
  (cond
    [(or (null? setA) (null? setB))'()]
    [else(map(append(cons(car setA) (cdr setB))))]))

(display(map pig-latin '((q u i r k y) (q u e a s y) (q u e s t))))
(display(arithmetic-prog 2 3 5))
(display(geometric-prog 1 10 5))
(display(sum '(1 2 3 4)))
(display(arith-sum 1 3 4))
(display(geo-sum 1 3 4))
(display(make-duples 0 '(1 2 3 4)))
