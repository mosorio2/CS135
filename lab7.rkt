#lang eopl
(define (digitize n)
  (if (equal? n 0)
  	'()
  	(append (digitize (quotient n 10)) (list (modulo n 10)))))

(define (sum-digits list)
  (if (equal? list '())
  	0
  	(+ (sum-digits (cdr list)) (car list))))
 
(define (even-parity? bitstring)
  (if (even? (sum-digits bitstring))
  	#t
  	#f))

(define (parity-bit bitstring)
  (if (equal? (even-parity? bitstring) #t)
  	0
  	1))

(define (Upc list-of-ints)
  (if (equal? list-of-ints '())
  	0
  	(+ (* (car list-of-ints) 3) (cadr list-of-ints) (Upc (cddr list-of-ints)))))

(define (upc? list-of-ints)
  (if (equal? (modulo (Upc list-of-ints) 10) 0)
  	#t
  	#f))

(define (Upc11 list-of-ints)
  (if (equal? (length list-of-ints) 1)
  	(* (car list-of-ints) 3)
  	(+ (* (car list-of-ints) 3) (cadr list-of-ints) (Upc11 (cddr list-of-ints)))))

(define (upc-check-digit list-of-ints)
  (if (equal? (- 10 (modulo (Upc11 list-of-ints) 10)) 10)
  	0
  	(- 10 (modulo (Upc11 list-of-ints) 10))))

(define tomato-soup '(0 5 1 0 0 0 1 4 8 7 2 8))
(define Aleve320 '(3 2 5 8 6 6 5 5 1 1 2 2))
(define hotSalsa '(0 2 8 4 0 0 0 5 5 9 9 4))
(define FlipSide '(0 1 9 2 7 5 0 5 4 6 0 1))
(define PuffsTissues '(0 3 7 0 0 0 6 2 2 6 2))
(define ChunkyChickenNoodle '(0 5 1 0 0 0 1 6 7 7 5))

(define (isbn tenlist n)
  (if (equal? n 1)
  	0
  	(+ (* (car tenlist) n) (isbn (cdr tenlist) (- n 1)))))

(define (isbnmod sum check)
  (if (equal? (modulo sum 11) 0)
  	check
  	(isbnmod (+ sum 1) (+ check 1))))

(define (ISBN? tenlist)
  (if (equal? (car (reverse tenlist)) (isbnmod (isbn tenlist 10) 0))
  	#t
  	#f))
 
(define winningways1 '(1 5 6 8 8 1 1 3 0 6))   
(define winningways2 '(1 5 6 8 8 1 1 4 2 10))  
(define winningways3 '(1 6 5 8 8 1 1 4 3 8))   
(define winningways4 '(1 5 6 8 8 1 1 4 4 6))   
(define ritchie '(0 1 3 1 1 0 3 7 0 8))   	 
(define ritchiePBK '(0 1 3 1 1 0 3 6 2 8))  

(define secret 48420295610439194978804589923121515475455500393246020365720930824673275)

(define (convert number base)
  (reverse (Convert number base)))

(define (Convert number base)
  (if (= number 0)
  	'()
  	(cons (modulo number base) (Convert (quotient number base) base))))

(define (base10 oldbase numberlist)
  (Base10 oldbase (reverse numberlist) 0))

(define (Base10 oldbase numberlist kount)
  (if (null? numberlist)
  	0
  	(+ (* (expt oldbase kount)(car numberlist)) (Base10 oldbase (cdr numberlist) (+ kount 1)))))


(define (hexconvert hexvalue)
  (if (null? hexvalue)
  	'()
  	(cons (altHEXsubletter (car hexvalue)) (hexconvert (cdr hexvalue)))))

(define (altHEXsubletter value)
  (cond ((= 10 value) 'A)
    	((= 11 value) 'B)
    	((= 12 value) 'C)
    	((= 13 value) 'D)
    	((= 14 value) 'E)
    	((= 15 value) 'F)
    	((= 16 value) 'G)
    	((= 17 value) 'H)
    	((= 18 value) 'I)
    	((= 19 value) 'J)
    	((= 20 value) 'K)
    	((= 21 value) 'L)
    	((= 22 value) 'M)
    	((= 23 value) 'N)
    	((= 24 value) 'O)
    	((= 25 value) 'P)
    	((= 26 value) 'Q)
    	((= 27 value) 'R)
    	((= 28 value) 'S)
    	((= 29 value) 'T)
    	((= 30 value) 'U)
    	((= 31 value) 'V)
    	((= 32 value) 'W)
    	((= 33 value) 'X)
    	((= 34 value) 'Y)
    	((= 35 value) 'Z)
    	((= 36 value) '-)
    	((= 37 value) '!)
    	(else value)))

(define (numconvert numberlist)
  (if (null? numberlist)
  	'()
  	(cond ((eq? (car numberlist) 'A) (cons 10 (numconvert (cdr numberlist))))
        	((eq? (car numberlist) 'B) (cons 11 (numconvert (cdr numberlist))))
        	((eq? (car numberlist) 'C) (cons 12 (numconvert (cdr numberlist))))
        	((eq? (car numberlist) 'D) (cons 13 (numconvert (cdr numberlist))))
        	((eq? (car numberlist) 'E) (cons 14 (numconvert (cdr numberlist))))
        	((eq? (car numberlist) 'F) (cons 15 (numconvert (cdr numberlist))))
        	((eq? (car numberlist) 'G) (cons 16 (numconvert (cdr numberlist))))
        	((eq? (car numberlist) 'H) (cons 17 (numconvert (cdr numberlist))))
        	((eq? (car numberlist) 'I) (cons 18 (numconvert (cdr numberlist))))
        	((eq? (car numberlist) 'J) (cons 19 (numconvert (cdr numberlist))))
        	((eq? (car numberlist) 'K) (cons 20 (numconvert (cdr numberlist))))
        	((eq? (car numberlist) 'L) (cons 21 (numconvert (cdr numberlist))))
        	((eq? (car numberlist) 'M) (cons 22 (numconvert (cdr numberlist))))
        	((eq? (car numberlist) 'N) (cons 23 (numconvert (cdr numberlist))))
        	((eq? (car numberlist) 'O) (cons 24 (numconvert (cdr numberlist))))
        	((eq? (car numberlist) 'P) (cons 25 (numconvert (cdr numberlist))))
        	((eq? (car numberlist) 'Q) (cons 26 (numconvert (cdr numberlist))))
        	((eq? (car numberlist) 'R) (cons 27 (numconvert (cdr numberlist))))
        	((eq? (car numberlist) 'S) (cons 28 (numconvert (cdr numberlist))))
        	((eq? (car numberlist) 'T) (cons 29 (numconvert (cdr numberlist))))
        	((eq? (car numberlist) 'U) (cons 30 (numconvert (cdr numberlist))))
        	((eq? (car numberlist) 'V) (cons 31 (numconvert (cdr numberlist))))
        	((eq? (car numberlist) 'W) (cons 32 (numconvert (cdr numberlist))))
        	((eq? (car numberlist) 'X) (cons 33 (numconvert (cdr numberlist))))
        	((eq? (car numberlist) 'Y) (cons 34 (numconvert (cdr numberlist))))
        	((eq? (car numberlist) 'Z) (cons 35 (numconvert (cdr numberlist))))
        	((eq? (car numberlist) '-) (cons 36 (numconvert (cdr numberlist))))
        	((eq? (car numberlist) '!) (cons 37 (numconvert (cdr numberlist))))
        	(else (cons (car numberlist) (numconvert (cdr numberlist)))))))






(display(digitize 123))
(display(even-parity? '(0 1 1)))
(display(even-parity? '(1 1 1)))
(display(parity-bit '(0 0 0)))
(display(parity-bit '(1 1 1)))
(display(upc? tomato-soup))
(display(upc? '(1 5 1 0 0 0 1 4 8 7 2 8)))
(display(upc-check-digit '(1 5 1 0 0 0 1 4 8 7 2)))
(display(ISBN? '(0 2 6 2 5 6 0 9 9 2)))
(display(ISBN? '(0 2 6 2 5 6 0 6 9 2)))
;this is my "hexjoke" but im not really funny so I just wanted to show you that I can make words in Hexspeak.
;I hope you like "bad" jokes.
(display(hexconvert(convert 2989 16)))
(display(hexconvert(convert (quotient secret 97)37)))







