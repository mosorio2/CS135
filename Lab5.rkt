#lang eopl
;Part 1

(define (p-random modulus a-mult inC seed)
  (cons seed (next-p-random modulus a-mult inC seed 35)))
(define (next-p-random modulus a-mult inC seed kount)
   (if (= 0 kount)
       '() 
       (cons (modulo (+ (* a-mult seed) inC) modulus) 
             (next-p-random modulus 
                           a-mult 
                           inC 
                           (modulo (+ (* a-mult seed) inC) modulus) 
                           (- kount 1)))))

;(display(p-random 1996 1 25 0))

;Part 2

(define (convert number base)
  (cond
   [(equal? number 0) '()]
   [else (append  (convert (quotient number base) base) (list(modulo number base)))]))

(display (convert 50 3))

;Part 3

(define (base10 oldbase numberlist)
  (cond
    [(null? numberlist)0]
    [else (+ (base10 oldbase (cdr numberlist)) (* (car numberlist) (expt oldbase (- (length numberlist) 1))))]))

(display(base10 3 '(1 2 1 2)))

;Part 4

(define ME '((m i c h a e l) (o s o r i o)))

(define alloneword '((a l l o n e w o r d i n e x t r a p a r e n s)))




        
       ;Performs caesar shift on message moving each letter key spaces down alphabet
(define (encrypt message key)
  (if (null? message)
      '()
      (cons (shift key (numword (car message))) (encrypt (cdr message) key))))

;Performs shift word by word
(define (shift n word)
  (if (null? word)
      '()
      (cons (modulo (+ (car word) n) 27) (shift n (cdr word)))))

;Performs shift on a single word
(define (numlist words)
  (if (null? words)
      '()
      (cons (numword (car words)) (numlist (cdr words)))))

;Makes letter by letter substitution for numbers
(define (numword alphas)
  (if (null? alphas)
      '()
      (cond ((eq? (car alphas) 'a) (cons 1 (numword (cdr alphas))))
            ((eq? (car alphas) 'b) (cons 2 (numword (cdr alphas))))
            ((eq? (car alphas) 'c) (cons 3 (numword (cdr alphas))))
            ((eq? (car alphas) 'd) (cons 4 (numword (cdr alphas))))
            ((eq? (car alphas) 'e) (cons 5 (numword (cdr alphas))))
            ((eq? (car alphas) 'f) (cons 6 (numword (cdr alphas))))
            ((eq? (car alphas) 'g) (cons 7 (numword (cdr alphas))))
            ((eq? (car alphas) 'h) (cons 8 (numword (cdr alphas))))
            ((eq? (car alphas) 'i) (cons 9 (numword (cdr alphas))))
            ((eq? (car alphas) 'j) (cons 10 (numword (cdr alphas))))
            ((eq? (car alphas) 'k) (cons 11 (numword (cdr alphas))))
            ((eq? (car alphas) 'l) (cons 12 (numword (cdr alphas))))
            ((eq? (car alphas) 'm) (cons 13 (numword (cdr alphas))))
            ((eq? (car alphas) 'n) (cons 14 (numword (cdr alphas))))
            ((eq? (car alphas) 'o) (cons 15 (numword (cdr alphas))))
            ((eq? (car alphas) 'p) (cons 16 (numword (cdr alphas))))
            ((eq? (car alphas) 'q) (cons 17 (numword (cdr alphas))))
            ((eq? (car alphas) 'r) (cons 18 (numword (cdr alphas))))
            ((eq? (car alphas) 's) (cons 19 (numword (cdr alphas))))
            ((eq? (car alphas) 't) (cons 20 (numword (cdr alphas))))
            ((eq? (car alphas) 'u) (cons 21 (numword (cdr alphas))))
            ((eq? (car alphas) 'v) (cons 22 (numword (cdr alphas))))
            ((eq? (car alphas) 'w) (cons 23 (numword (cdr alphas))))
            ((eq? (car alphas) 'x) (cons 24 (numword (cdr alphas))))
            ((eq? (car alphas) 'y) (cons 25 (numword (cdr alphas))))
            ((eq? (car alphas) 'z) (cons 26 (numword (cdr alphas))))
            ((eq? (car alphas) '-) (cons 0 (numword (cdr alphas))))
            (else (numword (cdr alphas))))))

;Decryption routines for caesar shift

;"Unshifts" message key spaces to the left
(define (decrypt message key)
  (if (null? message)
      '()
      (cons (letterword key (car message)) (decrypt (cdr message) key))))

;Performs shift a word at a time
(define (letterword n numbers)
  (if (null? numbers)
      '()
      (cons (alpha (modulo (+ (- 27 n) (car numbers)) 27)) (letterword n (cdr numbers)))))

;Makes substitution number by number for letters
(define (alpha k)
  (cond ((= 0 k) '-)
        ((= 1 k) 'A)
        ((= 2 k) 'B)
        ((= 3 k) 'C)
        ((= 4 k) 'D)
        ((= 5 k) 'E)
        ((= 6 k) 'F)
        ((= 7 k) 'G)
        ((= 8 k) 'H)
        ((= 9 k) 'I)
        ((= 10 k) 'J)
        ((= 11 k) 'K)
        ((= 12 k) 'L)
        ((= 13 k) 'M)
        ((= 14 k) 'N)
        ((= 15 k) 'O)
        ((= 16 k) 'P)
        ((= 17 k) 'Q)
        ((= 18 k) 'R)
        ((= 19 k) 'S)
        ((= 20 k) 'T)
        ((= 21 k) 'U)
        ((= 22 k) 'V)
        ((= 23 k) 'W)
        ((= 24 k) 'X)
        ((= 25 k) 'Y)
        ((= 26 k) 'Z)))

(define (decryptall message)
  (decryptAll message 0))

(define (decryptAll message key)
  newline
  (if (= 27 key)
      (newline)
      (begin
        (newline)
        (display (decrypt message key))
        (decryptAll message (+ key 1)))))
  

;Encryption routines for the affine transformation

;Encrypts message with 2-part key a and b
(define (affine message a b)
  (if (null? message)
      '()
      (cons (aff-trans a b (numword (car message))) (affine (cdr message) a b))))

;Performs transformation word by word
(define (aff-trans a b word)
  (if (null? word)
      '()
      (cons (modulo (+ (* a (car word)) b) 27) (aff-trans a b (cdr word)))))

;Performs decryption of affine transformation cipher
(define (inv-affine message a b)
  (if (null? message)
      '()
      (cons (inv-aff-trans (a-inv a) b (car message)) (inv-affine (cdr message) a b))))

;Performs affine decryption word by word
(define (inv-aff-trans a-inv b numbers)
  (if (null? numbers)
      '()
      (cons (alpha (modulo (* (modulo (- 
                                       (car numbers) b) 27) a-inv) 27))  (inv-aff-trans a-inv b (cdr numbers)))))

;Computes multiplicative group inverse of a
(define (a-inv a) ;NOTE:  if a and 27 are not relatively prime, this will enter infinite loop
  (if (< a 2)
      a
      (find-inv a 2)))

;Actual calculation of 
(define (find-inv a k)
  (if (= 1 (modulo (* a k) 27)) ;only way out of infinite loop is if (a,27)=1
      k
      (find-inv a (+ 1 k))))

(define TAmessage2 '((15 18 11 22) (17 13 6) (11 22) (22 15 11 17 17) (12 13 22 7)))
(define typingtest '((t h e) (q u i c k) (b r o w n) (f o x) (j u m p s) (o v e r) (t h e) (l a z y) (d o g)))
(define alphabet '((- a b c d e f g h i j k l m n o p q r s t u v w x y z)))
(define allnums '(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26))
 
         
(define TAmessage1 '((6 21 22 5) (25 14 15) (22 5) (6 1 1) (18 14 5 11)))
  
(display(decrypt TAmessage1 13))
(display (inv-affine TAmessage2 20 20))





