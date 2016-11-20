#lang eopl


;Given

(define (base27 list-of-ints)
  (if (null? list-of-ints)
      0
      (+ (* (car list-of-ints) (expt 27 (- (length list-of-ints) 1))) (base27 (cdr list-of-ints)))))

(define (integers n)
  (if (= 0 n)
      '()
      (cons n (integers (- n 1)))))

(define (sievefast n)
  (nextprimefast n (cdr (reverse (integers n)))))


(define (nextprimefast n sievelist)
  (if (or (null? sievelist) (null? (cdr sievelist)))
      sievelist
      (if (> (car sievelist) (sqrt n))
          sievelist
          (cons (car sievelist) 
                (nextprimefast n (crossout (car sievelist) (cdr sievelist)))))))

(define (crossout prime sievelist)
  (if (null? sievelist)
      '()
      (if (= 0 (modulo (car sievelist) prime))
          (crossout prime (cdr sievelist))
          (cons (car sievelist) (crossout prime (cdr sievelist))))))

(define (prime? n)
  (if (= 2 n)
      #t
      (if (even? n)
          #f
          (ISPRIME? 3 n))))

(define (ISPRIME? d n)
  (if (> d (sqrt n))
      #t
      (if (= 0 (modulo n d))
          #f
          (ISPRIME? (+ 2 d) n))))

(define (find-next-prime n howManyMore)
     (if (= howManyMore 0) '()
         (if (prime? n) (cons n (find-next-prime (+ n 2) (- howManyMore 1))) (find-next-prime (+ n 2) howManyMore))))







;(define inv (inverse exp mod))


(define (convert number base)
  (reverse (Convert number base)))

(define (Convert number base)
  (if (< number base)
      (list number)  
      (cons (modulo number base) (Convert (quotient number base) base))))



(define (bitstring number)
  (Bitstring number))

(define (Bitstring number)
  (if (< number 2)   
      (list number)  
      (cons (modulo number 2) (Bitstring (quotient number 2)))))


(define (modExp b n m)
  (B^NmodM (bitstring n) m 1 (modulo b m)))

(define (B^NmodM nbits m x power)
  (if (= 0 (length nbits))
      x
      (if (= 1 (car nbits))
          (B^NmodM (cdr nbits) m (modulo (* x power) m) (modulo (* power power) m))
          (B^NmodM (cdr nbits) m x (modulo (* power power) m)))))


(define (decode base-27-int)
  (reverse (Decode base-27-int)))

(define (Decode base-27-int)
  (if (= 0 base-27-int)
      '()
      (cons (alpha (modulo base-27-int 27)) (Decode (quotient base-27-int 27)))))

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

;part 0
(define (fast-mod-exp b n m)
  (B^NmodM (bitstring n) m 1 (modulo b m)))


;part 1

(define p1 1111111111111111111)
(define q1 11111111111111111111111)
(define p1q1 (* p1 q1))
(define repUnitRSAmod (* (- p1 1) (- q1 1)))
(define eRepUnit 10103)


(define (inverse exponent modulus)
  (Inverse exponent modulus '(1 0) '(0 1) (reverse (euclid-quotients exponent modulus))))

(define (Inverse exponent modulus s-list t-list q-list)
  (if (= (length s-list)  (+ (length q-list) 1) ) ;not sure if q-list starts counting at 0 or 1.
      (modulo (+ (car t-list) modulus) modulus)
      (Inverse exponent modulus (extend s-list q-list) (extend t-list q-list) q-list)))

(define (extend s-or-t-list q-list)
  (cons (- (cadr s-or-t-list) (* (right-q s-or-t-list q-list) (car s-or-t-list))) s-or-t-list))

(define (right-q shortlist longlist)
  (if (= (- (length shortlist) 1) (length longlist))
      (car longlist)
      (right-q shortlist (cdr longlist))))

(define (euclid-quotients a b)
  (if (= 0 (modulo a b))
      (list (quotient a b) )
      (cons (quotient a b) (euclid-quotients b (modulo a b)))))


(define dRepUnit (inverse eRepUnit repUnitRSAmod))

(define repUnitMessage '(i t - i s - s t i l l - t o o - h o t))
(define repUnitMessageBIG '(t h i s - m e s s a g e - i s - j u s t - t o o - l o n g))

;part 2

(define (prime-find seed)
  (if (prime? seed)
  	seed
  	(prime-find (+ 2 seed))))

(define my-seed 100000512000001)
(find-next-prime my-seed 2)

(define p 100000512000073)
(define q 100000512000083)

(define pq (* p q))   	
(define n pq)         	
(define RSAmod (* (- p 1) (- q 1)))
(define myMod (* p q))
(define e 413)
(define mine (inverse e RSAmod))
(define d (inverse e RSAmod))
(/ (log pq) (log 27))

;part 3

(define MEpublicKeyModulus 27606985387162255149739023449107931668458716142620601169954803000803329)
(define MEpublicKeyExponent (- (expt 2 41) 1))
(define MEpublicKeyPayload (/ (log MEpublicKeyModulus) (log 27)))

(define ME-mod MEpublicKeyModulus)
(define ME-e   MEpublicKeyExponent)
(define hisInv (inverse ME-e ME-mod))

;10000102400277744079872006059 pq
;413 e
;17528732019514140133393889504194386739393670831643534809255463970956036 mod
;(decode(modExp theNumber mine myMod))
;5954626950066296245383557843

;part 4

(display(decode (modExp 11999813080057331612519560429584011957563542927117403565040166057720951 ME-e ME-mod)))
(define MEsignedMessage 11843052787342355840107561998278661057562261921828356217409147849087298)


















