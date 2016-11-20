#lang eopl

;part 0

(define sun-tsu '((2 3) (3 5) (2 7))) 
(define tsu-sun '((1 3) (2 5) (6 7)))  
(define sum-tsu '((3 3) (5 5) (8 7)))  
(define prod-tsu '((2 3) (6 5) (12 7)))
(define primes7 '(2 3 5 7 11 13 17))

(define (duple-list? list-of-duples)
  (if (null? list-of-duples)
      #t
      (if (not (and (pair? (car list-of-duples)) (= 2 (length (car list-of-duples)))))
          #f
          (duple-list? (cdr list-of-duples)))))

(define (pair-wise-rel-prime? list-of-duples)
  (rel-prime? (cdr (list-of-cadars list-of-duples)) (list-of-cadars list-of-duples)))

(define (rel-prime? riplist fulllist)  ;checks cadars in duple-list--need for m's in our simultaneous congruences
  (if (null? (cdr fulllist))
      #t
      (if (null? riplist)
          (rel-prime? (cddr fulllist) (cdr fulllist))
          (if (not (= 1 (gcd (car fulllist) (car riplist))))
              #f
              (rel-prime? (cdr riplist) fulllist)))))

(define (crt list-of-duples)
    (if (and (duple-list? list-of-duples) (pair-wise-rel-prime? list-of-duples))
        (triple-multiply (list-of-caars list-of-duples) 
                         (M-list list-of-duples) 
                         (yk-list list-of-duples (apply * (list-of-cadars list-of-duples))) 
                         (apply * (list-of-cadars list-of-duples)))
        (display "Bad duple list")))

(define (triple-multiply ak-list mk-list yk-list M)
  (modulo (T-mult ak-list mk-list yk-list) M))
;T-mult just multiplies the three cars of our three lists and adds the product to the running total

(define (T-mult ak-list mk-list yk-list)
  (if (null? ak-list)
      0
      (+ (* (car ak-list) (car mk-list) (car yk-list)) (T-mult (cdr ak-list) (cdr mk-list) (cdr yk-list)))))

;Six lines of Scheme to do this enormous number crunching, and one of those lines is just "0".


;This will get us a list of the (ak) in correct order
(define (list-of-caars list-of-duples)
  (if (null? list-of-duples)
      '()
      (cons (caar list-of-duples) (list-of-caars (cdr list-of-duples)))))

;This will get us a list of the (mk) moduli in correct order
(define (list-of-cadars list-of-duples)
  (if (null? list-of-duples)
      '()
      (cons (cadar list-of-duples) (list-of-cadars (cdr list-of-duples)))))

;This gives us a list of Mk we need (in correct order) for simple solution-execution
(define (M-list list-of-duples)
  (if (duple-list? list-of-duples)
      (m-list list-of-duples (apply * (list-of-cadars list-of-duples)))
      '()    ;just added this to make it run.  I think old version did not require else statement.
      ))

;Helper for M-list
(define (m-list list-of-duples m)
  (if (null? list-of-duples)
      '()
      (cons (quotient m (cadar list-of-duples)) (m-list (cdr list-of-duples) m))))

;Here are the helpers that get us the yk's, the inverses of the Mk's modulo the mk's.
;It's an implementation of the Extended Euclidean Algorithm; you should consult Rosen pg.273 for the details.

;This is the inverses of the ak mod mk
(define (yk-list list-of-duples M)
  (if (null? list-of-duples)
      '()
      (cons (ext-euclid (quotient M (cadar list-of-duples)) (cadar list-of-duples))
            (yk-list (cdr list-of-duples) M))))

(define (ext-euclid a b)
  (Ext-euclid a b '(1 0) '(0 1) (reverse (euclid-quotients a b))))

(define (Ext-euclid a b s-list t-list q-list)
  (if (= (length s-list)  (+ (length q-list) 1) ) ;not sure if q-list starts counting at 0 or 1.
      (modulo (+ (car t-list) b) b)
;      (list (car s-list) 
;            (car t-list) 
;            (list (car s-list) b (car t-list) a (+ (* (car s-list) b) (* (car t-list) a)) s-list t-list q-list))
      (Ext-euclid a b (extend s-list q-list) (extend t-list q-list) q-list)))

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

(define (y-s list-of-duples)
  (if (null? list-of-duples)
      '()
      (cons (* (caar list-of-duples) (cadar list-of-duples)) (y-s (cdr list-of-duples)))))

;I need the Extended Euclidean Algorithm
(define (a-inverse-mod-m a m yk- modulus)
  (if (= (modulo a m) 0)
      yk-;(- modulus yk-)
      (a-inverse-mod-m m (modulo a m) (quotient a m) modulus)));Euclidean algorithm hides in here in first two parameters


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

(define (duple-zipper list1 list2)
  (if (null? list1)
      '()
      (cons (list (car list1) (car list2)) (duple-zipper (cdr list1) (cdr list2)))))


;(numword list-of-chars) turns list of chars into list of ints
;(base27 list-of-ints) turns list of ints into base-27 number
;(crt-encode number list-of-rel-prime-ints) turns number into a system of congruences
;(crt system-of-congruences) turns system of congruences into a number (integer)


(define (rotate list-of-atoms atom)
  (if (null? list-of-atoms)
      (list atom)
      (cons (car list-of-atoms) (rotate (cdr list-of-atoms) atom))))
  
(define primes300 '(2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61 67 71 73 79 83 89 97 101 103 107 109 113
127 131 137 139 149 151 157 163 167 173 179 181 191 193 197 199 211 223 227 229 233 239 241 251 257 263 269 271 277 281 283 293 307 311 313 317 331 337 347 349 353 359 367 373 379 383 389 397 401 409 419 421 431 433 439 443 449 457 461 463 467 479 487 491 499 503 509 521 523 541 547 557 563 569 571 577 587 593 599 601 607 613 617 619 631 641 643 647 653 659 661 673 677 683 691 701 709 719 727 733 739 743 751 757 761 769 773 787 797 809 811 821 823 827 829 839 853 857 859 863 877 881 883 887 907 911 919 929 937 941 947 953 967 971 977 983 991 997 1009 1013 1019 1021 1031 1033 1039 1049 1051 1061 1063 1069 1087 1091 1093 1097 1103 1109 1117 1123 1129 1151 1153 1163 1171 1181 1187 1193 1201 1213 1217 1223 1229 1231 1237 1249 1259 1277 1279 1283 1289 1291 1297 1301 1303 1307 1319 1321 1327 1361 1367 1373 1381 1399 1409 1423 1427 1429 1433 1439 1447 1451 1453 1459 1471 1481 1483 1487 1489 1493 1499 1511 1523 1531 1543 1549 1553 1559 1567 1571 1579 1583 1597 1601 1607 1609 1613 1619 1621 1627 1637 1657 1663 1667 1669 1693 1697 1699 1709 1721 1723 1733 1741 1747 1753 1759 1777 1783 1787 1789 1801 1811 1823 1831 1847 1861 1867 1871 1873 1877 1879 1889 1901 1907 1913 1931 1933 1949 1951 1973 1979 1987))
            





(define (zipper ak-list mk-list)
  (if (null? ak-list)
      '()
      (cons (list (car ak-list) (car mk-list)) (zipper (cdr ak-list) (cdr mk-list)))))



;part 1

(define minus1 '((-1 2) (-1 3) (-1 5) (-1 7) (-1 11) (-1 13) (-1 17)))
(define minus2 '((-2 2) (-2 3) (-2 5) (-2 7) (-2 11) (-2 13) (-2 17)))
(define minus3 '((-3 2) (-3 3) (-3 5) (-3 7) (-3 11) (-3 13) (-3 17)))

;part 2

(define Blake0 '(t y g e r - t y g e r))

(define (base27 list-of-ints)
  (if (null? list-of-ints)
      0
      (+ (* (car list-of-ints) (expt 27 (- (length list-of-ints) 1))) (base27 (cdr list-of-ints)))))

(define (crt-encode integer moduli) 
  (if (null? moduli)
      '()
      (cons (list (modulo integer (car moduli)) (car moduli)) (crt-encode integer (cdr moduli)))))

(define (decode-crt base-27-int)
  (reverse (Decode-CRT base-27-int)))

(define (Decode-CRT base-27-int)
  (if (= 0 base-27-int)
      '()
      (cons (alpha (modulo base-27-int 27)) (Decode-CRT (quotient base-27-int 27)))))

(define Blake1 '(t y g e r - t y g e r - b u r n i n g ))
(define BlakeVerse1 '(t y g e r - t y g e r - b u r n i n g - b r i g h t - i n - t h e - f o r e s t s - o f - t h e - n i g h t - w h a t - i m m o r t a l - h a n d - o r - e y e - c o u l d - f r a m e - t h y - f e a r f u l - s y m m e t r y))


;Part 3

(define primes135 '(2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61 67 71 73 79 83 89 97 101 103 107 109 113 127 131 137 139 149 151 157 163 167 173 179 181 191 193 197 199 211 223 227 229 233 239 241 251 257 263 269 271 277 281 283 293 307 311 313 317 331 337 347 349 353 359 367 373 379 383 389 397 401 409 419 421 431 433 439 443 449 457 461 463 467 479 487 491 499 503 509 521 523 541 547 557 563 569 571 577 587 593 599 601 607 613 617 619 631 641 643 647 653 659 661 673 677 683 691 701 709 719 727 733 739 743 751 757 761))
(define keyME '(2 3 5 7 13 17 19 23 29 31 37 43 47 53 59 61 67 71 73 79 83 89 97 101 103 107 109 113 127 131 137 139 149 151 157 163 167 173 179 181 191 193 197 199 211 223 227 229 233 239 241 251 257 263 269 271 277 281 283 293 307 311 313 317 331 337 347 349 353 359 367 373 379 383 389 397 401 409 419 421 431 433 439 443 449 457 461 463 467 479 487 491 499 503 509 521 523 541 547 557 563 569 571 577 587 593 599 601 607 613 617 619 631 641 643 647 653 659 661 673 677 683 691 701 709 719 727 733 739 743 751 757 761))
(define mymessage '(i - l o v e - h e n r y))

;Part 4

(crt-encode (base27 (numword mymessage)) keyME)
(list-of-caars (crt-encode (base27 (numword mymessage)) keyME))
(decode-crt (crt (zipper (list-of-caars (crt-encode (base27 (numword mymessage)) keyME)) keyME)))
(define KEY '(2 3 5 7 13 17 19 23 29 31 37 43 47 53 59 61 67 71 73 79 83 89 97 101 103 107 109 113 127 131 137 139 149 151 157 163 167 173 179 181 191 193 197 199 211 223 227 229 233 239 241 251 257 263 269 271 277 281 283 293 307 311 313 317 331 337 347 349 353 359 367 373 379 383 389 397 401 409 419 421 431 433 439 443 449 457 461 463 467 479 487 491 499 503 509 521 523 541 547 557 563 569 571 577 587 593 599 601 607 613 617 619 631 641 643 647 653 659 661 673 677 683 691 701 709 719 727 733 739 743 751 757 761))
;41 and 47

(define code '(0
 2
 3
 6
 6
 10
 15
 5
 6
 9
 19
 13
 3
 7
 44
 0
 16
 62
 32
 66
 13
 25
 34
 5
 3
 14
 16
 105
 4
 6
 2
 105
 12
 100
 125
 84
 31
 74
 108
 52
 13
 123
 116
 162
 147
 86
 166
 204
 60
 45
 119
 15
 171
 53
 250
 52
 83
 155
 161
 26
 255
 144
 80
 52
 254
 124
 139
 36
 337
 178
 26
 147
 86
 286
 81
 214
 1
 157
 36
 270
 101
 401
 128
 172
 335
 174
 48
 231
 242
 214
 395
 376
 97
 435
 199
 381
 213
 279
 217
 164
 114
 33
 479
 570
 38
 482
 156
 27
 403
 205
 174 67 598 15 256 449 27 87 121 54 93 407 286 450 648 484 271 328 154 569 519 201 419))








