#lang eopl

;Definitions

(define (element? item list-of-items)
  (if (null? list-of-items)                  ;Is our "set" empty?
      #f                                     ;If empty, not an element!
      (if (equal? item (car list-of-items))  ;Is our item first in list?
          #t                                 ;Yes?  Then it's an element!
          (element? item (cdr list-of-items)))));No? Check the rest.

(define (make-set list-of-items)
  (if (null? list-of-items) ;An empty list can have no duplicates,
      '()                   ;so just return an empty list.
      (if (element? (car list-of-items) (cdr list-of-items))
          (make-set (cdr list-of-items))
          (cons (car list-of-items) (make-set (cdr list-of-items))))))

(define (union setA setB)
  (make-set (append setA setB))) 

(define (intersection setA setB)
  (make-set (Intersection (make-set setA) (make-set setB))))

(define (Intersection setA setB)
  (if (null? setA) 
      '()
      (if (element? (car setA) setB)
          (cons (car setA) (intersection (cdr setA) setB))
          (intersection (cdr setA) setB))))

(define (subset? setA setB)
  (if (null? setA)
      #t
      (if (element? (car setA) setB)
          (subset? (cdr setA)  setB)
          #f)))

(define (set-equal? setA setB)
   (and (subset? setA setB) (subset? setB setA)))

(define (proper-subset? setA setB)
  (and (subset? setA setB) (not (set-equal? setA setB))))

(define (set-difference setA setB)
  (make-set (Set-Difference setA setB)))

(define (Set-Difference setA setB)
  (if (null? setA)
      '()
      (if (element? (car setA) setB)
          (Set-Difference (cdr setA) setB)
          (cons (car setA) (Set-Difference (cdr setA) setB)))))

(define (sym-diff setA setB)
  (union (set-difference setA setB) (set-difference setB setA)))

(define (cardinality set)
  (length (make-set set)))

(define (disjoint? setA setB)
  (null? (intersection setA setB)))

(define (superset? setA setB)
  (subset? setB setA))

(define (insert element set)
  (make-set (cons element set)))

(define (remove element set)
  (set-difference set (list element)))

;part 0

(define Hasse72 '((6 12) (6 18) (9 18) (8 24) (12 24) (12 36) (18 36) (24 72) (36 72) (1 2) (1 3) (2 4) (2 6) (3 6) (3 9) (4 8) (4 12)))
(define RevHasse72 (reverse Hasse72))
(define HasseX '((LowerLeft Middle) (Middle UpperRight) (LowerRight Middle) (Middle UpperLeft)))

;part 1

(define (firstlist duples)
  (if (null? duples)
      '()
      (cons (caar duples) (firstlist (cdr duples)))))

(define (newlist duples)
  (if (null? duples)
      '()
      (cons (reverse (car duples)) (newlist (cdr duples)))))

(define (minimal-elements duples)
  (set-difference (firstlist duples) (firstlist (newlist duples))))


;part2

(define (rinse element duples)
  (if (null? duples)
      '()
      (if (equal? element (caar duples))
          (rinse element (cdr duples))
          (cons (car duples) (rinse element (cdr duples))))))

;part 3

(define (topological-sort duples)
  (if (null? duples)
      '()
      (cons (car (minimal-elements duples)) (topological-sort(rinse (car (minimal-elements duples)) duples)))))


;part 4

(define (cdarlist duples)
  (if (null? duples)
      '()
      (append (cdar duples) (cdarlist (cdr duples)))))

(define (maximal-elements duples)
  (set-difference (cdarlist duples) (cdarlist (newlist duples)))) 

(define (topo-sort duples)
  (append (topological-sort duples) (maximal-elements duples)))
    
 
;tests

(display(minimal-elements Hasse72))
(display( rinse 1 Hasse72))
(display(topological-sort HasseX))
(display(maximal-elements Hasse72))
(display(topo-sort RevHasse72))
    
                         
    














