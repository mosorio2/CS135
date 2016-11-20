#lang eopl

(define (relation? list-of-duples-we-hope)
  (if (equal? list-of-duples-we-hope '())
  	#t
  	(if (equal? (length (car list-of-duples-we-hope)) 2)
      	(relation? (cdr list-of-duples-we-hope))
      	#f)))

(define (id n)
  (if (equal? n 0)
  	'()
  	(append (list (append (list n) (list n))) (id (- n 1)))))

(define (reflexive? relation n)
  (subset? (id n) relation))

(define (reflexive-closure relation n)
  (if (equal? relation '())
  	(reverse (id n))
  	(if (member (car relation) (id n))
      	(reflexive-closure (cdr relation) n)
      	(union (reflexive-closure (cdr relation) n) (list (car relation))))))

(define (R-minus-1 relation)
  (if (equal? relation '())
  	'()
  	(append (list (reverse (car relation))) (R-minus-1 (cdr relation)))))

(define (symmetric? relation)
  (subset? relation (R-minus-1 relation)))

(define (symmetric-closure relation)
  (symm relation relation))

(define (symm relation start)
  (if (equal? relation '())
 	(R-minus-1 start)
 	(if (member (car relation) (R-minus-1 relation))
     	(symm (cdr relation) start)
     	(cons (car relation) (symm (cdr relation) start) ))))

(define (antisymmetric? relation)
  (if (equal? relation '())
  	#t
  	(if (member (reverse (car relation) ) relation)
      	#f
      	(antisymmetric? (cdr relation)))))

(define (related-to element relation)
  (if (equal? relation '())
  	'()
  	(if (equal? (caar relation) element)
      	(append (cdar relation) (related-to element (cdr relation)))
      	(related-to element (cdr relation)))))

(define (composite relationOuter relationInner)
  (if (equal? relationOuter '())
  	'()
  	(append (comp (caar relationOuter) (related-to (cadar relationOuter) relationInner)) (composite (cdr relationOuter) relationInner))))
       	 
       	 
(define (comp element list1)
  (if (equal? list1 '())
  	'()
  	(cons (append (list element) (list(car list1))) (comp element (cdr list1)))))


(define cycle6 '((1 2) (2 3) (3 4) (4 5) (5 6) (6 1)))
(define david '((1 3) (3 1) (2 4) (4 2) (3 5) (5 3) (4 6) (6 4) (5 1) (1 5) (6 2) (2 6)))
(define study '((1 6) (2 2) (2 3) (3 1) (4 1) (5 1) (5 6) (6 5)))
(define test '((1 2) (2 3) (2 1) (3 3) (4 5) (5 2)))
(define author-character '((Doyle Holmes) (Poe Dupin) (Dickens Dodger) (Shakespeare Macbeth) (Shakespeare Lear) (Shakespeare Hamlet) (Twain Finn) (Hugo Valjean) (Shakespeare Romeo) (Shakespeare Juliet) (Tolstoy Karenina) (Dumas Athos) (Dumas Porthos) (Dahl WillyWonka) (Dumas Aramis) (Melville Ahab) (Stevenson Silver) (Christie Marple) (Christie Poirot) (Carroll Alice)))
(define delta6 '((1 1) (2 2) (3 3) (4 4) (5 5) (6 6)))



(define (element? item list-of-items)
  (if (null? list-of-items)              	;Is our "set" empty?
  	#f                                 	;If empty, not an element!
  	(if (equal? item (car list-of-items))  ;Is our item first in list?
      	#t                             	;Yes?  Then it's an element!
      	(element? item (cdr list-of-items)))));No? Check the rest.

(define (make-set list-of-items)
  (if (null? list-of-items) ;An empty list can have no duplicates,
  	'()               	;so just return an empty list.
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

;tests
(display(relation? cycle6))
(display(relation? author-character))
(display(id 6))
(display(id 2))
(display(reflexive? study 3))
(display(reflexive? cycle6 4))
(display(reflexive-closure cycle6 4))
(display(R-minus-1 delta6))
(display(R-minus-1 cycle6))
(display(symmetric? author-character))
(display(symmetric-closure study))
(display(antisymmetric? study))
(display(antisymmetric? cycle6))
(display(related-to 1 (id 6)))
(display(related-to 'Shakespeare author-character))
(display(composite cycle6 cycle6))
(display(composite test cycle6))




