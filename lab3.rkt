#lang eopl

(define (element? item list-of-items)
  (if (null? list-of-items)                  ;
      #f                                     
      (if (equal? item (car list-of-items))  
          #t                                 
          (element? item (cdr list-of-items)))))

(define (make-set list-of-items)
  (cond [(null? list-of-items)
         '()]                 
        [(element? (car list-of-items) (cdr list-of-items))
         (make-set (cdr list-of-items))]
        [else                           
         (cons (car list-of-items) (make-set (cdr list-of-items)))]))

(define (union setA setB)
  (make-set(append setA setB)))

(display (union '(1 2 3) '(4 5 6)))
(display (union '(1 2 3) '(1 2 3)))


(define (intersection setA setB)
  (if (null? setA) 
      '()
      (if (element? (car setA) setB)
          (make-set(cons (car setA) (intersection (cdr setA) setB)))
          (make-set(intersection (cdr setA) setB)))))

(display(intersection '(1 2 3) '(3 4 5)))
(display(intersection '(1 2 3 3) '(3 4 5)))
(display(intersection '(3 3 4 5) '(3)))

(define (subset? setA setB)
  (cond [(null? setA) #t]
        [(element? (car setA) setB)(subset? (cdr setA)  setB)]
        [else #f]))

(display(subset? '(1 2 3) '(5 4 3 2 1)))
(display(subset? '(1 2 3) '(5)))  


(define (set-equal? setA setB)
 (subset? setA setB) #t)

(display(set-equal? '() '()))
(display(set-equal? '(1 2 3) '(3 2 1)))


(define (proper-subset? setA setB)
  (cond
    [(equal? setA setB) #f]
    [else (subset? setA setB) #t]))

(display (proper-subset? '(1 2 3) '(1 2 3)))
(display(proper-subset? '() '(1 2 3)))

(define (set-difference setA setB)
  (make-set (Set-Difference setA setB)))
(define (Set-Difference setA setB)
  (cond
    [(null? setA) '() ]
    [(equal? setA setB) '()]
    [(element? (car setA) setB) (set-difference (cdr setA) setB)]
    [#t (cons (car setA) (set-difference (cdr setA) setB))]))

(display(set-difference '(1 2 3) '(2 3 4)))
(display(set-difference '(1 2 3) '(1 2 3)))

(define (sym-diff setA setB)
  (union (make-set (set-difference setA setB)) (make-set(set-difference setB setA))))

(display(sym-diff '(1 2 3) '(3 4 5))) 
(display(sym-diff '(1 2 3) '(4 5 6)))

(define (cardinality set)
  (length(make-set set)))
  
(display(cardinality '(1 2 3)))
(display(cardinality '(1 1 2 3 3)))

(define (disjoint? setA setB)
  (null? (intersection setA setB)))

(display(disjoint? '(1 2 3) '(4 5 6)))

(define (superset? setA setB)
  (set-equal? setA setB))

(display(superset? '(1 2 3) '(1 2)))

(define (insert element set)
  (make-set(cons element set)))

(display(insert 0 '(1 2 3)))
(display(insert 1 '(1 2 3)))

(define (remove element set)
  (cond
    [(null? set) '()]
    [( equal? element (car set)) remove element (cdr set)]
    [else 
     (cons (car set) (remove element (cdr set)))]))

(display (remove 2 '(1 2 3)))
(display (remove 3 '(3)))
     
        
        