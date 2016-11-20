#lang eopl

(define firstlist (list 1 2 3))
(define secondlist (list 2 12 22 32 42))
(define shortlist (list 0))
(define thirdlist '(3 33 333 3333))
(define procs (list + - * /))
(define procedures (list '+ '- '* '/))
empty
(quote())
'()
(null? empty)
(null? '())
(null? procedures)
(equal? firstlist secondlist)
(length firstlist)
(length shortlist)
(length empty)
(reverse firstlist)
(reverse shortlist)
(reverse empty)
(append shortlist firstlist)
(append firstlist firstlist)
(car firstlist)
(car secondlist)
(car procedures)
(car shortlist)
(cdr firstlist)
(cdr secondlist)
(cdr procedures)
(cdr shortlist)

(define (last list-of-things)
  (car(reverse list-of-things)))
(display (last '(1 2 3 4 5)))

(cons 0 firstlist)
(cons '@ procedures)
(cons 0 '())
(cons firstlist secondlist)
(cons secondlist firstlist)
(cons '() '())
(cons firstlist secondlist)
(append firstlist secondlist)

(define (yoda 3-word-list)
  (cons(last 3-word-list)(reverse (cdr(reverse 3-word-list)))))
(display(yoda '(You Will Program)))

(define happy '(h a p p y))
(define birthday '(b i r t h d a y))
(define ay '(a y))

(define (pig-latin wordlist)
  (append (cdr wordlist)(cons(car wordlist)'(ay))))
(display(pig-latin happy))


(define student 
  '((IDnumber DegreeSought) (LastName FirstName) (day month year) (class/year ((major) (minor)) GPA) ((number street apt) (city state zip)) (class1 class2 ... classN)))

(define (YODA words)
  (append(cddr words) (cons(car words)(list(cadr words)))))
(display (YODA '(You Will Program)))
  
 
  
