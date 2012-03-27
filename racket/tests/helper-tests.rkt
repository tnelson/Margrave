#lang racket

(require rackunit
         "../margrave.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; partition*
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Need to call hash-copy because partition returns a MUTABLE hash table, but '#hash( ... ) 
; is immutable. Mut != Immut.
(check-true (equal? 
             (partition* even? 
                         '())
             (hash-copy '#hash())))

(check-true (equal? 
             (partition* (lambda (x) (remainder x 3))
                         '(1 2 3) 
                         #:init-keys (list 17))
             (hash-copy '#hash( (0 . (3)) (1 . (1)) (2 . (2)) (17 . () )))))

(check-true (equal?
             (partition* (lambda (x) (and (even? x) (remainder x 3)))
                         '(1 2 3 4 5 6 7 8 9 10))    
            (hash-copy '#hash((0 . (6)) (2 . (8 2)) (1 . (10 4))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Lexically valid symbols for functions, predicates, etc.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(check-true (valid-function? 'f0))
(check-false (valid-function? 100))  

(check-true (valid-constant? ''cONSTANT))
(check-false (valid-constant? 'c))
(check-false (valid-constant? '100))  
(check-false (valid-constant? ''100))  

(check-true (valid-predicate? 'myPred))
(check-false (valid-predicate? 100)) 

(check-true (valid-sort? 'A))
(check-false (valid-sort? 'a))  
(check-false (valid-sort? 'constant))  
(check-false (valid-sort? 100))  

(check-true (valid-variable? 'myVar0))
(check-false (valid-variable? 'A))  
(check-false (valid-variable? 100))  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Others
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(check-true (equal? (hash-union/overlap (hash "A" 2 "B" 3 "C" 4) (hash "B" 3) "oops")
                    (hash-copy (hash "A" 2 "B" 3 "C" 4))))
(check-true (equal? (hash-union/overlap (hash "A" 2 "B" 3 "C" 4) (hash ) "oops")
                    (hash-copy (hash "A" 2 "B" 3 "C" 4))))
(check-true (equal? (hash-union/overlap (hash "A" 2 "B" 3 "C" 4) (hash "B" 3 "D" 5) "oops")
                    (hash-copy (hash "A" 2 "B" 3 "C" 4 "D" 5))))