#lang racket

(require margrave)

(provide unordered-list-equal?
         hash-contents-equal?
         m-vocabulary-equal?
         m-theory-equal?
         m-policy-equal?)

(define (unordered-list-equal? xs ys)
  (match xs
    [(list) (empty? ys)]
    [(cons x xs) (and (member? x ys)
                      (unordered-list-equal? xs (remove x ys)))]))

(define (hash-contents-equal? x y [pred? equal?])
  (define (hash-contents-contains? xs h)
    (match xs
      [(list) #t]
      [(cons x xs)
       (let ([key (car x)]
             [val (cdr x)])
         (and (hash-has-key? h key)
              (pred? (hash-ref h key) val)
              (hash-contents-contains? xs h)))]))
  (and (hash-contents-contains? (hash->list x) y)
       (hash-contents-contains? (hash->list y) x)))

(define (hash-contents-eq? x y)
  (hash-contents-eq? x y eq?))

(define (hash-contents-eqv? x y)
  (hash-contents-eqv? x y eqv?))

;; margrave util

(define (m-vocabulary-equal? lhs rhs)
  (and (equal? (m-vocabulary-name lhs) (m-vocabulary-name rhs))
       (hash-contents-equal? (m-vocabulary-types lhs)
                             (m-vocabulary-types rhs))
       (hash-contents-equal? (m-vocabulary-predicates lhs)
                             (m-vocabulary-predicates rhs))
       (hash-contents-equal? (m-vocabulary-constants lhs)
                             (m-vocabulary-constants rhs))
       (hash-contents-equal? (m-vocabulary-functions lhs)
                             (m-vocabulary-functions rhs))))

(define (m-theory-equal? lhs rhs)
  (and (equal? (m-theory-name lhs) (m-theory-name rhs))
       (equal? (m-theory-path lhs) (m-theory-path rhs))
       (m-vocabulary-equal? (m-theory-vocab lhs) (m-theory-vocab rhs))
       (unordered-list-equal? (m-theory-axioms lhs) (m-theory-axioms rhs))))

(define (m-policy-equal? lhs rhs)
  (and (equal? (m-policy-id lhs) (m-policy-id rhs))
       (m-theory-equal? (m-policy-theory lhs) (m-policy-theory rhs))
       (hash-contents-equal? (m-policy-vardecs lhs) (m-policy-vardecs rhs))
       (unordered-list-equal? (m-policy-rule-names lhs) (m-policy-rule-names rhs))
       (equal? (m-policy-rcomb lhs) (m-policy-rcomb rhs))
       (equal? (m-policy-target lhs) (m-policy-target rhs))
       (hash-contents-equal? (m-policy-idbs lhs) (m-policy-idbs rhs))))