#lang typed/racket


(define-type m-formula (U m-exists-formula m-boolean-formula m-and-formula))

(struct: m-boolean-formula ([value : Boolean]) 
  #:transparent)

(struct: m-and-formula ([conjuncts : (Listof m-formula)]))

(struct: m-exists-formula ([var : Symbol]
                           [sort : Symbol]
                           [fmla : m-formula])  
  #:transparent)

; is there an analogue to type case?

;(define (test f) 
;  (cond [m-boolean-formula? 1]
;        [m-exists-formula? 2]))

; The type of a cond on failure is void. So type error if not all types accounted for
; This would not work if the function returned void by design sometimes, and void was allowed in the decl.
(: test (m-formula -> Integer))
(define (test f) 
  (cond [(m-boolean-formula? f) 1]
  [(m-exists-formula? f) 2]
  [(m-and-formula? f) 3]
  ))


(test (m-exists-formula 'x 'S (m-boolean-formula #t)))

(: test2 (Integer -> Integer))
(define (test2 f) 
  (cond [(< (random) 0.5) 1]
        [(> (random) 0.5) 2]
        [(= (random) 0.5) 3]
  ))
(test2 1)


