#lang racket

(require rackunit
         ; Relative paths to avoid refreshing the Margrave collection every time something changes in dev.
         "../margrave.rkt"
         "../margrave-xml.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Conversion of vocabularies, theories, and policies to XML
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(check equal-unordered? (m-vocabulary->xexprs (Vocab myvoc (Types X Y (Z > A B C)) (Constants ('c A) ('c2 X)) (Functions (f1 A B) (f2 X Y Z)) (Predicates (r X Y))))
               '((MARGRAVE-COMMAND ((type "ADD")) (VOCAB-IDENTIFIER ((vname "myvoc"))) (SORT ((name "A"))))
                 (MARGRAVE-COMMAND ((type "ADD")) (VOCAB-IDENTIFIER ((vname "myvoc"))) (SORT ((name "C"))))
                 (MARGRAVE-COMMAND ((type "ADD")) (VOCAB-IDENTIFIER ((vname "myvoc"))) (SORT ((name "B"))))
                 (MARGRAVE-COMMAND ((type "ADD")) (VOCAB-IDENTIFIER ((vname "myvoc"))) (SORT-WITH-CHILDREN ((name "Z")) (SORT ((name "A"))) (SORT ((name "B"))) (SORT ((name "C")))))
                 (MARGRAVE-COMMAND ((type "ADD")) (VOCAB-IDENTIFIER ((vname "myvoc"))) (SORT ((name "X"))))
                 (MARGRAVE-COMMAND ((type "ADD")) (VOCAB-IDENTIFIER ((vname "myvoc"))) (SORT ((name "Y"))))
                 (MARGRAVE-COMMAND ((type "ADD")) (VOCAB-IDENTIFIER ((vname "myvoc"))) (PREDICATE ((name "r"))) (RELATIONS (RELATION ((name "X"))) (RELATION ((name "Y")))))
                 (MARGRAVE-COMMAND ((type "ADD")) (VOCAB-IDENTIFIER ((vname "myvoc"))) (CONSTANT ((name "c") (type "A"))))
                 (MARGRAVE-COMMAND ((type "ADD")) (VOCAB-IDENTIFIER ((vname "myvoc"))) (CONSTANT ((name "c2") (type "X"))))
                 (MARGRAVE-COMMAND ((type "ADD")) (VOCAB-IDENTIFIER ((vname "myvoc"))) (FUNCTION ((name "f1")) (RELATIONS (RELATION ((name "A"))) (RELATION ((name "B"))))))
                 (MARGRAVE-COMMAND ((type "ADD")) (VOCAB-IDENTIFIER ((vname "myvoc"))) (FUNCTION ((name "f2")) (RELATIONS (RELATION ((name "X"))) (RELATION ((name "Y"))) (RELATION ((name "Z"))))))))               

(check equal-unordered? 
       (m-theory->xexprs (Theory mythy (Vocab myvoc (Types X Y (Z > A B C)) (Constants ('c A) ('c2 X)) (Functions (f1 A B) (f2 X Y Z)) (Predicates (r X Y))) (Axioms (partial-function f1))))
       '((MARGRAVE-COMMAND ((type "ADD")) (VOCAB-IDENTIFIER ((vname "myvoc"))) (SORT ((name "A"))))
         (MARGRAVE-COMMAND ((type "ADD")) (VOCAB-IDENTIFIER ((vname "myvoc"))) (SORT ((name "C"))))
         (MARGRAVE-COMMAND ((type "ADD")) (VOCAB-IDENTIFIER ((vname "myvoc"))) (SORT ((name "B"))))
         (MARGRAVE-COMMAND ((type "ADD")) (VOCAB-IDENTIFIER ((vname "myvoc"))) (SORT-WITH-CHILDREN ((name "Z")) (SORT ((name "A"))) (SORT ((name "B"))) (SORT ((name "C")))))
         (MARGRAVE-COMMAND ((type "ADD")) (VOCAB-IDENTIFIER ((vname "myvoc"))) (SORT ((name "X"))))
         (MARGRAVE-COMMAND ((type "ADD")) (VOCAB-IDENTIFIER ((vname "myvoc"))) (SORT ((name "Y"))))
         (MARGRAVE-COMMAND ((type "ADD")) (VOCAB-IDENTIFIER ((vname "myvoc"))) (PREDICATE ((name "r"))) (RELATIONS (RELATION ((name "X"))) (RELATION ((name "Y")))))
         (MARGRAVE-COMMAND ((type "ADD")) (VOCAB-IDENTIFIER ((vname "myvoc"))) (CONSTANT ((name "c") (type "A"))))
         (MARGRAVE-COMMAND ((type "ADD")) (VOCAB-IDENTIFIER ((vname "myvoc"))) (CONSTANT ((name "c2") (type "X"))))
         (MARGRAVE-COMMAND ((type "ADD")) (VOCAB-IDENTIFIER ((vname "myvoc"))) (FUNCTION ((name "f1")) (RELATIONS (RELATION ((name "A"))) (RELATION ((name "B"))))))
         (MARGRAVE-COMMAND ((type "ADD")) (VOCAB-IDENTIFIER ((vname "myvoc"))) (FUNCTION ((name "f2")) (RELATIONS (RELATION ((name "X"))) (RELATION ((name "Y"))) (RELATION ((name "Z"))))))
         (MARGRAVE-COMMAND ((type "ADD")) (VOCAB-IDENTIFIER ((vname "mythy"))) (CONSTRAINT ((type "PARTIAL-FUNCTION")) (RELATIONS (RELATION ((name "f1"))))))))


(check equal-unordered? (m-policy->xexprs ((Policy uses Conference
                                                   (Variables 
                                                    (s Subject)
                                                    (a Action)
                                                    (r Resource))
                                                   (Rules 
                                                    (PaperNoConflict = (Permit s a r) :- (and (not (conflicted s r)) (readPaper a) (paper r)))
                                                    (PaperAssigned = (Permit s a r) :- (and (assigned s r) (readPaper a) (paper r)));
                                                    (PaperConflict = (Deny s a r) :- (and (conflicted s r) (readPaper a) (paper r)))))
                                           "../examples/conference/conference.p" "MYPOLICYID" #'foo))
       
       '((MARGRAVE-COMMAND ((type "CREATE POLICY LEAF")) (POLICY-IDENTIFIER ((pname "MYPOLICYID"))) (VOCAB-IDENTIFIER ((vname "Conference"))))
         (MARGRAVE-COMMAND ((type "ADD")) (POLICY-IDENTIFIER ((pname "MYPOLICYID"))) (VARIABLE-DECLARATION ((sort "Action") (varname "a"))))
         (MARGRAVE-COMMAND ((type "ADD")) (POLICY-IDENTIFIER ((pname "MYPOLICYID"))) (VARIABLE-DECLARATION ((sort "Resource") (varname "r"))))
         (MARGRAVE-COMMAND ((type "ADD")) (POLICY-IDENTIFIER ((pname "MYPOLICYID"))) (VARIABLE-DECLARATION ((sort "Subject") (varname "s"))))
         (MARGRAVE-COMMAND
          ((type "ADD"))
          (POLICY-IDENTIFIER ((pname "MYPOLICYID")))
          (RULE
           ((name "PaperNoConflict"))
           (DECISION-TYPE ((type "Permit")) (ID ((id "s"))) (ID ((id "a"))) (ID ((id "r"))))
           (TARGET
            (AND
             (NOT (ATOMIC-FORMULA (RELATION-NAME (ID ((id "conflicted")))) (TERMS (VARIABLE-TERM ((id "s"))) (VARIABLE-TERM ((id "r"))))))
             (AND
              (ATOMIC-FORMULA (RELATION-NAME (ID ((id "readPaper")))) (TERMS (VARIABLE-TERM ((id "a")))))
              (ATOMIC-FORMULA (RELATION-NAME (ID ((id "paper")))) (TERMS (VARIABLE-TERM ((id "r"))))))))))
         (MARGRAVE-COMMAND
          ((type "ADD"))
          (POLICY-IDENTIFIER ((pname "MYPOLICYID")))
          (RULE
           ((name "PaperAssigned"))
           (DECISION-TYPE ((type "Permit")) (ID ((id "s"))) (ID ((id "a"))) (ID ((id "r"))))
           (TARGET
            (AND
             (ATOMIC-FORMULA (RELATION-NAME (ID ((id "assigned")))) (TERMS (VARIABLE-TERM ((id "s"))) (VARIABLE-TERM ((id "r")))))
             (AND
              (ATOMIC-FORMULA (RELATION-NAME (ID ((id "readPaper")))) (TERMS (VARIABLE-TERM ((id "a")))))
              (ATOMIC-FORMULA (RELATION-NAME (ID ((id "paper")))) (TERMS (VARIABLE-TERM ((id "r"))))))))))
         (MARGRAVE-COMMAND
          ((type "ADD"))
          (POLICY-IDENTIFIER ((pname "MYPOLICYID")))
          (RULE
           ((name "PaperConflict"))
           (DECISION-TYPE ((type "Deny")) (ID ((id "s"))) (ID ((id "a"))) (ID ((id "r"))))
           (TARGET
            (AND
             (ATOMIC-FORMULA (RELATION-NAME (ID ((id "conflicted")))) (TERMS (VARIABLE-TERM ((id "s"))) (VARIABLE-TERM ((id "r")))))
             (AND
              (ATOMIC-FORMULA (RELATION-NAME (ID ((id "readPaper")))) (TERMS (VARIABLE-TERM ((id "a")))))
              (ATOMIC-FORMULA (RELATION-NAME (ID ((id "paper")))) (TERMS (VARIABLE-TERM ((id "r"))))))))))         
         (MARGRAVE-COMMAND ((type "SET RCOMBINE FOR POLICY")) (POLICY-IDENTIFIER ((pname "MYPOLICYID"))) (COMB-LIST))
         (MARGRAVE-COMMAND ((type "PREPARE")) (POLICY-IDENTIFIER ((pname "MYPOLICYID"))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Vocabulary combination and gathering policy references in fmla
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(check-true (equal? (combine-vocabs (m-vocabulary "v1" (hash) (hash) (hash) (hash)) 
                                    (m-vocabulary "v2" (hash) (hash) (hash) (hash)))
                    (m-vocabulary "v1+v2" (make-hash) (make-hash) (make-hash) (make-hash))))
(check-true (equal? (combine-vocabs (m-vocabulary "v1" (hash "A" (m-type "A" empty)) (hash) (hash) (hash)) 
                                    (m-vocabulary "v2" (hash "A" (m-type "A" empty)) (hash) (hash) (hash)))
                    (m-vocabulary "v1+v2" (make-hash `(("A" ,@(m-type "A" '())))) (make-hash) (make-hash) (make-hash))))
(check-true (equal? (combine-vocabs (m-vocabulary "v1" (hash "A" (m-type "A" '("B"))) (hash) (hash) (hash)) 
                                    (m-vocabulary "v2" (hash "A" (m-type "A" '("C"))) (hash) (hash) (hash)))
                    (m-vocabulary "v1+v2" (make-hash `(("A" ,@(m-type "A" '("B" "C"))))) (make-hash) (make-hash) (make-hash))))

(check-true (set-empty? (gather-policy-references '(and (= x y) (r x y z)))))
(check-false (set-empty? (gather-policy-references '(or false ((MyPolicy permit) x y z)))))
(check-true (equal? 2 (set-count (gather-policy-references '(or ((MyOtherPolicy deny) z y x) ((MyPolicy permit) x y z))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; m-type and m-predicate translation
; m-axiom translation
; m-formula? and m-term?
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(check-true (equal?
             (m-type->cmd "vocabname" (m-type "A" (list "B" "C")))                    
             '(MARGRAVE-COMMAND ((type "ADD")) (VOCAB-IDENTIFIER ((vname "vocabname"))) 
                                (SORT-WITH-CHILDREN ((name "A")) (SORT ((name "B"))) (SORT ((name "C")))))))

(check-true (equal?
             (m-predicate->cmd "vocab" (m-predicate "pname" '("A" "B" "C")))                    
             '(MARGRAVE-COMMAND ((type "ADD")) (VOCAB-IDENTIFIER ((vname "vocab")))
                                (PREDICATE ((name "pname"))) (RELATIONS (RELATION ((name "A"))) (RELATION ((name "B"))) (RELATION ((name "C")))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(check-true (m-formula? '(or (= x y) (r x y))))
(check-true (m-formula? #'(and (= x y) (iff (= 'c z) (r x y)))))
(check-true (m-formula? #'(implies (= x y) (not (r x y)))))
(check-true (m-formula? #'(forall x S (r x y))))
(check-true (m-formula? #'(exists x S (r x y))))
(check-true (m-formula? #'(isa x S (r x y))))
(check-false(m-formula? #'(forall X S (r x y))))
(check-false (m-formula? #'(exists X S (r x y))))
(check-false (m-formula? #'(isa X S (r x y))))
(check-true (m-formula? #'true))
(check-true (m-formula? 'true))
(check-true (m-formula? #'false))
(check-false (m-formula? #'(or (= x y) (A x 1))))
(check-true (m-formula? '((mypolicyname permit) s a r)))
(check-true (m-formula? #'((mypolicyname permit) s a r)))
(check-true (m-formula? '(aRelation s x)))
(check-false (m-formula? '((mypolicyname permit) 1 2 3)))
(check-false (m-formula? '(S x y)))
(check-false (m-formula? '(S )))
(check-true (m-formula? '(S x)))
(check-true (m-formula? '(S 'c)))
(check-true (m-formula? '(S (f 'c x y))))
(check-true (equal? 
             (m-formula->xexpr '(S (f 'c x y)))
             '(ISA ((sort "S")) (TERM (FUNCTION-TERM ((func "f")) (CONSTANT-TERM ((id "c"))) (VARIABLE-TERM ((id "x"))) (VARIABLE-TERM ((id "y"))))) (FORMULA (TRUE)))))
; ^^^ More cases go here. Not fully covered!

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(check-true (m-term? #'(f x (g x z) 'c x)))
(check-false (m-term? #'(f x (g x z) 'c 2)))
(check-false (m-term? #'('c x (g x z) 'c 2)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(check-true (equal? (m-axiom->xexpr '(constants-neq-all A)) '(CONSTRAINT ((type "CONSTANTS-NEQ-ALL")) (RELATIONS (RELATION ((name "A")))))))
(check-true (equal? (m-axiom->xexpr '(constants-neq 'a 'b)) '(CONSTRAINT ((type "CONSTANTS-NEQ")) (RELATIONS (RELATION ((name "a"))) (RELATION ((name "b")))))))
(check-true (equal? (m-axiom->xexpr '(disjoint preda predb)) '(CONSTRAINT ((type "DISJOINT")) (RELATIONS (RELATION ((name "preda"))) (RELATION ((name "predb")))))))
(check-true (equal? (m-axiom->xexpr '(formula (foo x y z))) 
                    '(CONSTRAINT ((type "FORMULA")) (ATOMIC-FORMULA (RELATION-NAME (ID ((id "foo")))) (TERMS (VARIABLE-TERM ((id "x"))) (VARIABLE-TERM ((id "y"))) (VARIABLE-TERM ((id "z"))))))))
(check-true (equal? (m-axiom->xexpr '(constants-neq-all ASort))
                    '(CONSTRAINT ((type "CONSTANTS-NEQ-ALL")) (RELATIONS (RELATION ((name "ASort")))))))
(check-true (equal? (m-axiom->xexpr '(partial-function foo)) '(CONSTRAINT ((type "PARTIAL-FUNCTION")) (RELATIONS (RELATION ((name "foo")))))))
(check-true (equal? (m-axiom->xexpr '(total-relation foo)) '(CONSTRAINT ((type "TOTAL-RELATION")) (RELATIONS (RELATION ((name "foo")))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Dereferencing terms in scenarios
; Testing whether tuples involve terms
; Filtering tuples from scenarios 
; Filtering relations from scenarios
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define dereference-term-test-scenario-1 
  (m-scenario 5 
              '("Atom#1" "Atom#2" "Atom#3" "Atom#4" "Atom#5")
              (list (m-relation "ASort" 'sort '(("Atom#1") ("Atom#2") ("Atom#3") ("Atom#4") ("Atom#5"))))
              (list (m-relation "$x" 'skolem '(("Atom#1"))) )
              (list (m-relation "c" 'constant '(("Atom#2")))
                    (m-relation "f" 'relation '(("Atom#1" "Atom#3")
                                                ("Atom#4" "Atom#5")))
                    (m-relation "g" 'relation '(("Atom#2" "Atom#1" "Atom#4"))))                    
              (m-statistics #f #f #f empty (hash))
              empty
              ""))
(check-true (equal? (dereference-term dereference-term-test-scenario-1 'x) "Atom#1"))
(check-true (equal? (dereference-term dereference-term-test-scenario-1 ''c) "Atom#2"))
(check-true (equal? (dereference-term dereference-term-test-scenario-1 '(f x)) "Atom#3"))
(check-true (equal? (dereference-term dereference-term-test-scenario-1 '(g 'c x)) "Atom#4"))
(check-true (equal? (dereference-term dereference-term-test-scenario-1 '(f (g 'c x))) "Atom#5"))
; (f c) not defined in this scenario, so should get an error:
(check-exn exn:fail? (lambda () (dereference-term dereference-term-test-scenario-1 '(f 'c))))
; no such variable
(check-exn exn:fail? (lambda () (dereference-term dereference-term-test-scenario-1 'z)))
; mis-use of constant as variable
(check-exn exn:fail? (lambda () (dereference-term dereference-term-test-scenario-1 'c)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(check-true (tuple-involves-terms dereference-term-test-scenario-1 '("Atom#1") '(x)))
(check-false (tuple-involves-terms dereference-term-test-scenario-1 '("Atom#1") '('c)))
(check-true (tuple-involves-terms dereference-term-test-scenario-1 '("Atom#1" "Atom123") '(x)))
(check-exn exn:fail? (lambda () (tuple-involves-terms dereference-term-test-scenario-1 '("Atom#1") '((f 'c)))))

(check-true (equal? (filter-tuples (lambda (atuple)
                                     (tuple-involves-terms dereference-term-test-scenario-1 
                                                           atuple
                                                           '()))
                                   dereference-term-test-scenario-1)
                    (m-scenario 5 
                                '("Atom#1" "Atom#2" "Atom#3" "Atom#4" "Atom#5")
                                (list (m-relation "ASort" 'sort '()))
                                (list (m-relation "$x" 'skolem '()) )
                                (list (m-relation "c" 'constant '())
                                      (m-relation "f" 'relation '())
                                      (m-relation "g" 'relation '()))                    
                                (m-statistics #f #f #f empty (hash))
                                empty
                                "")))
(check-true (equal? (filter-tuples (lambda (atuple)
                                     (tuple-involves-terms dereference-term-test-scenario-1 
                                                           atuple
                                                           '(x)))
                                   dereference-term-test-scenario-1)
                    (m-scenario 5
                                '("Atom#1" "Atom#2" "Atom#3" "Atom#4" "Atom#5")
                                (list (m-relation "ASort" 'sort '(("Atom#1"))))
                                (list (m-relation "$x" 'skolem '(("Atom#1"))))
                                (list
                                 (m-relation "c" 'constant '())
                                 (m-relation "f" 'relation '(("Atom#1" "Atom#3")))
                                 (m-relation "g" 'relation '(("Atom#2" "Atom#1" "Atom#4"))))
                                (m-statistics #f #f #f empty (hash))
                                empty
                                "")))
(check-true (equal? (filter-tuples (lambda (atuple)
                                     (tuple-involves-terms dereference-term-test-scenario-1 
                                                           atuple
                                                           '('c x (g 'c x))))
                                   dereference-term-test-scenario-1)
                    (m-scenario 5 
                                '("Atom#1" "Atom#2" "Atom#3" "Atom#4" "Atom#5")
                                (list (m-relation "ASort" 'sort '(("Atom#1") ("Atom#2") ("Atom#4"))))
                                (list (m-relation "$x" 'skolem '(("Atom#1"))) )
                                (list (m-relation "c" 'constant '(("Atom#2")))
                                      (m-relation "f" 'relation '(("Atom#1" "Atom#3")
                                                                  ("Atom#4" "Atom#5")))
                                      (m-relation "g" 'relation '(("Atom#2" "Atom#1" "Atom#4"))))                    
                                (m-statistics #f #f #f empty (hash))
                                empty
                                "")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(check-true (equal? (filter-relations (lambda (arelation)
                                        true)
                                      dereference-term-test-scenario-1)
                    (m-scenario 5 
                                '("Atom#1" "Atom#2" "Atom#3" "Atom#4" "Atom#5")
                                (list (m-relation "ASort" 'sort '(("Atom#1") ("Atom#2") ("Atom#3") ("Atom#4") ("Atom#5"))))
                                (list (m-relation "$x" 'skolem '(("Atom#1"))))
                                (list (m-relation "c" 'constant '(("Atom#2")))
                                      (m-relation "f" 'relation '(("Atom#1" "Atom#3")
                                                                  ("Atom#4" "Atom#5")))
                                      (m-relation "g" 'relation '(("Atom#2" "Atom#1" "Atom#4"))))
                                (m-statistics #f #f #f empty (hash))
                                empty
                                "")))
(check-true (equal? (filter-relations (lambda (arelation)
                                        false)
                                      dereference-term-test-scenario-1)
                    (m-scenario 5 
                                '("Atom#1" "Atom#2" "Atom#3" "Atom#4" "Atom#5")
                                (list )
                                (list )
                                (list )
                                (m-statistics #f #f #f empty (hash))
                                empty
                                "")))
(check-true (equal? (filter-relations (lambda (arelation)
                                        (or (equal? (m-relation-reltype arelation) 'constant)
                                            (equal? (m-relation-reltype arelation) 'sort)))
                                      dereference-term-test-scenario-1)
                    (m-scenario 5 
                                '("Atom#1" "Atom#2" "Atom#3" "Atom#4" "Atom#5")
                                (list (m-relation "ASort" 'sort '(("Atom#1") ("Atom#2") ("Atom#3") ("Atom#4") ("Atom#5"))))
                                (list )
                                (list (m-relation "c" 'constant '(("Atom#2"))))
                                (m-statistics #f #f #f empty (hash))
                                empty
                                "")))
(check-true (equal? (filter-relations (lambda (arelation)
                                        (not (equal? (m-relation-name arelation) "$x")))
                                      dereference-term-test-scenario-1)
                    (m-scenario 5 
                                '("Atom#1" "Atom#2" "Atom#3" "Atom#4" "Atom#5")
                                (list (m-relation "ASort" 'sort '(("Atom#1") ("Atom#2") ("Atom#3") ("Atom#4") ("Atom#5"))))
                                (list )
                                (list (m-relation "c" 'constant '(("Atom#2")))
                                      (m-relation "f" 'relation '(("Atom#1" "Atom#3")
                                                                  ("Atom#4" "Atom#5")))
                                      (m-relation "g" 'relation '(("Atom#2" "Atom#1" "Atom#4"))))
                                (m-statistics #f #f #f empty (hash))
                                empty
                                "")))
