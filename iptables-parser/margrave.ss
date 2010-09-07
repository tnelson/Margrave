#lang scheme

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Margrave Modeling Framework
;; Copyright (C) 2009-2010 Christopher Barratt  All rights reserved.
;;
;;  This file is part of Margrave.
;;
;;  Margrave is free software: you can redistribute it and/or modify
;;  it under the terms of the GNU Lesser General Public License as published by
;;  the Free Software Foundation, either version 3 of the License, or
;;  (at your option) any later version.
;;
;;  Margrave is distributed in the hope that it will be useful,
;;  but WITHOUT ANY WARRANTY; without even the implied warranty of
;;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;  GNU Lesser General Public License for more details.
;;
;;  You should have received a copy of the GNU Lesser General Public License
;;  along with Margrave.  If not, see <http://www.gnu.org/licenses/>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require (only-in srfi/1 lset-difference))

(provide atom<%>)
(provide variable<%>)
(provide predicate<%>)
(provide rule<%>)
(provide policy<%>)
(provide vocabulary<%>)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Atoms
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; An atom<%> is a value that Margrave may assign to a request variable.
;;   get-constraints : -> (listof (listof symbol))
;;     Returns the constraints for this atom
;;   get-name : -> symbol
;;     Returns the symbolic name for this atom
;;   covers? : atom<%> -> boolean
;;     Returns whether this atom covers another atom
;;   single? : -> boolean
;;     Returns whether this atom can appear at most once in a solution
;;   get-type-name : -> symbol
;;     Returns the symbolic name for the type of this atom
(define atom<%> (interface (equal<%>) get-constraints get-name covers? single? get-type-name))

;; symbol symbol -> atom<%>
;;   Creates an atom
(provide/contract
 [new-atom (-> symbol? symbol? (is-a?/c atom<%>))])

(define (new-atom name type-name)
  (new atom% [name name] [type-name type-name] [root? #f]))

;; symbol symbol -> atom<%>
;;   Creates a root atom
(provide/contract
 [new-root-atom (-> symbol? symbol? (is-a?/c atom<%>))])

(define (new-root-atom name type-name)
  (new atom% [name name] [type-name type-name] [root? #t]))

;; symbol symbol boolean
;;   A simple atom
(define atom%
  (class* object% (atom<%>)
    (init-field name type-name root?)
    (super-new)
    
    (define/public (equal-to? rhs equal-proc)
      (and (is-a? rhs atom<%>)
           (eq? name (send rhs get-name))
           (eq? type-name (send rhs get-type-name))))
    
    (define/public (equal-hash-code-of hash-proc)
      (equal-hash-code name))
    
    (define/public (equal-secondary-hash-code-of hash2-proc)
      (equal-secondary-hash-code name))
    
    (define/public (get-constraints)
      (if (single?)
          `((atmostone ,name))
          '()))
    
    (define/public (get-name)
      name)
    
    (define/public (covers? rhs)
      (if root?
          (not (equal-to? rhs equal?))
          #f))
    
    (define/public (single?)
      (not root?))
    
    (define/public (get-type-name)
      type-name)
    ))

;; An atom-tree<%> is a tree of atoms in which parents cover children.
;;   get-constraints : -> (listof (listof symbol))
;;     Returns the constraints for the atoms in this tree
;;   get-name : -> (listof any)
;;     Returns the symbolic representation of this tree
;;   get-root : -> atom<%>
;;     Returns the root atom for this tree
;;   get-type-name : -> symbol
;;     Returns the symbolic name for the type of atom in this tree
;;   covered? : atom<%> -> boolean
;;     Returns whether the specified atom covers this tree
;;   covers? : atom<%> -> boolean
;;     Returns whether this tree covers the specified atom
;;   insert : atom<%> -> atom-tree<%>
;;     Inserts the specified atom into this tree
;;   insert-list : (listof atom<%>) -> atom-tree<%>
;;     Inserts the specified atoms into this tree
;;   insert-tree : atom-tree<%> -> atom-tree<%>
;;     Inserts the specified atoms into this tree
;;   root-equal-to? : atom<%> -> boolean
;;     Returns whether the root atom of this tree equals the specified atom
(define atom-tree<%> (interface ()
                       get-constraints
                       get-name
                       get-root
                       get-type-name
                       covered?
                       covers?
                       insert
                       insert-list
                       insert-tree
                       root-equal-to?))

;; atom<%> -> atom-tree<%>
;;   Creates an atom tree
(provide/contract
 [new-atom-tree (-> (is-a?/c atom<%>) (is-a?/c atom-tree<%>))])

(define (new-atom-tree root)
  (new atom-tree% [root root] [children '()]))

;; atom<%> (listof atom-tree<%>)
;;   An atom tree
(define atom-tree%
  (class* object% (atom-tree<%>)
    (init-field root children)
    (super-new)
    
    (define/public (get-constraints)
      (if (empty? children)
          (send root get-constraints)
          (foldl (λ (child result)
                   (append (send child get-constraints) result))
                 (send root get-constraints)
                 children)))
    
    (define/public (get-name)
      (if (empty? children)
          (send root get-name)
          `(,(send root get-name)
            ,@(map (λ (child)
                     (send child get-name))
                   children))))
    
    (define/public (get-root)
      root)
    
    (define/public (get-type-name)
      (send root get-type-name))
    
    (define/public (covered? atom)
      (send atom covers? root))
    
    (define/public (covers? atom)
      (send root covers? atom))
    
    (define/public (insert atom)
      (cond [(not (eq? (get-type-name) (send atom get-type-name)))
             (error "Incompatible atom type" (send atom get-type-name))]
            [(root-equal-to? atom) this]
            [else (or (ormap (λ (child)
                               (cond
                                 ;; If the atom is already in this tree,
                                 ;; return this tree unchanged.
                                 [(send child root-equal-to? atom)
                                  this]
                                 ;; If this child tree covers the atom,
                                 ;; insert the atom there.
                                 [(send child covers? atom)
                                  (new atom-tree%
                                       [root root]
                                       [children (cons (send child insert atom)
                                                       (remove child children))])]
                                 ;; Move on to the next child.
                                 [else #f]))
                             children)
                      ;; Insert a new tree for the atom into the children for
                      ;; this tree, moving covered children into the new tree.
                      (let-values [([covered-children uncovered-children]
                                    [partition (λ (child)
                                                 (send child covered? atom))
                                               children])]
                        (new atom-tree%
                             [root root]
                             [children (cons (new atom-tree%
                                                  [root atom]
                                                  [children covered-children])
                                             uncovered-children)])))]))
    
    (define/public (insert-list atoms)
      (foldl (λ (atom final-tree)
               (send final-tree insert atom))
             this
             atoms))
    
    (define/public (insert-tree tree)
      (foldl (λ (sub-tree final-tree)
               (send final-tree insert-tree sub-tree))
             (send tree insert (get-root))
             children))
    
    (define/public (root-equal-to? atom)
      (send root equal-to? atom equal?))
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Variables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A variable<%> is a request variable.
;;   name : -> symbol
;;     Returns the symbolic name for this variable
;;   required? : -> boolean
;;     Returns whether this variable is required for all rules
;;   type-name : -> symbol
;;     Returns the symbolic name for the type of this variable
(define variable<%> (interface (equal<%>) get-name required? get-type-name))

;; symbol symbol -> variable<%>
;;   Creates a variable
(provide/contract
 [new-required-variable (-> symbol? symbol? (is-a?/c variable<%>))]
 [new-optional-variable (-> symbol? symbol? (is-a?/c variable<%>))])

(define (new-required-variable name type-name)
  (new variable% [name name] [type-name type-name] [required #t]))

(define (new-optional-variable name type-name)
  (new variable% [name name] [type-name type-name] [required #f]))

;; symbol symbol boolean
;;   A variable
(define variable%
  (class* object% (variable<%>)
    (init-field name type-name required)
    (super-new)
    
    (define/public (equal-to? rhs equal-proc)
      (and (is-a? rhs variable<%>)
           (eq? name (send rhs get-name))
           (eq? type-name (send rhs get-type-name))))
    
    (define/public (equal-hash-code-of hash-proc)
      (equal-hash-code name))
    
    (define/public (equal-secondary-hash-code-of hash2-proc)
      (equal-secondary-hash-code-of type-name))
    
    (define/public (get-name)
      name)
    
    (define/public (required?)
      required)
    
    (define/public (get-type-name)
      type-name)
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Predicates
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A predicate<%> is a request predicate.
;;   get-name : -> symbol
;;     Returns the symbolic representation for this predicate
;;   get-atoms : -> (listof atom<%>)
;;     Returns the atoms that this predicate references
;;   get-variables : -> (listof variable<%>)
;;     Returns the variables that this predicate references
(define predicate<%> (interface (equal<%>) get-name get-atoms get-variables))

;; atom<%> variable<%> -> predicate<%>
;;   Creates a predicate
(provide/contract
 [new-predicate (-> (is-a?/c atom<%>) (is-a?/c variable<%>) (is-a?/c predicate<%>))])

(define (new-predicate atom variable)
  (new predicate% [atom atom] [variable variable]))

;; atom<%> variable<%>
;;   A simple predicate
(define predicate%
  (class* object% (predicate<%>)
    (init-field atom variable)
    (super-new)
    
    (define/public (equal-to? rhs equal-proc)
      (local [(define (object-equals? lhs rhs)
                (send lhs equal-to? rhs equal-proc))]
        (and (is-a? rhs predicate<%>)
             (empty? (lset-difference object-equals?
                                      (list atom)
                                      (send rhs get-atoms)))
             (empty? (lset-difference object-equals?
                                      (list variable)
                                      (send rhs get-variables))))))
    
    (define/public (equal-hash-code-of hash-proc)
      (send variable equal-hash-code-of hash-proc))
    
    (define/public (equal-secondary-hash-code-of hash2-proc)
      (send atom equal-secondary-hash-code-of hash2-proc))
    
    (define/public (get-name)
      `(,(send atom get-name) ,(send variable get-name)))
    
    (define/public (get-atoms)
      (list atom))
    
    (define/public (get-variables)
      (list variable))
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Rules
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A rule<%> is a rule.
;;   get-atoms : -> (listof atom<%>)
;;     Returns the atoms that this rule contains
;;   get-name : -> symbol
;;     Returns the symbolic name for this rule
;;   get-decision : -> symbol
;;     Returns the decision for this rule
;;   get-predicates : -> (listof predicate<%>)
;;     Returns the predicates that this rule contains
;;   get-constraints : -> (listof (listof symbol}}
;;     Returns the constraints for this rule
(define rule<%> (interface ()
                  get-atoms
                  get-name
                  get-decision
                  get-predicates
                  get-constraints))

;; symbol symbol (listof variable<%>) (listof predicate<%>)
;;   Creates a rule
(provide/contract
 [new-rule (-> symbol?
               symbol?
               (listof (is-a?/c variable<%>))
               (listof (is-a?/c predicate<%>))
               (is-a?/c rule<%>))])

(define (new-rule name decision variables predicates)
  [new rule%
       [name name]
       [decision decision]
       [variables variables]
       [predicates predicates]])

;; symbol symbol (listof variable<%>) (listof predicate<%>)
;;   A rule
(define rule%
  (class* object% (rule<%>)
    (init-field name decision variables predicates)
    (super-new)
    
    (define/public (get-atoms)
      (remove-duplicates (flatten (map (λ (predicate)
                                         (send predicate get-atoms))
                                       predicates))))
    
    (define/public (get-name)
      name)
    
    (define/public (get-decision)
      decision)
    
    (define/public (get-predicates)
      predicates)
    
    (define/public (get-constraints)
      (remove-duplicates (foldl (λ (atom result)
                                  (append (send atom get-constraints) result))
                                '()
                                (get-atoms))))
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Policies
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A policy<%> is a policy.
;;   get-name : -> symbol
;;     Returns the symbolic name for this policy
;;   get-vocabulary-name : -> symbol
;;     Returns the symbolic name for the vocabulary that this policy uses
;;   get-rules : -> (listof rule<%>)
;;     Returns the rules that this policy contains
;;   get-constraints : -> (listof (listof symbol}}
;;     Returns the constraints for this policy
(define policy<%> (interface () get-name get-vocabulary-name get-rules get-constraints))

;; symbol symbol (listof rule<%>)
;;   Creates a policy
(provide/contract
 [new-policy (-> symbol?
                 symbol?
                 (listof (is-a?/c rule<%>))
                 (is-a?/c policy<%>))])

(define (new-policy name vocabulary-name rules)
  (new policy%
       [name name]
       [vocabulary-name vocabulary-name]
       [rules rules]))

;; symbol symbol (listof rule<%>)
;;   A policy
(define policy%
  (class* object% (policy<%>)
    (init-field name vocabulary-name rules)
    (super-new)
    
    (define/public (get-name)
      name)
    
    (define/public (get-vocabulary-name)
      vocabulary-name)
    
    (define/public (get-rules)
      rules)
    
    (define/public (get-constraints)
      (append (remove-duplicates (foldl (λ (rule result)
                                          (append (send rule get-constraints) result))
                                        '()
                                        rules))
              ; Abstract constraints, until we merge (e.g.) Port and port-any
              '( (abstract Address) (abstract Port) (abstract Interface) (abstract Protocol) (abstract Chain) (abstract State) (abstract Message) (abstract Flags))
              ; Same with disjoint-all for these special sorts (ip-0-0-0-0/0-0-0-0 already dealt with)
              ; flags need not be disjoint
              '( (disjoint-all port-0:65535) (disjoint-all ip) (disjoint-all chain-any) (disjoint-all if-any) (disjoint-all icmp-any))))
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Vocabularies
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A vocabulary<%> is a vocabulary.
;;   get-name : -> symbol
;;     Returns the symbolic name for this vocabulary
;;   get-types : -> hash
;;     Returns the types that this vocabulary defines
;;   get-decisions : -> (listof symbol)
;;     Returns the types that this vocabulary defines
;;   get-predicates : -> (listof predicate<%>)
;;     Returns the predicates that this vocabulary defines
;;   get-variables : -> (listof variable<%>)
;;     Returns the variables that this vocabulary defines
;;   get-constraints : -> (listof (listof symbol))
;;     Returns the constraints that this vocabulary defines
;;   get-text : -> S-expr
;;     Returns the S-expression representation of this vocabulary
(define vocabulary<%> (interface ()
                        get-name
                        get-types
                        get-decisions
                        get-predicates
                        get-variables
                        get-constraints
                        get-text))

;; symbol hash (listof symbol) (listof predicate<%>) (listof variable<%>) (listof (listof any)) -> vocabulary<%>
;;   Creates a vocabulary
(provide/contract
 [new-vocabulary (-> symbol?
                     hash?
                     (listof symbol?)
                     (listof (is-a?/c predicate<%>))
                     (listof (is-a?/c variable<%>))
                     (listof (listof any/c))
                     (is-a?/c vocabulary<%>))])

(define (new-vocabulary name types decisions predicates variables constraints)
  (new vocabulary%
       [name name]
       [types types]
       [decisions decisions]
       [predicates predicates]
       [variables variables]
       [constraints constraints]))

;; symbol hash (listof symbol) (listof predicate<%>) (listof variable<%>) (listof (listof symbol}}
;;   A vocabulary
(define vocabulary%
  (class* object% (vocabulary<%>)
    (init-field name types decisions predicates variables constraints)
    (super-new)
    
    (define/public (get-name)
      name)
    
    (define/public (get-types)
      types)
    
    (define/public (get-decisions)
      decisions)
    
    (define/public (get-predicates)
      predicates)
    
    (define/public (get-variables)
      variables)
    
    (define/public (get-constraints)
      constraints)
    
    (define/public (get-text)
      `(PolicyVocab ,name
                    (Types ,@(hash-map types
                                       (λ (type-name type)
                                         `(,type-name : ,(send type get-name)))))
                    (Decisions ,@decisions)
                    (Predicates ,@(map (λ (predicate)
                                         (send predicate get-name))
                                       predicates))
                    (ReqVariables ,@(map (λ (variable)
                                           `(,(send variable get-name) : ,(send variable get-type-name)))
                                         (filter (λ (variable)
                                                   (send variable required?))
                                                 variables)))
                    (OthVariables ,@(map (λ (variable)
                                           `(,(send variable get-name) : ,(send variable get-type-name)))
                                         (filter (λ (variable)
                                                   (not (send variable required?)))
                                                 variables)))
                    (Constraints ,@constraints)
                    ))
    ))
