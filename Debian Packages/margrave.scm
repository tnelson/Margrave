;    Copyright © 2009 Brown University and Worcester Polytechnic Institute.
;    
;    This file is part of Margrave.

;    Margrave is free software: you can redistribute it and/or modify
;    it under the terms of the GNU Lesser General Public License as published by
;    the Free Software Foundation, either version 3 of the License, or
;    (at your option) any later version.
;
;    Margrave is distributed in the hope that it will be useful,
;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;    GNU Lesser General Public License for more details.
;
;    You should have received a copy of the GNU Lesser General Public License
;    along with Margrave.  If not, see <http://www.gnu.org/licenses/>.

; tn

; *********************************
; IMPORTANT:
; Need to use R5RS since SISC does.
; *********************************

(import s2j)

; NOTE
; At the moment, need to have the folder with all the Margrave
; .class files added to classpath

(define-java-class <MVocab> |edu.wpi.margrave.MVocab|)
(define-java-class <MPolicyLeaf> |edu.wpi.margrave.MPolicyLeaf|)
(define-java-class <MPolicySet> |edu.wpi.margrave.MPolicySet|)
(define-java-class <MPolicy> |edu.wpi.margrave.MPolicy|)
(define-java-class <MCustomIDB> |edu.wpi.margrave.MCustomIDB|)
(define-java-class <MQuery> |edu.wpi.margrave.MQuery|)
(define-java-class <MREPL> |edu.wpi.margrave.MREPL|)
(define-java-class <ArrayList> |java.util.ArrayList|)

; Save the current directory when this file is loaded.
; (Will be our absolute Margrave path.)
(define my-directory (current-directory))
; Windows doesn't provide a good current-directory
(if (string=? my-directory ".")
    (set! my-directory "./")
    void)
(if (string=? my-directory "file:.")
    (set! my-directory "file:./")
    void)

(define myMargrave (java-new <MREPL>))

;****************************************************************

; Initialize policy file name.
; This is used by load-policy to assure we don't need to change
; the working directory.
; see normalize-url
(define local-policy-filename ".")

; Helper functions for MVocab and MPolicy objects

(define (contains theElement theList)
  (if (eqv? '() theList)
      #f
      (if (equal? theElement (car theList)) 
          #t
          (contains theElement (cdr theList)))))
  

(define (removeall toRemove sourceList)
  (filter-helper (lambda (element)
                   (not (contains element toRemove)))
                 sourceList))

; Filter isn't implemented in SISC
; Inefficient replacement for now
(define (filter-helper fun thelist)
  (if (eqv? '() thelist)
      thelist ; empty, no change
      (if (fun (car thelist))
          (cons (car thelist) (filter-helper fun (cdr thelist)))
          (filter-helper fun (cdr thelist)))))

; listsubs contains a list of the subsorts for this sort. 
; However, it may be nested: subsorts may themselves have subsorts.
(define (add-subtypes-of vocab parent listsubs)  
  ; listsubs may be empty -- if so, do nothing (already added parent)
  (if (> (length listsubs) 0)            
      ; Subtypes exist -- Add each subtype
      (for-each (lambda (s) 
                 (if (list? s) 
                     
                     (begin
                       ; Add subtype relationship between parent and s
                       ((generic-java-method '|addSubSort|) 
                        vocab (->jstring parent) (->jstring (car s)))
                       
                       ; Is this a nested subtype? If so, we must
                       ; deal with s's subtypes.
                       ; Check for list size;
                       ; someone may have used parens without meaning to.
                       (if (> (length s) 1)
                           (add-subtypes-of vocab (car s) (cdr s))))
                     
                     ((generic-java-method '|addSubSort|) vocab (->jstring parent) (->jstring s)))) 
               
               listsubs)
      void))

; list->jlist: scheme-list-of-strings-or-symbols -> jlist
; Does the obvious.
(define (list->jlist thelist)
  ; Instantiate an empty list
  (let ((jlst (java-new <ArrayList>)))
    ; Copy the scheme list into the java list
    (map (lambda (x) ((generic-java-method '|add|)
                      jlst
                      (->jstring x))) 
         thelist)
    ; Return the filled list
    jlst))

; jlist->list: java-list-of-strings -> list
; Does the obvious.
(define (jlist-helper thejlist idx)
  (if (< idx 0)
      '()
      (cons (->string ((generic-java-method '|get|) thejlist (->jint idx)))
            (jlist-helper thejlist (- idx 1)))))
  
(define (jlist->list thejlist)  
    (jlist-helper thejlist (- (->number ((generic-java-method '|size|) thejlist)) 1)))


; Same as list->jlist but doesn't try to pass the list
; elements through ->jstring
(define (list->jlistx thelist)
  (let ((jlst (java-new <ArrayList>)))
    (map (lambda (x) ((generic-java-method '|add|)
                     jlst
                     x))
         thelist)
    jlst))

; list->jstring: scheme-list -> scheme-string
; Flattens the list into a scheme-string, with spaces between
; the elements.
(define (list->string thelist)
  (cond [(eqv? '() thelist) ""]
        [(not (list? thelist)) (symbol->string thelist)]
        [else (string-append (symbol->string (car thelist)) 
                     (if (eqv? '() (cdr thelist))
                         ""
                         " ")
                     (list->string (cdr thelist)))]))

(define (add-constraint vocab typename listrels)
  ; Switch by typename:
  ; (Assume user is passing appropriate number of arguments)
  
  (let ((axioms 
         ((generic-java-field-accessor '|axioms|) vocab)))
    (cond 
      ; typename is a symbol at this point, not a string    
      ((eqv? typename 'disjoint) ((generic-java-method '|addConstraintDisjoint|) axioms (->jstring (car listrels)) (->jstring (car (cdr listrels)))))
      ((eqv? typename 'disjoint-all) ((generic-java-method '|addConstraintDisjointAll|) axioms (->jstring (car listrels))))
      ((eqv? typename 'nonempty) ((generic-java-method '|addConstraintNonempty|) axioms (->jstring (car listrels))))
      ((eqv? typename 'nonempty-all) ((generic-java-method '|addConstraintNonemptyAll|) axioms (->jstring (car listrels))))
      ((eqv? typename 'singleton) ((generic-java-method '|addConstraintSingleton|) axioms (->jstring (car listrels))))
      ((eqv? typename 'singleton-all) ((generic-java-method '|addConstraintSingletonAll|) axioms (->jstring (car listrels))))
      ((eqv? typename 'atmostone) ((generic-java-method '|addConstraintAtMostOne|) axioms (->jstring (car listrels))))
      ((eqv? typename 'atmostone-all) ((generic-java-method '|addConstraintAtMostOneAll|) axioms (->jstring (car listrels))))
      ((eqv? typename 'partial-function) ((generic-java-method '|addConstraintPartialFunction|) axioms (->jstring (car listrels))))
      ((eqv? typename 'total-function) ((generic-java-method '|addConstraintTotalFunction|) axioms (->jstring (car listrels))))
      ((eqv? typename 'abstract) ((generic-java-method '|addConstraintAbstract|) axioms (->jstring (car listrels))))
      ((eqv? typename 'abstract-all) ((generic-java-method '|addConstraintAbstractAll|) axioms (->jstring (car listrels))))

      ((eqv? typename 'subset) ((generic-java-method '|addConstraintSubset|) axioms (->jstring (car listrels)) (->jstring (car (cdr listrels)))))
      (else (display " Error! Unsupported constraint type")))))

; Add a custom relation of type (car listrels) X (car (cdr listrels)) X ...
; Java expects an (unneeded!) arity value
(define (add-predicate vocab predname listrels)
  ((generic-java-method '|addPredicate|) 
   vocab 
   (->jstring predname)
   (->jstring (list->string listrels))))

; Sets the target property of a policy object
(define (set-target mypolicy conjlist)
  ((generic-java-method '|setTarget|) mypolicy (list->jlist conjlist)))

; Add a rule of the form rulename = (dtype reqvars) :- conjlist
(define (add-rule mypolicy myvarorder rulename dtype reqvars conjlist)
  (if (not (string=? myvarorder 
                 (apply string-append (map 
                                       (lambda (x) (string-append x " ")) ; leave trailing whitespace in java api too.
                                       (map symbol->string reqvars))))) 
      (begin (display "Error: Unable to add rule. Variable ordering ")
             (display reqvars)
             (newline)
             (display "did not agree with vocabulary, which expected ")
             (display myvarorder) 
             (display ".")
             (newline))
      
      ((generic-java-method '|addRule|) 
       mypolicy 
       (->jstring rulename)
       (->jstring dtype)
       (list->jlist conjlist))))
  
; PolicyVocab: Parses a vocabulary definition and creates an MVocab object
(define-syntax PolicyVocab
  (syntax-rules (Types Decisions Constraints Predicates Variables :)
    ((PolicyVocab myvocabname 
                  (Types (t : subt ...) ...) 
                  (Decisions r ...) 
                  (Predicates (pname : prel ...) ...)
                  (ReqVariables (rvname : rvsort) ...)
                  (OthVariables (ovname : ovsort) ...)
                  (Constraints (ctype crel ...) ...) )
     (let ((thisvocab (java-new <MVocab> (->jstring 'myvocabname )))) ; Instantiate a new MVocab object  
              
       ; These sections must be in order.
       
      ; (display "Parsing Types") (newline)       
       ; Types
       (begin
         ((generic-java-method '|addSort|) thisvocab (->jstring 't))
         (add-subtypes-of thisvocab 't (list 'subt ...)))         
       ... ; for each type/subtype set
       
      ; (display "Parsing Decisions") (newline)

       
       ; Decisions               
       ((generic-java-method '|addDecision|) thisvocab (->jstring 'r)) 
       ...
       
      ; (display "Parsing Predicates") (newline)
       ; Predicates
       (add-predicate thisvocab 'pname (list 'prel ...))
       ... ; for each custom predicate
              
      ; (display "Parsing Variables") (newline)       
       ; Request Variables
       ((generic-java-method '|addRequestVar|) thisvocab (->jstring 'rvname) (->jstring 'rvsort))
       ... ; for each req var
       
      ; (display "Parsing Other Variables") (newline)
       ; Other Variables
       ((generic-java-method '|addOtherVar|) thisvocab (->jstring 'ovname) (->jstring 'ovsort))
       ... ; for each oth var
       
       ;(display "Parsing Constraints") (newline)
       ; Constraints
       (add-constraint thisvocab 'ctype (list 'crel ...))       
       ... ; for each constraint

      ; (display "Vocab parsed") (newline)
       ; Return the object for use by the policy macro
       thisvocab)))) 



; Policy: Parses a policy definition and creates an MPolicyLeaf OR MPolicySet object
; Policies are permitted to have child policies, so this may be recursively called
; (but is guaranteed to terminate.)
(define-syntax Policy
  (syntax-rules (Target Rules = :- uses RComb PComb Children)
    ((Policy policyname uses vocabname
             (Target tconj ...) ; back end expects a list of conjuncts
             (Rules (rulename = (dtype v ...) :- conj ...) ; for each variable in the IDB dec; for each conjunct in the rule itself                    
                    ...); for each rule
             (RComb rcstr ...) ; rule combination alg?
             (PComb pcstr ...) ; policy combination alg?
             (Children child ...)) ; child policies? 
     
     ; Is the vocab named 'vocabname defined? If so, use it. If not, open it and add it to the list.   
  
     ; Return a function of one argument that can be called in the context of some local directory.
     ; This is so we know where to find the vocabulary file.
     (lambda (local-policy-filename) 
       (let ((mychildren (list child ...))
             (myvocab              
                   ; they all must be lower-case file names.                    
                   ; Eval to trigger the macro
                           (eval (read (open-input-file
                                           (normalize-url 
                                               ; Make sure we look in the correct directory!
                                               local-policy-filename 
                                               (string-append (symbol->string 'vocabname) ".v"))))))) 
        (let ((mypolicy (if (< (length mychildren) 1)
                            (java-new <MPolicyLeaf> (->jstring 'policyname) myvocab)
                            (java-new <MPolicySet> (->jstring 'policyname) myvocab)))
              (myvarorder (->string ((generic-java-method '|getExpectedRequestVarOrder|) myvocab))))

          
          ; Set the policy target (if any)
          (set-target mypolicy (list 'tconj ...))
          
          ; Add the rules to the policy. 'true is dealt with in the back-end.         
          (add-rule mypolicy myvarorder 'rulename 'dtype (list 'v ...) (list (list->string 'conj) ...))
          ...
          
          ; Set the rule and policy combinator (depending on type)
          (if (< (length mychildren) 1)
              ((generic-java-field-modifier '|rCombine|) mypolicy (->jstring (list->string (list 'rcstr ...))))
              ((generic-java-field-modifier '|pCombine|) mypolicy (->jstring (list->string (list 'pcstr ...)))))
          
          ; Each child is a Policy
          (let ((cpol child))
            ((generic-java-method '|addChild|) mypolicy cpol)
            )
          ...
          
          ; Trigger IDB calculation
          ((generic-java-method '|initIDBs|) mypolicy)
          
          
          mypolicy)))))) ; Return this policy object (used by policy hierarchy code above)



; ***************************************************************************************
; User Functions
; ***************************************************************************************

;; IMPORTANT
; When adding new user functions, be certain that all string arguments are converted to
; lower case!

; policy-file-name -> MPolicy
; This function is used because a raw (load x) call will return void, not the object desired.
; Note: rather than load with case-sensitivity turned on, all input strings need to be passed
; to the backend in lower-case.
(define (load-policy fn)
;  (case-sensitive #t)
;  (display (read (open-input-file fn))) (newline)
;  (case-sensitive #f)  
 ; (display "*** ") (display fn) (newline)
  ;; Macro returns a func 
  ;; Potential security issues here, calling eval on arbitrary code that we "promise" is an
  ;; innocent policy definition. Is there a fix for this?
 ; (case-sensitive #t)
  (let ([pol ((eval (read (open-input-file fn))) fn)])
   ; (case-sensitive #f)
    pol))

; define-policy
; string MPolicy -> void
; Saves the given policy under the given identifier in the Java env
; does NOT save the policy in scheme!
(define (define-policy pname thepol)
  (->boolean ((generic-java-method '|savePolicyAs|) myMargrave (->jstring pname) thepol)))


; the above doesn't quite work. scope of def?

; m
; string -> void
; Runs the given Margrave query or command.
(define (m cmd)
  ((generic-java-method '|command|) myMargrave (->jstring cmd)))



; xacml-policy-filename -> MPolicy
; Loads an XACML policy 
(define (load-xacml-policy fn)
  ((generic-java-method '|readXACML|) (java-null <MPolicy>) (->jstring fn) (->jstring (string-append my-directory "xacml20.xsd"))))

; sqs-policy-filename -> MPolicy
; Loads an XACML policy 
(define (load-sqs-policy fn)
  ((generic-java-method '|loadSQS|) (java-null <MPolicy>) (->jstring fn) ))


; MPolicy -> void
(define (print-policy-info pol)
  ((generic-java-method '|printPolicyInfo|) pol))

; MPolicy string string -> void
(define (assume-disjoint pol sort1 sort2)
  ((generic-java-method '|addDisjointAssumption|) pol (->jstring sort1) (->jstring sort2)))

(define (assume-subset pol sortchild sortparent)
  ((generic-java-method '|addSubsetAssumption|) pol (->jstring sortchild) (->jstring sortparent)))

(define (assume-disjoint-with-prefix pol sortprefix)
  ((generic-java-method '|addDisjointPrefixAssumption|) pol (->jstring sortprefix)))

; MPolicy string
(define (assume-singleton pol sortname)
  ((generic-java-method '|addSingletonAssumption|) pol (->jstring sortname)))

; MPolicy string -> MQuery
; Creates an MQuery object representing the query string given being run on the given MPolicy.
(define (query-policy p str)
  ((generic-java-method '|queryPolicy|) p (->jstring (string-downcase str))))

; MQuery -> int
; Returns the number of solutions the query has.
(define (count-solutions qry)
  (->number ((generic-java-method '|countSatisfyingSolutions|) ((generic-java-method '|runQuery|) qry))))

; MQuery list-of-strings -> list-of-strings

(define (get-populated-relations-in-first-solution qry candidates)
  ((generic-java-method '|getPopulatedRelationNamesInFirstSolution|) qry (list->jlist candidates)))

; set-idb-output-indexing: MQuery string list-of-strings -> void
; Tells Margrave how to index an IDB for output when tupling a query.
(define (set-idb-output-indexing qry idbname indexing)
  ((generic-java-method '|setIDBOutputIndexing|) qry (->jstring idbname) (list->jlist indexing)))

; MPolicy MPolicy -> void
; Out-of-the-box change impact function: No need to enter a cumbersome query.
(define (get-policy-differences pol1 pol2)
  ((generic-java-method '|compareWithPolicy|) pol1 pol2))    

; MPolicy string -> void
(define (set-policy-name pol name)
  ((generic-java-method '|setName|) pol (->jstring name)))

; MPolicy string -> void
; Using the Margrave query language, state a general assumption that any query involving this policy 
; must respect. (For instance : "(forall s Subject (or (isconflicted=true s) (isconflicted=false s)))")
(define (add-custom-assumption pol str)
 ((generic-java-method '|addCustomConstraint|)
  ((generic-java-field-accessor '|assumptions|) pol)
  (->jstring str)))

; list-of-MPolicies string -> MQuery
; Same as above, except the MQuery "sees" all policies in the given list. 
; (For instance, given 2 policies, exactly when will one render permit but the other render deny?)
(define (query-policies plist str)
  ((generic-java-method '|queryThesePolicies|) (java-null <MQuery>) (->jstring (string-downcase str)) (list->jlistx plist)))

; MQuery query-string -> MQuery
; Returns a newly made query object whose formula is the conjunction of the given query's and the new string.
(define (refine-query qryobj str)
  ((generic-java-method '|refineQuery|) qryobj (->jstring (string-downcase str))))

; MQuery -> boolean
; Returns whether KodKod returns any satisfying instances for this query.
; (Note the conversion back into scheme booleans.)
(define (is-query-satisfiable? qry)
  (->boolean ((generic-java-method '|isQuerySatisfiable|) qry)))

; MQuery -> void
; Pretty prints the query results (if any)
(define (pretty-print-results qry)
  ((generic-java-method '|prettyPrintSolutions|) qry))

; MQuery -> void
; Outputs the query results in block format, with don't care literals treated properly.
; Calling this can be *much* more succinct than pretty-print-results.
(define (pretty-print-results-condensed qry)
  ((generic-java-method '|prettyPrintSolutionsCondensed|) qry))

; MQuery -> void
; Prints ONE solution.
(define (pretty-print-one-solution qry)
  ((generic-java-method '|prettyPrintOneSolution|) qry))

; MQuery int -> void
; Sets debugging info level. See README.
(define (set-debug-level qry level)
  ((generic-java-field-modifier '|debug_verbosity|) qry (->jint level)))

; MQuery bool -> void
; Activates use of tupling query optimization
(define (set-tupling qry b)
  ((generic-java-field-modifier '|doTupling|) qry (->jboolean b)))


; MQuery -> void
; Flags certain IDB names for output. Will provide more output information,
; but may make query execution slower.
(define (set-idb-output-list qry lst)
  ((generic-java-field-modifier '|idb_names_to_output|) qry (list->jlist lst)))

(define (parser-debug b)
  ((generic-java-field-modifier '|debugParser|) myMargrave (->jboolean b)))


; string list-of-MIDBs list-of-strings string -> MCustomIDB
; Creates a custom IDB object. Use in place of Policy objects when running queries.
(define (make-custom-idb idbname list-of-other-idbsets list-varorder viewstring)
  (java-new <MCustomIDB> (->jstring idbname) (list->jlistx list-of-other-idbsets) (list->jlist list-varorder) (->jstring viewstring)))

; MQuery int -> void
; Sets the model size ceiling
(define (set-size-ceiling qry n)
  ((generic-java-field-modifier '|sizeCeiling|) qry (->jint n)))

; MQuery -> void
; Prints out the Query object's current settings
(define (print-query-settings qry)
  ((generic-java-method '|printSettings|) qry))

; MQuery symbol -> void
; Sets the given MQuery object's satsolver. Allowed:
; 'defaultsat4j 
; 'minisat
(define (set-query-satsolver qry sym)
  (cond [(eqv? sym 'minisat) ((generic-java-method '|useMiniSAT|) qry)]
        [(eqv? sym 'sat4j) ((generic-java-method '|useSAT4j|) qry)]
        [else (begin (display "Error! Unknown satsolver type.") (newline))]))

(define (set-default-satsolver sym)
  (cond [(eqv? sym 'minisat) ((generic-java-method '|useMiniSATAsDefault|) qry)]
        [(eqv? sym 'sat4j) ((generic-java-method '|useSAT4jAsDefault|) qry)]
        [else (begin (display "Error! Unknown satsolver type.") (newline))]))


; Functions to support easier query string creation

; get-existential-request-prefix
(define (get-existential-request-prefix pol)
  (->string ((generic-java-method '|getExistentialRequestPrefix|) pol)))

; get-request-prefix-closing
(define (get-request-prefix-closing pol)
  (->string ((generic-java-method '|getRequestPrefixClosing|) pol)))

; get-request-var-vector
(define (get-request-var-vector pol)
  (->string ((generic-java-method '|getRequestVarVector|) pol)))

; get-idbname-list
(define (get-idbname-list pol)
  (jlist->list ((generic-java-method '|getIDBNameList|) pol)))

; get-qualified-idbname-list
; Same as get-idbname-list but includes policy name prefix
(define (get-qualified-idbname-list pol)
  (jlist->list ((generic-java-method '|getQualifiedIDBNameList|) pol)))

; get-request-var-list
(define (get-request-var-list pol)
  (jlist->list ((generic-java-method '|getRequestVarList|) pol)))

; get-decision-for-idbname
; Policy String -> String
; Given an idbname, policy will report its decision if a rule, or the empty string otherwise
(define (get-decision-for-rule-idbname policy idbname)
  (->string ((generic-java-method '|getDecisionForRuleIDBName|) policy (->jstring idbname))))

; rules-with-higher-priority
; Policy String -> List
; Returns a list of rule idb names who have higher priority than the given rule.
; (This doesn't consider whether an overlap is possible, just the rule-ordering
;  given by combining algs.) Names are qualified with policyname:.
;; TODO: Only works for Leaves, not Sets so far.
(define (rule-idbs-with-higher-priority pol rulename)
  (jlist->list ((generic-java-method '|ruleIDBsWithHigherPriorityThan|) pol (->jstring rulename))))


; **************************************************************
; Test case procedures

(define (test desc s1 s2)
  (if (eqv? s1 s2)
      (display (string-append desc ": Passed."))
      (display (string-append desc ": FAILED!")))
  (newline))

(define (test-model desc qry size exp_sols exp_ceiling)
  (if (->boolean ((generic-java-method '|runTestCase|) qry (->jint size) (->jint exp_sols) (->jint exp_ceiling)))
      (display (string-append desc ": Passed."))
      (display (string-append desc ": FAILED!")))
  (newline))

(define (pause-for-user) 
  (display "======================== Hit enter to continue. ========================") (newline) (newline) (read-char))

(define-java-class <MFormulaManager> |edu.wpi.margrave.MFormulaManager|)
(define-java-class <System> |java.lang.System|)

; Should only be used for testing purposes:

(define (run-gc)
  ((generic-java-method '|gc|) (java-null <System>)))

(define (print-cache-info)
  ((generic-java-method '|printStatistics|) (java-null <MFormulaManager>)))

(define (get-system-clock-ms)
  (->number ((generic-java-method '|currentTimeMillis|) (java-null <System>))))
; *************************************************************



