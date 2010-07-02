;    Copyright © 2009-2010 Brown University and Worcester Polytechnic Institute.
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
; Modifications by TN and VS, June 2010

#lang racket

(require racket/system)

(require framework)

(require xml)

; Save the current directory when this file is loaded.
; (Will be our absolute Margrave path.)
(define my-directory (path->string (current-directory)))

;****************************************************************
;;Java Connection

(define windows? (equal? 'windows (system-path-convention-type)))

(define java-class-separator
  (if windows?
      ";"
      ":"))

;pid should be the process number of the process to kill
(define (close-last pid)
  (if windows?
        ;On Windows, need to kill all child process (java.exe). Automatically happens on *nix
        (process (string-append "taskkill /pid " (number->string pid) " /t"))
        (process (string-append "kill " (number->string pid)))))

;Name for file that contains the last process ID started
(define input-file-name
  (string-append 
                  (path->string (build-path (current-directory)))
                  ".javaIsStarted"))

;Close last process
;Read in PID
#;(when (file-exists? input-file-name)
  (begin
      ;(display last-pid)
      (close-input-port input-file-name)))

(define margrave-command-line
  (string-append
   "java -cp "
   (path->string
    (build-path (current-directory)
                "bin"
                "margrave.jar"))
   java-class-separator
   (path->string
    (build-path (current-directory)
                "bin"
                "kodkod.jar"))
   java-class-separator
   (path->string
    (build-path (current-directory)
                "bin"
                "org.sat4j.core.jar"))
   java-class-separator
   (path->string
    (build-path (current-directory)
                "bin"
                "sunxacml.jar"))
   java-class-separator
   (path->string
    (build-path (current-directory)
                "bin"
                "java_cup.jar"))
   java-class-separator
   (path->string
    (build-path (current-directory)
                "bin"
                "json.jar"))
   " edu.wpi.margrave.MCommunicator"))

(define java-process-list (process margrave-command-line))

(define-values (input-port output-port process-id err-port ctrl-function) 
  (values (first java-process-list)
          (second java-process-list)
          (third java-process-list)
          (fourth java-process-list)
          (fifth java-process-list)))

;Save pid to kill
(define out-file (open-output-file 
                 (string-append 
                  (path->string (build-path (current-directory)))
                  ".javaIsStarted") #:mode 'text #:exists 'replace))

(display "true" out-file)
(close-output-port out-file)
  
(define (close)
  (begin
    (display "QUIT" output-port)
    (flush-output output-port)
    (close-input-port input-port)
    (close-output-port output-port)
    (close-input-port err-port)
    (ctrl-function 'kill)
    ))
  
;Deprecated
  #;(if windows?
        ;On Windows, need to kill all child process (java.exe). Automatically happens on *nix
        (process (string-append "taskkill /pid " process-id " /t"))
        (ctrl-function 'kill))

;Kill process on exit
(exit:insert-on-callback close)

; Need to
; (0 - tim) (GET REQUEST VECTOR) and XACML/SQS loading   

; (1) Any way to hook drracket exit and close ports + process? 
; (2) hook re-run and close ports + process?
; (3) Need to auto-get/save the reply from server (via m? return the string from stdout. what about stderr?)
; (4) update policy load process (probably some command strings to correct, too)
; [DONE?] (5) build paths using DrRacket's library... paths&dirs handling


;****************************************************************
;;XML



;****************************************************************

; Initialize policy file name.
; This is used by load-policy to assure we don't need to change
; the working directory.
; see normalize-url
(define local-policy-filename ".")

; removeall is remove* in Racket, no need to define it here. Removed. -- TN

; Helper functions for MVocab and MPolicy objects

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
                        (m (string-append "ADD TO " 'vocab " SUBSORT " 'parent " " '(car s)))
                        
                        ; Is this a nested subtype? If so, we must
                        ; deal with s's subtypes.
                        ; Check for list size;
                        ; someone may have used parens without meaning to.
                        (when (> (length s) 1)
                          (add-subtypes-of vocab (car s) (cdr s))))
                      
                      (m (string-append "ADD TO " 'vocab " SUBSORT " 'parent " " 's))))
                listsubs)
      void))


(define (add-constraint vocab typename listrels)
  ; Switch by typename:
  ; (Assume user is passing appropriate number of arguments)
  
  (cond 
    ; typename is a symbol at this point, not a string    
    ((eqv? typename 'disjoint) (m (string-append "ADD TO " vocab " CONSTRAINT DISJOINT " (car listrels) " " (car (cdr listrels)))))
    ((eqv? typename 'disjoint-all) (m (string-append "ADD TO " vocab " CONSTRAINT DISJOINT ALL " (car listrels))))
    ((eqv? typename 'nonempty) (m (string-append "ADD TO " vocab " CONSTRAINT NONEMPTY " (car listrels))))
    ((eqv? typename 'nonempty-all) (m (string-append "ADD TO " vocab " CONSTRAINT NONEMPTY ALL " (car listrels))))
    ((eqv? typename 'singleton) (m (string-append "ADD TO " vocab " CONSTRAINT SINGLETON " (car listrels))))
    ((eqv? typename 'singleton-all) (m (string-append "ADD TO " vocab " CONSTRAINT SINGLETON ALL " (car listrels))))
    ((eqv? typename 'atmostone) (m (string-append "ADD TO " vocab " CONSTRAINT ATMOSTONE " (car listrels))))
    ((eqv? typename 'atmostone-all) (m (string-append "ADD TO " vocab " CONSTRAINT ATMOSTONE ALL " (car listrels))))
    ((eqv? typename 'partial-function) (m (string-append "ADD TO " vocab " CONSTRAINT PARTIAL FUNCTION " (car listrels))))
    ((eqv? typename 'total-function) (m (string-append "ADD TO " vocab " CONSTRAINT TOTAL FUNCTION " (car listrels))))
    ((eqv? typename 'abstract) (m (string-append "ADD TO " vocab " CONSTRAINT ABSTRACT " (car listrels))))
    ((eqv? typename 'abstract-all) (m (string-append "ADD TO " vocab " CONSTRAINT ABSTRACT ALL " (car listrels))))
    ((eqv? typename 'subset) (m (string-append "ADD TO " vocab " CONSTRAINT SUBSET " (car listrels) " " (car (cdr listrels)))))
    (else (display " Error! Unsupported constraint type"))))

; Add a custom relation of type (car listrels) X (car (cdr listrels)) X ...
; Java expects an (unneeded!) arity value
(define (add-predicate vocab predname listrels)
  (m (string-append "ADD TO " vocab " PREDICATE " predname " " (list->string listrels))))

; Sets the target property of a policy object
(define (set-target mypolicy conjlist)
  (m (string-append "SET TARGET FOR POLICY " mypolicy " " conjlist)))

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
      
      (m (string-append "ADD RULE TO " mypolicy " " rulename " " dtype " " conjlist))))

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
     (begin 
       (m (string-append "CREATE VOCABULARY " 'myvocabname)) ; Instantiate a new MVocab object  
       
       ; These sections must be in order.
       
       ; (display "Parsing Types") (newline)       
       ; Types
       (m (string-append "ADD TO " 'myvocabname " SORT " 't...))
       (add-subtypes-of thisvocab 't (list 'subt ...))         
       ... ; for each type/subtype set
       
       ; (display "Parsing Decisions") (newline)
       
       
       ; Decisions               
       (m (string-append "ADD TO " 'myvocabname " DECISION " 'r)) 
       ...
       
       ; (display "Parsing Predicates") (newline)
       ; Predicates
       (add-predicate thisvocab 'pname (list 'prel ...))
       ... ; for each custom predicate
       
       ; (display "Parsing Variables") (newline)       
       ; Request Variables
       (m (string-append "ADD TO " 'myvocabname " REQUESTVAR " 'rvname " " 'rvsort))
       ... ; for each req var
       
       ; (display "Parsing Other Variables") (newline)
       ; Other Variables
       (m (string-append "ADD TO " 'myvocabname " OTHERVAR " 'ovname " " 'ovsort))
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
         (begin (if (< (length mychildren) 1)
                    (m "CREATE POLICY LEAF " 'policyname " " myvocab)
                    (m "CREATE POLICY SET " 'policyname " " myvocab))
                
                (let ((myvarorder (m "GET REQUEST VECTOR " myvocab)))
                  
                  
                  ; Set the policy target (if any)
                  (set-target mypolicy (list 'tconj ...))
                  
                  ; Add the rules to the policy. 'true is dealt with in the back-end.         
                  (add-rule mypolicy myvarorder 'rulename 'dtype (list 'v ...) (list (list->string 'conj) ...))
                  ...
                  
                  ; Set the rule and policy combinator (depending on type)
                  (if (< (length mychildren) 1)
                      (m (string-append "SET RCOMBINE FOR POLICY " mypolicy " " (list->string (list 'rcstr ...))))
                      (m (string-append "SET PCOMBINE FOR POLICY " mypolicy " " (list->string (list 'pcstr ...)))))
                  
                  ; Each child is a Policy
                  (let ((cpol child))
                    (m (string-append "ADD CHILD TO " mypolicy " " cpol))
                    )
                  ...
                  
                  ; Trigger IDB calculation
                  (begin 
                    (m "PREPARE 'mypolicy")
                    
                    
                    mypolicy)))))))) ; Return this policy object (used by policy hierarchy code above)



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

; m
; string -> void
; Runs the given Margrave query or command.
(define (m cmd)
  (begin 
    (display (string-append cmd ";") output-port)
    (flush-output output-port)
    (read-xml input-port)    
    ))

;deprecated
#;(local ((define (helper)
              (let ((next-char (read-char input-port)))
                (if (equal? next-char #\nul)
                    ""
                    (string-append (string next-char) (helper))))))
      (helper))


; !!!
; Need to support these once more. Commands exist, need to route them in java. - TN

; xacml-policy-filename -> MPolicy
; Loads an XACML policy 
;(define (load-xacml-policy fn)
;  ((generic-java-method '|readXACML|) (java-null <MPolicy>) (->jstring fn) (->jstring (string-append my-directory "xacml20.xsd"))))

; sqs-policy-filename -> MPolicy
; Loads an XACML policy 
;(define (load-sqs-policy fn)
;  ((generic-java-method '|loadSQS|) (java-null <MPolicy>) (->jstring fn) ))







; MPolicy -> void
;(define (print-policy-info pol)
;  (m (string-append "PRINT POLICY INFO " pol)))

; MPolicy string string -> void
;(define (assume-disjoint pol sort1 sort2)
;  (m (string-append "ADD TO " pol " DISJOINT ASSUMPTION " sort1 " " sort2)))
;
;(define (assume-subset pol sortchild sortparent)
;  (m (string-append "ADD TO " pol " SUBSET ASSUMPTION " sortchild " " sortparent)))
;
;(define (assume-disjoint-with-prefix pol sortprefix)
;  (m (string-append "ADD TO " pol " DISJOINT PREFIX ASSUMPTION " sortprefix)))

;; MPolicy string
;(define (assume-singleton pol sortname)
;  (m (string-append "ADD TO " pol " SINGLETON ASSUMPTION " sortname)))

; MPolicy string -> MQuery
; Creates an MQuery object representing the query string given being run on the given MPolicy.
;(define (query-policy p str)
;  (m (string-append "QUERY POLICY " p (string-downcase str))))

; MQuery -> int
; Returns the number of solutions the query has.
;(define (count-solutions qry)
;  (->number ((generic-java-method '|countSatisfyingSolutions|) ((generic-java-method '|runQuery|) qry))))

; MQuery list-of-strings -> list-of-strings

; set-idb-output-indexing: MQuery string list-of-strings -> void
; Tells Margrave how to index an IDB for output when tupling a query.
;(define (set-idb-output-indexing qry idbname indexing)
;  (m (string-append "SET IDB OUTPUT INDEXING " qry " " idbname " " indexing)))

; MPolicy MPolicy -> void
; Out-of-the-box change impact function: No need to enter a cumbersome query.
;(define (get-policy-differences pol1 pol2)
;  (m (string-append "COMPARE WITH POLICY " pol1 " " pol2)))

; MPolicy string -> void
;(define (set-policy-name pol name)
;  (m (string-append "SET NAME " pol " " name)))

; MPolicy string -> void
; Using the Margrave query language, state a general assumption that any query involving this policy 
; must respect. (For instance : "(forall s Subject (or (isconflicted=true s) (isconflicted=false s)))")
;(define (add-custom-assumption pol str)
;  ((generic-java-method '|addCustomConstraint|)
;   ((generic-java-field-accessor '|assumptions|) pol)
;   (->jstring str)))

; list-of-MPolicies string -> MQuery
; Same as above, except the MQuery "sees" all policies in the given list. 
; (For instance, given 2 policies, exactly when will one render permit but the other render deny?)
;(define (query-policies plist str)
;  ((generic-java-method '|queryThesePolicies|) (java-null <MQuery>) (->jstring (string-downcase str)) (list->jlistx plist)))


; MQuery query-string -> MQuery
; Returns a newly made query object whose formula is the conjunction of the given query's and the new string.
;(define (refine-query qryobj str)
;  (m (string-append "REFINE QUERY " qryobj " " (string-downcase str))))

; MQuery -> boolean
; Returns whether KodKod returns any satisfying instances for this query.
; (Note the conversion back into scheme booleans.)
;(define (is-query-satisfiable? qry)
;  (->boolean ((generic-java-method '|isQuerySatisfiable|) qry)))

; MQuery -> void
; Pretty prints the query results (if any)
;(define (pretty-print-results qry)
;  (m (string-append "PRETTY PRINT SOLUTIONS " qry )))

; MQuery -> void
; Outputs the query results in block format, with don't care literals treated properly.
; Calling this can be *much* more succinct than pretty-print-results.
;(define (pretty-print-results-condensed qry)
;  (m (string-append "PRETTY PRINT SOLUTIONS CONDENSED " qry )))

; MQuery -> void
; Prints ONE solution.
;(define (pretty-print-one-solution qry)
;  (m (string-append "PRETTY PRINT ONE SOLUTION " qry )))

; MQuery int -> void
; Sets debugging info level. See README.
;(define (set-debug-level qry level)
;  (m (string-append "DEBUG VERBOSITY " qry " " level)))

; MQuery bool -> void
; Activates use of tupling query optimization
;(define (set-tupling qry b)
;  (m (string-append "DO TUPLING " qry " " b)))


; MQuery -> void
; Flags certain IDB names for output. Will provide more output information,
; but may make query execution slower.
;(define (set-idb-output-list qry lst)
;  (m (string-append "IDB NAMED TO OUTPUT " qry " " lst)))

; !!! This is now an argument to the java invocation. pass "debug" after the class name to activate it - TN
;(define (parser-debug b)
;  (m (string-append "DEBUG PARSER " myMargrave " " b)))


; string list-of-MIDBs list-of-strings string -> MCustomIDB
; Creates a custom IDB object. Use in place of Policy objects when running queries.
;(define (make-custom-idb idbname list-of-other-idbsets list-varorder viewstring)
;  (java-new <MCustomIDB> (->jstring idbname) (list->jlistx list-of-other-idbsets) (list->jlist list-varorder) (->jstring viewstring)))

; MQuery int -> void
; Sets the model size ceiling
;(define (set-size-ceiling qry n)
;  (m (string-append "SIZE CEILING " qry " " n)))

; MQuery -> void
; Prints out the Query object's current settings
;(define (print-query-settings qry)
;  (m (string-append "PRINT SETTINGS " qry )))


; !!! We now have sat4j-specific code. Not sure if it can be extended to minisat. Maybe.
; !!!  -- the new code was worthwhile (huge speed up for populated/unpopulated). - TN
; MQuery symbol -> void
; Sets the given MQuery object's satsolver. Allowed:
; 'defaultsat4j 
; 'minisat
;(define (set-query-satsolver qry sym)
;  (cond [(eqv? sym 'minisat) (m (string-append "USE MINISAT " qry ))]
;        [(eqv? sym 'sat4j) (m (string-append "USE SAT4J " qry ))]
;        [else (begin (display "Error! Unknown satsolver type.") (newline))]))
;
;(define (set-default-satsolver sym)
;  (cond [(eqv? sym 'minisat) (m (string-append "USE MINISAT AS DEFAULT" qry ))]
;        [(eqv? sym 'sat4j) (m (string-append "USE SAT4J AS DEFAULT" qry ))]
;        [else (begin (display "Error! Unknown satsolver type.") (newline))]))


; Functions to support easier query string creation

; get-existential-request-prefix
;(define (get-existential-request-prefix pol)
;  (m (string-append "GET EXISTENTIAL REQUEST PREFIX " pol)))

; get-request-prefix-closing
;(define (get-request-prefix-closing pol)
;  (m (string-append "GET REQUEST PREFIX CLOSING " pol)))

; get-request-var-vector
;(define (get-request-var-vector pol)
;  (m (string-append "GET REQUEST VAR VECTOR " pol)))

; get-idbname-list
(define (get-idbname-list pol)
  (m (string-append "GET RULES IN  " pol)))

; get-qualified-idbname-list
; Same as get-idbname-list but includes policy name prefix
(define (get-qualified-idbname-list pol)
  (m (string-append "GET QUALIFIED RULES IN " pol)))

; get-request-var-list
;(define (get-request-var-list pol)
;  (m (string-append "GET REQUEST VAR LIST " pol)))

; get-decision-for-idbname
; Policy String -> String
; Given an idbname, policy will report its decision if a rule, or the empty string otherwise
(define (get-decision-for-rule-idbname policy idbname)
  (m (string-append "GET DECISION FOR " policy " " idbname)))

; rules-with-higher-priority
; Policy String -> List
; Returns a list of rule idb names who have higher priority than the given rule.
; (This doesn't consider whether an overlap is possible, just the rule-ordering
;  given by combining algs.) Names are qualified with policyname:.
;; TODO: Only works for Leaves, not Sets so far.
(define (rule-idbs-with-higher-priority pol rulename)
  (m (string-append "GET HIGHER PRIORITY THAN " pol " " rulename)))


; **************************************************************
; Test case procedures

(define (test desc s1 s2)
  (if (eqv? s1 s2)
      (display (string-append desc ": Passed."))
      (display (string-append desc ": FAILED!")))
  (newline))

; !!! Not provided. Should it be? Can get count at size via COUNT... - TN
;(define (test-model desc qry size exp_sols exp_ceiling)
;  (if (->boolean 
;       (m (string-append "RUN TEST CASE " qry " " size " " exp_sols " " exp_ceiling)))
;      (display (string-append desc ": Passed."))
;      (display (string-append desc ": FAILED!")))
;  (newline))

(define (pause-for-user) 
  (display "======================== Hit enter to continue. ========================") (newline) (newline) (read-char))

;(define-java-class <MFormulaManager> |edu.wpi.margrave.MFormulaManager|)
;(define-java-class <System> |java.lang.System|)

; Should only be used for testing purposes:

;(define (run-gc)
;  ((generic-java-method '|gc|) (java-null <System>)))

;(define (print-cache-info)
;  ((generic-java-method '|printStatistics|) (java-null <MFormulaManager>)))

;(define (get-system-clock-ms)
;  (->number ((generic-java-method '|currentTimeMillis|) (java-null <System>))))
; *************************************************************



