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
; Modifications by TN and VS, Summer 2010

#lang racket

(require racket/system)
(require framework)
(require xml)



; **********************************************************
; TODO
; **********************************************************
; * (Tim) XACML/SQS loading   
; * (Tim) colons in some sort names created by IOS parser. Need to quote them in query text or change parser to allow for it.
; * Close java gracefully and automatically. 
; * Test cases
; * More helper functions (see supnew.rkt as a use case)
; * More Human-readable output from the XML. 

; Next phase 
; First step in next phase is to move the query-language parser to DrRacket and start _sending_ XML to java.
; (Also, search for a way to terminate that process cleanly in tool/language-level docs.)
; **********************************************************



; todo: more later
(provide stop-margrave-engine
         start-margrave-engine
         m
         pretty-print-model
         load-policy
         get-idbname-list
         get-qualified-idbname-list
         get-decision-for-rule-idbname
         mxout
         pause-for-user
                  
         xml-make-info-command
         xml-make-rename-command
         xml-make-get-command
         xml-make-atomic-formula-n
         xml-make-atomic-formula-y
         xml-make-explore-command
         xml-make-is-possible-command
         xml-make-debug
         xml-make-publish
         xml-make-tupling
         xml-make-ceiling
         xml-make-is-guaranteed-command
         xml-make-identifiers-list
         xml-make-type
         xml-make-id
         xml-make-idbout
         xml-make-under
         xml-make-policy-identifier
         xml-make-vocab-identifier)

; We use eval to load policies and vocabularies, and the call is in the definitions window.
; Thus we need to provide a namespace for eval, or it won't know what to do with the Policy
; and PolicyVocab syntax.
(define-namespace-anchor the-margrave-namespace-anchor)
(define the-margrave-namespace (namespace-anchor->namespace the-margrave-namespace-anchor))

;****************************************************************
;;Java Connection

(define windows? (equal? 'windows (system-path-convention-type)))

(define java-class-separator
  (if windows?
      ";"
      ":"))

(define margrave-command-line
  (string-append
   "java -cp "
   ;For testing
   (path->string
    (build-path (current-directory)
                "bin"))
   #;(path->string
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

; Initial values
(define java-process-list #f)
(define input-port #f)
(define output-port #f)
(define process-id #f)
(define err-port #f)
(define ctrl-function #f)

(define (start-margrave-engine)
  (if (eq? java-process-list #f)
      (begin
        (set! java-process-list (process margrave-command-line))
        (set! input-port (first java-process-list))
        (set! output-port (second java-process-list))
        (set! process-id (third java-process-list))
        (set! err-port (fourth java-process-list))
        (set! ctrl-function (fifth java-process-list))
        #t)
      #f))

(define (stop-margrave-engine)
  (display "QUIT;" output-port) ; semicolon is necessary
  (flush-output output-port)    
  (close-input-port input-port)
  (close-output-port output-port)
  (close-input-port err-port)
  (ctrl-function 'kill)
  
  ; allow restart of the engine
  (set! java-process-list #f)
  (set! input-port #f)
  (set! output-port #f)
  (set! process-id #f)
  (set! err-port #f)
  (set! ctrl-function #f)) 


; exit-handler doesn't get called when exiting DrRacket or when hitting Run, only when explicitly calling (exit x)
; (exit:insert-on-callback) doesn't work for some reason either
;Kill process on exit
; (exit:insert-on-callback stop-margrave-engine)


;****************************************************************
;;XML

; Get list of child elements with name = name-symbol.
; Element -> Symbol -> List(Element)
(define (get-element-children-named ele name-symbol)
  (filter (lambda (con) (and (element? con) (equal? (element-name con) name-symbol)))
          (element-content ele)))

; Get value for attribute name-symbol of element ele
; Element -> Symbol -> String
(define (get-attribute-value ele name-symbol)
  (attribute-value (first (filter (lambda (attr) (equal? (attribute-name attr) name-symbol))
                                  (element-attributes ele)))))


; Get the response type of a MARGRAVE-RESPONSE element:
; Document -> String
(define (get-response-type doc)
  (get-attribute-value (document-element doc) 'type))


; Document -> Boolean
(define (response-is-success? doc)
  (equal? (get-response-type doc)
          "success"))
(define (response-is-error? doc)
  (equal? (get-response-type doc)
          "error"))
(define (response-is-exception? doc)
  (equal? (get-response-type doc)
          "exception"))

; Fetch various error properties
; Document -> String
(define (get-response-error-type doc)
  (get-attribute-value (first (get-element-children-named (document-element doc) 'ERROR)) 'type))
(define (get-response-error-subtype doc)
  (get-attribute-value (first (get-element-children-named (document-element doc) 'ERROR)) 'subtype))
(define (get-response-error-descriptor doc)
  (pcdata-string (first (element-content (first (get-element-children-named (document-element doc) 'ERROR))))))


; Placeholder
(define (pretty-xml doc)
  (xexpr->string (xml->xexpr (document-element doc))))

(define (mx cmd)
  (pretty-xml (m cmd)))

(define (mxout cmd)
  (display (mx cmd)) (newline))

;****************************************************************

; Initialize policy file name.
; This is used by load-policy to assure we don't need to change
; the working directory.
; see normalize-url
(define local-policy-filename ".")

; removeall is remove* in Racket, no need to define it here. Removed. -- TN

; symbol or string -> string
; Returns the argument, quoted, as a string.
(define (symbol->quoted-string arg)
  (if (symbol? arg)
      (string-append "\"" (symbol->string arg)"\"")
      (string-append "\"" arg "\"")))


;****************************************************************
;;Pretty Printing returned XML

(define testXML (read-xml (open-input-string
                           "<MARGRAVE-RESPONSE type=\"model\">
<MODEL size=\"3\">
<RELATION arity=\"1\" name=\"author\">
<TUPLE>
<ATOM>Atom0</ATOM>
</TUPLE>
</RELATION>
<RELATION arity=\"1\" name=\"paper\">
<TUPLE>
<ATOM>Atom1</ATOM>
</TUPLE>
</RELATION>
<RELATION arity=\"1\" name=\"subject\">
<TUPLE>
<ATOM>Atom0</ATOM>
</TUPLE>
</RELATION>
<RELATION arity=\"1\" name=\"resource\">
<TUPLE>
<ATOM>Atom1</ATOM>
</TUPLE>
</RELATION>
<RELATION arity=\"1\" name=\"submitreview\" />
<RELATION arity=\"1\" name=\"action\">
<TUPLE>
<ATOM>Atom2</ATOM>
</TUPLE>
</RELATION>
<RELATION arity=\"1\" name=\"reviewer\" />
<RELATION arity=\"1\" name=\"readpaper\">
<TUPLE>
<ATOM>Atom2</ATOM>
</TUPLE>
</RELATION>
<RELATION arity=\"1\" name=\"review\" />
<RELATION arity=\"1\" name=\"submitpaper\" />
<RELATION arity=\"2\" name=\"conflicted\">
<TUPLE>
<ATOM>Atom0</ATOM>
<ATOM>Atom1</ATOM>
</TUPLE>
</RELATION>
<RELATION arity=\"2\" name=\"assigned\" />
<RELATION arity=\"1\" name=\"$r\">
<TUPLE>
<ATOM>Atom1</ATOM>
</TUPLE>
</RELATION>
<RELATION arity=\"1\" name=\"$a\">
<TUPLE>
<ATOM>Atom2</ATOM>
</TUPLE>
</RELATION>
<RELATION arity=\"1\" name=\"$s\">
<TUPLE>
<ATOM>Atom0</ATOM>
</TUPLE>
</RELATION>
</MODEL>
</MARGRAVE-RESPONSE>")))

;name is the name of the atom, such as "s" or "r" (doesn't include the $), and list of types is a list of types (which are really predicates that have only one atom in (predicate-list-of-atoms))
(define-struct atom (name list-of-types) #:mutable)

;Note that a type is also a predicate, but with only one atom.
(define-struct predicate (name arity list-of-atoms) #:mutable)

;Maps strings (such as "s") to their corresponding atoms
(define atom-hash (make-hash))

;Maps name of predicates (strings) to their corresponding predicate structs
(define predicate-hash (make-hash))

(define model-size 0)

(define (pretty-print-next-model id) 
  "")

;Takes a document with <MODEL> as its outer type
(define (pretty-print-model xml-model)
  ;First, go through the XML and update the 2 hashes. Then print them out.
  (local [(define (helper content) ;content is a list of alternating pcdatas and RELATION elements
            (cond [(empty? content) void]
                  [(pcdata? (first content)) (helper (rest content))]
                  [(element? (first content))
                   (let* ((relation (first content))
                          (relation-arity (string->number (attribute-value (first  (element-attributes relation)))))
                          (relation-name  (attribute-value (second (element-attributes relation)))))
                     (begin
                       (when (not (hash-ref predicate-hash relation-name #f)) ;if the relation (predicate) doesn't exist in the hash yet, create it
                         (hash-set! predicate-hash relation-name (make-predicate relation-name relation-arity empty)))
                       (let ((predicate-struct (hash-ref predicate-hash relation-name))) ;should definitely exist, since we just created it if it didn't
                         (when (not (empty? (element-content relation))) ;if there is at least one atom that satisfies this relation
                           (let ((tuple-content (element-content (second (element-content relation)))))
                             (local [(define (parse-tuple t-cont)
                                       (cond [(empty? t-cont) void]
                                             [(pcdata? (first t-cont)) (parse-tuple (rest t-cont))]
                                             [(element? (first t-cont))
                                              (let* ((atom (first t-cont))
                                                     (atom-name (pcdata-string (first (element-content atom)))))
                                                (begin 
                                                  (when (not (hash-ref atom-hash atom-name #f)) ;if the atom doesn't exist in the hash yet, create it
                                                    (hash-set! atom-hash atom-name (make-atom atom-name empty)))
                                                  (let ((atom-struct (hash-ref atom-hash atom-name))) ;should definitely exist, since we just created it if it didn't
                                                    (if (equal? (string-ref relation-name 0) #\$)
                                                        (set-atom-name! atom-struct (make-string 1 (string-ref relation-name 1))) ;Have to turn the char into a string
                                                        (if (=  relation-arity 1) ;If relation is a type
                                                            (begin 
                                                              (set-atom-list-of-types! atom-struct (cons relation-name (atom-list-of-types atom-struct)))
                                                              (set-predicate-list-of-atoms! predicate-struct (cons atom-struct (predicate-list-of-atoms predicate-struct))))
                                                            (begin (set-predicate-list-of-atoms! predicate-struct (cons atom-struct (predicate-list-of-atoms predicate-struct)))
                                                                   (parse-tuple (rest t-cont))))))))]
                                             [else "Error in pretty-print-model!"]))]
                               (parse-tuple tuple-content)))))
                       (helper (rest content))))]
                  [else "Error in pretty-print-model!!"]))]
    (begin (set! atom-hash (make-hash)) ;First reset the hashes
           (set! predicate-hash (make-hash))
           (set! model-size (attribute-value (first (element-attributes xml-model))))
           (helper (element-content xml-model))
           (display (string-from-hash)))))

;Returns a string to display based on atom-hash and predicate-hash
(define (string-from-hash)
  (local [(define (atom-helper hash-pos)
            (cond [(false? hash-pos) ""]
                  [else (let ((atom (hash-iterate-value atom-hash hash-pos)))
                          (string-append
                           (atom-name atom)
                           ": "
                           (foldl (λ(type rest) (string-append type " " rest)) "" (atom-list-of-types atom))
                           "\n"
                           (atom-helper (hash-iterate-next atom-hash hash-pos))))]))
          (define (predicate-helper hash-pos)
            (cond [(false? hash-pos) ""]
                  [else (let ((predicate (hash-iterate-value predicate-hash hash-pos)))
                          (if (= (predicate-arity predicate) 1) ;If type, continue, otherwise print
                              (predicate-helper (hash-iterate-next predicate-hash hash-pos))
                              (string-append
                               (predicate-name predicate)
                               " = {["
                               (foldl (λ(atom rest) (string-append (atom-name atom)
                                                                   (if (not (equal? rest ""))
                                                                       ", "
                                                                       "") 
                                                                   rest)) "" (predicate-list-of-atoms predicate))
                               "]}"
                               "\n"
                               (predicate-helper (hash-iterate-next predicate-hash hash-pos)))))]))]
    (string-append "********* SOLUTION FOUND at size = " model-size " ******************\n"
                   (atom-helper (hash-iterate-first atom-hash))
                   (predicate-helper (hash-iterate-first predicate-hash)))))

(define test-model-xml (second (element-content (document-element testXML))))



;****************************************************************
; Helper functions 

; listsubs contains a list of the subsorts for this sort. 
; However, it may be nested: subsorts may themselves have subsorts.
(define (add-subtypes-of vocab parent listsubs)  
  ; listsubs may be empty -- if so, do nothing (we already added parent)
  (when (> (length listsubs) 0)            
    (for-each (lambda (s) 
                ; Is this a sort with subsorts itself?
                (if (list? s)                       
                    (begin
                      (display parent)
                      ; Add subtype relationship between parent and s
                      (m (xml-make-command "ADD" (list (xml-make-vocab-identifier vocab) (xml-make-subsort parent (symbol->string (car s))))))
                      
                      ; Is this a nested subtype? If so, we must
                      ; deal with s's subtypes.
                      
                      ; Check for list size;
                      ; someone may have used parens without meaning to.
                      (when (> (length s) 1)
                        (add-subtypes-of vocab (symbol->string (car s)) (cdr s))))
                    
                    ; Bottom of sort tree. 
                    (m (xml-make-command "ADD" (list (xml-make-vocab-identifier vocab) (xml-make-subsort parent (symbol->string s)))))))
              listsubs)))


(define (add-constraint vocab typename listrels)
  ; Switch by typename:
  ; (Assume user is passing appropriate number of arguments)
  
  (cond 
    ; typename is a symbol at this point, not a string    
    ((eqv? typename 'disjoint) (xml-make-command "ADD" (list (xml-make-vocab-identifier vocab) (xml-make-constraint "DISJOINT" (list (car listrels) (car (cdr listrels)))))))
    ((eqv? typename 'disjoint-all) (xml-make-command "ADD" (list (xml-make-vocab-identifier vocab) (xml-make-constraint "DISJOINT-ALL" (list (car listrels) )))))
    ((eqv? typename 'nonempty) (xml-make-command "ADD" (list (xml-make-vocab-identifier vocab) (xml-make-constraint "NONEMPTY" (list (car listrels) )))))
    ((eqv? typename 'nonempty-all) (xml-make-command "ADD" (list (xml-make-vocab-identifier vocab) (xml-make-constraint "NONEMPTY-ALL" (list (car listrels) )))))
    ((eqv? typename 'singleton) (xml-make-command "ADD" (list (xml-make-vocab-identifier vocab) (xml-make-constraint "SINGLETON" (list (car listrels) )))))
    ((eqv? typename 'singleton-all) (xml-make-command "ADD" (list (xml-make-vocab-identifier vocab) (xml-make-constraint "SINGLETON-ALL" (list (car listrels) )))))
    ((eqv? typename 'atmostone) (xml-make-command "ADD" (list (xml-make-vocab-identifier vocab) (xml-make-constraint "ATMOSTONE" (list (car listrels) )))))
    ((eqv? typename 'atmostone-all) (xml-make-command "ADD" (list (xml-make-vocab-identifier vocab) (xml-make-constraint "ATMOSTONE-ALL" (list (car listrels) )))))
    ((eqv? typename 'partial-function) (xml-make-command "ADD" (list (xml-make-vocab-identifier vocab) (xml-make-constraint "PARTIALFUNCTION" (list (car listrels) )))))
    ((eqv? typename 'total-function) (xml-make-command "ADD" (list (xml-make-vocab-identifier vocab) (xml-make-constraint "TOTALFUNCTION" (list (car listrels) )))))
    ((eqv? typename 'abstract) (xml-make-command "ADD" (list (xml-make-vocab-identifier vocab) (xml-make-constraint "ABSTRACT" (list (car listrels) )))))
    ((eqv? typename 'abstract-all) (xml-make-command "ADD" (list (xml-make-vocab-identifier vocab) (xml-make-constraint "ABSTRACT-ALL" (list (car listrels) )))))
    ((eqv? typename 'subset) (xml-make-command "ADD" (list (xml-make-vocab-identifier vocab) (xml-make-constraint "SUBSET" (list (car listrels) (car (cdr listrels)))))))
    (else (printf " Error! Unsupported constraint type~n"))))

; May be a list, may not be a list
(define (fold-append-with-spaces posslist)
  (if (list? posslist)
      (foldr (lambda (s t) 
               (cond
                 [(and (symbol? s) (symbol? t)) (string-append (symbol->string s) " " (symbol->string t))]
                 [(and (symbol? s) (string=? t "")) (symbol->string s)] 
                 [(symbol? s) (string-append (symbol->string s) " " t)] 
                 [(symbol? t) (string-append s " " (symbol->string t))] 
                 [(string=? t "") s]
                 [else (string-append s " " t)]))
             ""
             posslist)
      (if (symbol? posslist)
          (symbol->string posslist)
          posslist)))

(define (fold-append-with-spaces-quotes posslist)
  (fold-append-with-spaces (if (list? posslist)
                               (map symbol->quoted-string posslist)
                               posslist)))

;;These functions return x-exprs for parts of command
(define (xml-make-info-id id)
  `(INFO ((id ,id))))

(define (xml-make-info-id-command id)
  (xml-make-command "INFO" (list (xml-make-info-id id))))

(define (xml-make-info)
  `(INFO))

(define (xml-make-info-command)
  (xml-make-command "INFO" (list (xml-make-info))))

(define (xml-make-policy-identifier policy-name)
  `(POLICY-IDENTIFIER ((pname ,policy-name))))

(define (xml-make-vocab-identifier vocab-name)
  `(VOCAB-IDENTIFIER ((vname ,vocab-name))))

(define (xml-make-parent-identifier parent-name)
  `(PARENT-IDENTIFIER ((name ,parent-name))))

(define (xml-make-child-identifier child-name)
  `(VOCAB-IDENTIFIER ((name ,child-name))))

(define (xml-make-predicate pred-name)
  `(PREDICATE ((name ,pred-name))))

(define (xml-make-rule rule-name dtype rule-list)
  `(RULE ((name ,rule-name)) ,(xml-make-decision-type dtype) ,(xml-make-rule-list rule-list)))

;rule-list is of the form ((!Conflicted s r) (ReadPaper a) (Paper r)), or true
(define (xml-make-rule-list rule-list)
  (if (equal? 'true (first rule-list))
      `(RELATIONS) 
      `(RELATIONS ,@(map (λ(relation)
                           (let* ((relation-name (symbol->string (first relation)))
                                  (negation? (starts-with-exclamation relation-name)))
                             `(RELATION ((name ,(if negation? ;Take out the exclamation point
                                                    (substring relation-name 1)
                                                    relation-name))
                                         (sign ,(if negation?
                                                    "false"
                                                    "true")))
                                        ,(xml-make-identifiers-list (rest relation)))))
                         rule-list))))

(define (starts-with-exclamation string)
  (if (equal? "!" (substring string 0 1))
      true
      false))

(define (xml-make-decision-type decision-type)
  `(DECISION-TYPE ((type ,decision-type))))

(define (xml-make-decision decision)
  `(DECISION ((name ,decision))))

(define (xml-make-sort sort-name)
  `(SORT ((name ,sort-name))))

(define (xml-make-subsort parent child)
  `(SUBSORT ((parent ,parent) (child ,child))))


(define (xml-make-request-var rvname rvsort)
  `(REQUESTVAR ((name ,rvname) (sort ,rvsort))))

(define (xml-make-other-var ovname ovsort)
  `(OTHERVAR ((name ,ovname) (sort ,ovsort))))

(define (xml-make-constraint constraint-type list-of-relations)
  `(CONSTRAINT ((type ,constraint-type)) ,(xml-make-relations-list list-of-relations)))

(define (xml-make-rename id1 id2)
  `(RENAME ((id1 ,id1) (id2 ,id2))))

(define (xml-make-rename-command id1 id2)
  (xml-make-command "RENAME" (list (xml-make-rename id1 id2))))

(define (xml-make-type type)
  `(type ,type))

(define (xml-make-id id)
  `(id ,id))

(define (xml-make-get type id)
  `(SHOW (,type ,id)))

(define (xml-make-get-command type id)
  (xml-make-command "SHOW" (list (xml-make-get type id))))

(define (xml-make-under policy)
  `(UNDER ,policy))

(define (xml-make-is-possible id)
  `(IS-POSSIBLE ((id ,id))))

(define (xml-make-is-possible-command id)
  (xml-make-command "IS-POSSIBLE" (list (xml-make-is-possible id))))

(define (xml-make-is-guaranteed id)
  `(IS-GUARANTEED ((id ,id))))

(define (xml-make-is-guaranteed-command id)
  (xml-make-command "IS-GUARANTEED" (list (xml-make-is-guaranteed id))))

(define (xml-make-publish list-of-identifiers)
  `(PUBLISH ,list-of-identifiers))

(define (xml-make-idbout list-of-atomic-formulas)
  `(IDB ,@list-of-atomic-formulas))

(define (xml-make-tupling) ;Just defaults to true, if you don't want tupling don't include
  `(TUPLING ((value "true")))) ;Value isn't actually used right now. Perhaps useless?

(define (xml-make-debug debug-level)
  `(DEBUG ((debug-level ,debug-level))))

(define (xml-make-ceiling ceiling-level)
  `(CEILING ((ceiling-level ,ceiling-level))))

;Atomic Formulas
(define (xml-make-atomic-formula-n relName xml-identifier-list)
  `(ATOMIC-FORMULA-N ((relation-name ,relName)) ,xml-identifier-list))  

(define (xml-make-atomic-formula-y collName relName xml-identifier-list)
  `(ATOMIC-FORMULA-Y ((collection-name ,collName) (relation-name ,relName)) ,xml-identifier-list)) 

;;EXPLORE
;Makes an xexpr for a list of atomic formulas (can be of size 1). symbol can be "and" or "or"
(define (xml-make-atomic-formulas-list symbol list-of-atomic-formulas)
  (if (equal? 1 (length list-of-atomic-formulas))
                  (first list-of-atomic-formulas)
                  (foldr (λ(atomic-formula rest)
                           (list symbol atomic-formula rest))
                         (first list-of-atomic-formulas)
                         (rest list-of-atomic-formulas))))

;Atomic Formulas must already be xexprs
(define (xml-make-explore list-of-atomic-formulas list-of-modifiers)
  `(EXPLORE (CONDITION 
             ,(if (equal? 1 (length list-of-atomic-formulas))
                  (first list-of-atomic-formulas)
                  (foldl (λ(atomic-formula rest)
                           `(AND ,atomic-formula ,rest))
                         (first list-of-atomic-formulas)
                         (rest list-of-atomic-formulas))))
            ,@list-of-modifiers))

(define (xml-make-explore-command list-of-atomic-formulas list-of-modifiers)
  (xml-make-command "EXPLORE" (list (xml-make-explore list-of-atomic-formulas list-of-modifiers))))

;;LISTS
(define (xml-make-generic-list list-name element-name attribute-name list-of-attribute-values)
  `(,list-name
    ,@(map (λ(attribute-value)
             `(,element-name ((,attribute-name ,(if (symbol? attribute-value)
                                                    (symbol->string attribute-value)
                                                    attribute-value)))))
           list-of-attribute-values)))

;algs-list is a list of strings decribing the combine algorithms
(define (xml-make-combine-algs algs-list)
  (xml-make-generic-list 'COMBINE-ALGS 'COMBINE-ALG 'desc algs-list))

(define (xml-make-conjunct-chain conjs-list)
  (xml-make-generic-list 'CONJUNCT-CHAIN 'CONJUNCT 'name conjs-list))

(define (xml-make-identifiers-list idents-list)
  (xml-make-generic-list 'IDENTIFIERS 'IDENTIFIER 'name idents-list))

(define (xml-make-relations-list relations-list)
  (xml-make-generic-list 'RELATIONS 'RELATION 'name relations-list))



;Takes a command type (string) and a list of children (x-exprs) and returns a formatted margrave-command as an XML string
(define (xml-make-command command-type list-of-children)
  (xexpr->string
   `(MARGRAVE-COMMAND ((type ,command-type)) ,@list-of-children)))


; Add a custom relation of type (car listrels) X (car (cdr listrels)) X ...
; Java expects an (unneeded!) arity value
(define (add-predicate vocab predname listrels)
  (m (xml-make-command "ADD" (list (xml-make-vocab-identifier vocab) (xml-make-predicate predname) (xml-make-relations-list listrels)))))

; Sets the target property of a policy object
(define (set-target mypolicy conjlist)
  (m (xml-make-command "SET TARGET FOR POLICY" (list (xml-make-policy-identifier mypolicy) (xml-make-conjunct-chain conjlist)))))

(define (wrap-list-parens lst)
  (fold-append-with-spaces (map (lambda (str) (string-append "(" str ")")) lst)))

; !!! TODO This will be much nicer once we're sending XML         

; Add a rule of the form rulename = (dtype reqvars) :- conjlist
(define (add-rule mypolicy 
                  ;myvarorder 
                  rulename dtype reqvars conjlist)
  ;  (if (not (string=? myvarorder 
  ;                     (apply string-append (map 
  ;                                           (lambda (x) (string-append x " ")) ; leave trailing whitespace in java api too.
  ;                                           reqvars)))) 
  ;      (begin (display "Error: Unable to add rule. Variable ordering ")
  ;             (display reqvars)
  ;             (newline)
  ;             (display "did not agree with vocabulary, which expected ")
  ;             (display myvarorder) 
  ;             (display ".")
  ;             (newline))
  
  (m (xml-make-command "ADD" (list (xml-make-policy-identifier mypolicy) (xml-make-rule rulename dtype conjlist))))) ;(wrap-list-parens conjlist)

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
       
       ; Instantiate a new MVocab object
       ; If already created, wipe and start over.
       (let ([ create-reply-doc 
               (m (xml-make-command "CREATE VOCABULARY" (list (xml-make-vocab-identifier (symbol->string 'myvocabname)))))])
         (when (response-is-error? create-reply-doc)
           (begin 
             (m (xml-make-command "DELETE VOCABULARY" (list (xml-make-vocab-identifier (symbol->string 'myvocabname)))))
             (m (xml-make-command "CREATE VOCABULARY" (list (xml-make-vocab-identifier (symbol->string 'myvocabname))))))))
       
       ; These sections must be in order.                     
       ; Types
       (begin
         (m (xml-make-command "ADD" (list (xml-make-vocab-identifier (symbol->string 'myvocabname)) (xml-make-sort (symbol->string 't)))))
         (add-subtypes-of (symbol->string 'myvocabname) (symbol->string 't) (list 'subt ...))         
         )
       ... ; for each type/subtype set
       
       ; Decisions               
       (m (xml-make-command "ADD" (list (xml-make-vocab-identifier (symbol->string 'myvocabname)) (xml-make-decision (symbol->string 'r))))) 
       ...
       
       ; Predicates
       (add-predicate (symbol->string 'myvocabname) (symbol->string 'pname) (list (symbol->string 'prel) ...))
       ... ; for each custom predicate
       
       ; Request Variables
       (m (xml-make-command "ADD" (list (xml-make-vocab-identifier (symbol->string 'myvocabname)) (xml-make-request-var (symbol->string 'rvname) (symbol->string 'rvsort)))))
       ... ; for each req var
       
       ; Other Variables
       (m (xml-make-command "ADD" (list (xml-make-vocab-identifier (symbol->string 'myvocabname)) (xml-make-other-var (symbol->string 'ovname) (symbol->string 'ovsort)))))
       ... ; for each oth var
       
       ; Constraints
       (add-constraint (symbol->string 'myvocabname) 'ctype (list (symbol->string 'crel) ...))       
       ... ; for each constraint
       
       ; Return the object for use by the policy macro
       'myvocabname)))) 



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
             
             ; !!! TODO: Is there a safer alternative to eval here? Look into sandboxing?
             (myvocab                            
              (symbol->string (call-with-input-file 
                                  (build-path (path-only local-policy-filename) 
                                              (string-append (symbol->string 'vocabname) ".v"))
                                (lambda (in-port) (eval (read in-port) the-margrave-namespace))))))
         
         ; In SISC, the above was: 
         ;                       (eval (read (open-input-file
         ;                           (normalize-url 
         ;                            ; Make sure we look in the correct directory!
         ;                            local-policy-filename 
         ;                            (string-append (symbol->string 'vocabname) ".v")))))))
         
         
         
         (begin (if (< (length mychildren) 1)
                    (m (xml-make-command "CREATE POLICY LEAF" (list (xml-make-policy-identifier (symbol->string 'policyname)) (xml-make-vocab-identifier myvocab))))
                    (m (xml-make-command "CREATE POLICY SET" (list (xml-make-policy-identifier (symbol->string 'policyname)) (xml-make-vocab-identifier myvocab)))))
                
                
                ;; !!! TODO This was an ugly hack to get around a problem with the .p language.
                ; Either fix the language, or fix the hack.
                ;(let ((myvarorder (m (string-append "GET REQUEST VECTOR " myvocab))))
                
                
                ; Set the policy target (if any)
                (let ((the-target (list (symbol->string 'tconj) ...)))
                  (when (> (length the-target) 0)
                    (set-target (symbol->string 'policyname) the-target)))
                
                ; Add the rules to the policy. 'true is dealt with in the back-end.         
                (add-rule (symbol->string 'policyname)
                          ;myvarorder 
                          (symbol->string 'rulename) (symbol->string 'dtype) (list (symbol->string 'v) ...) (list 'conj ...))
                ...
                
                ; Set the rule and policy combinator (depending on type)
                (if (< (length mychildren) 1)
                    (m (xml-make-command "SET RCOMBINE FOR POLICY" (list (xml-make-policy-identifier (symbol->string 'policyname)) (xml-make-identifiers-list (list 'rcstr ...)))))
                    (m (xml-make-command "SET PCOMBINE FOR POLICY" (list (xml-make-policy-identifier (symbol->string 'policyname)) (xml-make-identifiers-list (list 'pcstr ...))))))
                
                ;; !!! TODO: confirm this works. are we loading the sub-policy properly?
                
                ; Each child is a Policy
                (let ((cpol child))
                  (m (xml-make-command "ADD" (list (xml-make-policy-identifier (symbol->string 'policyname)) (xml-make-policy-identifier cpol))))
                  )
                ...
                
                ; Trigger IDB calculation
                (begin 
                  (m (xml-make-command "PREPARE" (list (xml-make-policy-identifier (symbol->string 'policyname)))))
                  
                  
                  (symbol->string 'policyname))   ; close paren for above GET REQUEST VECTOR commented out )
                )))))) ; Return this policy object (used by policy hierarchy code above)



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
  
  ; !!! TODO Check whether case-sensitivity problems remain in DrRacket
  
  ;  (case-sensitive #t)
  ;  (display (read (open-input-file fn))) (newline)
  ;  (case-sensitive #f)  
  ; (display "*** ") (display fn) (newline)
  ;; Macro returns a func 
  ;; Potential security issues here, calling eval on arbitrary code that we "promise" is an
  ;; innocent policy definition. Is there a fix for this?
  ; (case-sensitive #t)
  (let* ([file-port (open-input-file fn)]
         [pol ((eval (read file-port) the-margrave-namespace) fn)])
    ; (case-sensitive #f)
    
    ; don't keep the handle open! call-with-input-file would be better here.
    (close-input-port file-port)
    
    ; return the policy identifier
    pol))

; m
; string -> document or #f
; Runs the given Margrave query or command. Returns #f if the engine has not been started.
; Uses *buffered* string ports to avoid overhead due to excessive concatenation.
(define (m cmd)
  (if (equal? java-process-list #f) 
      (begin
        (printf "Could not send Margrave command because engine was not started. Call the start-margrave-engine function first.~n")
        #f)
      (begin 
        ;(printf "~a;~n" cmd)
        (display (string-append cmd ";") output-port)
        (flush-output output-port)        
        
        (let ([command-buffer (open-output-string)]
              [error-buffer (open-output-string)]) 
          (local ((define (clear-error)
                    (when (char-ready? err-port) ; Is there a character waiting? If so, read it.
                      (let ([next-char (read-char err-port)])                                                
                        (write-string (string next-char) error-buffer)
                        (clear-error))))
                  
                  (define (helper)
                    (let ([next-char (read-char input-port)])
                      (when (not (or (equal? next-char #\nul)
                                     (equal? next-char eof))) ; Read until we see a NUL.
                        (begin
                          (write-string (string next-char) command-buffer)
                          (helper))))))
            
            ; Populate the buffered ports
            (clear-error)
            (helper)
            
            ; Handle the results
            (let ([result (get-output-string command-buffer)]
                  [error-str (get-output-string error-buffer)])
              (when (> (string-length error-str) 0)
                (printf "Additional ERROR information received:~n ~a~n" error-str))
              (begin
                (display result)
                (read-xml (open-input-string result))))))))  )






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







; !!! This is now an argument to the java invocation. pass "debug" after the class name to activate it - TN
;(define (parser-debug b)
;  (m (string-append "DEBUG PARSER " myMargrave " " b)))



; !!! We now have sat4j-specific code. Not sure if it can be extended to minisat. Maybe.
; !!!  -- the new code was worthwhile (huge speed up for populated/unpopulated). - TN

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
  (printf "======================== Hit enter to continue. ========================~n~n")
  (read-char))


