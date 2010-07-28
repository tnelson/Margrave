#lang racket

(require xml)

(provide 
 ; pretty-printer
 pretty-print-info-xml
 get-attribute
 
 ; XML construction commands (used by load-policy in margrave.rkt AND the compiler here)
 ; They are the correct way to construct XML
 xml-make-decision
 xml-make-sort
 xml-make-subsort
 xml-make-request-var
 xml-make-other-var
 xml-make-command
 xml-make-info-command
 xml-make-info-id-command
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
 xml-make-vocab-identifier
 xml-make-constraint
 xml-make-predicate
 xml-make-relations-list
 xml-make-conjunct-chain
 xml-make-rule
 xml-make-load-xacml-command
 xml-make-load-sqs-command
 xml-make-count
 xml-make-count-with-size
 xml-make-size
 xml-make-file-name
 xml-make-schema-file-name
 xml-make-load
 xml-make-load-with-schema
 xml-make-quit
 xml-make-show-populated-command
 xml-make-show-unpopulated-command
 xml-make-forcases)

(define (get-attribute name-of-attribute attribute-list)
  (let ((name (symbol->string (attribute-name (first attribute-list)))))
  (cond [(empty? attribute-list) #f]
        [else
         (if (equal? name-of-attribute name)
             name
             (get-attribute name-of-attribute (rest attribute-list)))])))

;Pass this function a <MARGRAVE-RESPONSE type="sysinfo"> element
(define (pretty-print-info-xml info-element)
  (let ([string-buffer (open-output-string)])
    (local ((define (write s)
              (write-string s string-buffer)))
    (begin
      (write "INFO\n")
      (write (string-append "Type: " (get-attribute "type" (element-attributes info-element)) "\n"))
      (let* ((element-content (element-content info-element))
             (manager-element (second element-content))
             (manager-attributes (element-attributes manager-element))
             (vocab-element (fourth element-content))
             (collections-element (sixth element-content))
             (results-element (eighth element-content)))
        (local ((define (get-manager-attribute s)
                  (get-attribute s manager-attributes)))
        (write (string-append "Atoms: " (get-manager-attribute "atoms")))
          (write (string-append "Conjunctions: " (get-manager-attribute "conjunctions") "\n"))
          (write (string-append "Decls: " (get-manager-attribute "decls") "\n"))
          (write (string-append "Disjunctions: " (get-manager-attribute "disjunctions") "\n"))
          (write (string-append "Multiplicity: " (get-manager-attribute "multiplicity") "\n"))
          (write (string-append "Negations: " (get-manager-attribute "negations") "\n"))
          (write (string-append "Num-variables: " (get-manager-attribute "num-variables") "\n"))
          (write (string-append "Q-exists: " (get-manager-attribute "q-exists") "\n"))
          (write (string-append "Q-forall: " (get-manager-attribute "q-forall") "\n"))
          (write (string-append "Relations: " (get-manager-attribute "relations") "\n"))
          (write (string-append "Total-Formulas: " (get-manager-attribute "total-formulas") "\n"))
          (write (string-append "Total-Reclaimed: " (get-manager-attribute "total-reclaimed") "\n"))
          (write (string-append "Variable-Tuples: " (get-manager-attribute "variable-tuples") "\n")))
    (display (get-output-string string-buffer)))))))

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

(define (xml-make-count id)
  `(COUNT (,id)))

(define (xml-make-count-with-size id size)
  `(COUNT (,id ,size)))

(define (xml-make-size size)
  `(size ,size))


(define (xml-make-file-name file-name)
  `(file-name ,file-name))

(define (xml-make-schema-file-name schema-file-name)
  `(schema-file-name ,schema-file-name))

(define (xml-make-load file-name)
  `(LOAD (,file-name)))

(define (xml-make-load-with-schema file-name schema-file-name)
  `(LOAD (,file-name ,schema-file-name)))
    
(define (xml-make-load-xacml-command load)
  (xml-make-command "LOAD-XACML-POLICY" (list load)))

(define (xml-make-load-sqs-command load)
  (xml-make-command "LOAD-SQS-POLICY" (list load)))

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
  `(IDBOUTPUT ,@list-of-atomic-formulas))

(define (xml-make-show-populated-command id childlist)
  (xml-make-command "SHOW" (list `(SHOW ((type "POPULATED") ,id) ,@childlist))))

(define (xml-make-show-unpopulated-command id childlist)
  (xml-make-command "SHOW" (list `(SHOW ((type "UNPOPULATED") ,id) ,@childlist))))

(define (xml-make-forcases the-cases)
  `(FORCASES ,@the-cases))

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
;  (printf "~a ~a ~n" collName relName)
  (if (empty? xml-identifier-list)
      `(ATOMIC-FORMULA-Y ((collection-name ,collName) (relation-name ,relName)))
      `(ATOMIC-FORMULA-Y ((collection-name ,collName) (relation-name ,relName)) ,xml-identifier-list))) 

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


(define (xml-make-quit)
  (xml-make-command "QUIT" empty))



;Takes a command type (string) and a list of children (x-exprs) and returns a formatted margrave-command as an XML string
(define (xml-make-command command-type list-of-children)
  (xexpr->string
   `(MARGRAVE-COMMAND ((type ,command-type)) ,@list-of-children)))