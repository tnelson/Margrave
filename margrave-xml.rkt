#lang racket

(require xml)

(provide 
 stop-margrave-engine
 start-margrave-engine
 mtext
 m
 mxtextout
 get-idbname-list
 get-qualified-idbname-list
 get-decision-for-rule-idbname
 pretty-print-model
 pretty-print-response-xml
 get-attribute-value
 
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

;****************************************************************
;;Java Connection

(define windows? (equal? 'windows (system-path-convention-type)))

(define java-class-separator
  (if windows?
      ";"
      ":"))    

; Initial values
(define java-process-list #f)
(define input-port #f)
(define output-port #f)
(define process-id #f)
(define err-port #f)
(define ctrl-function #f)
(define margrave-home-path (current-directory))

; Home-path is the location of the margrave.rkt, read.rkt, etc. files.
; If not passed, will use (current-directory).
(define (start-margrave-engine (home-path margrave-home-path))
  (if (eq? java-process-list #f)
      (let ([ margrave-command-line
              (string-append
               "java -cp "
               
               ;For testing, use the .class files instead of the .jar:
               (path->string
                (build-path home-path
                            "bin"))
               #;(path->string
                  (build-path home-path
                              "bin"
                              "margrave.jar"))
               
               ; Margrave requires these JAR files to run:
               java-class-separator
               (path->string
                (build-path home-path
                            "bin"
                            "kodkod.jar"))
               java-class-separator
               (path->string
                (build-path home-path
                            "bin"
                            "org.sat4j.core.jar"))
               java-class-separator
               (path->string
                (build-path home-path
                            "bin"
                            "sunxacml.jar"))
               java-class-separator
               (path->string
                (build-path home-path
                            "bin"
                            "java_cup.jar"))
               java-class-separator
               (path->string
                (build-path home-path
                            "bin"
                            "json.jar"))
               
               
               ; Run this class:
               " edu.wpi.margrave.MCommunicator")])
        
        ;(printf "~a ~a ~a~n" home-path margrave-home-path margrave-command-line)
        (set! java-process-list (process margrave-command-line))
        (set! input-port (first java-process-list))
        (set! output-port (second java-process-list))
        (set! process-id (third java-process-list))
        (set! err-port (fourth java-process-list))
        (set! ctrl-function (fifth java-process-list))
        (set! margrave-home-path home-path)
        #t)
      #f))

(define (cleanup-margrave-engine)
  (close-input-port input-port)
  (close-output-port output-port)
  (close-input-port err-port)  
  ; allow restart of the engine
  (set! java-process-list #f)
  (set! input-port #f)
  (set! output-port #f)
  (set! process-id #f)
  (set! err-port #f)
  (set! ctrl-function #f))

(define (stop-margrave-engine)
  (if (eq? java-process-list #f)
      #f
      (begin
        (m "<MARGRAVE-COMMAND type=\"QUIT\" />")
        ;Should be this: (m (evalxml "QUIT"))
        (ctrl-function 'kill)  ; may not be necessary
        
        (flush-output output-port)    
        (cleanup-margrave-engine)
        #t))) 


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


; Get the response type of a MARGRAVE-RESPONSE element:
; Document/#f -> String
(define (get-response-type doc)
  (if (equal? doc #f)
      ""
      (get-attribute-value (document-element doc) 'type)))


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

; for debugging
(define (mxtextout cmd)
  (printf "Command: ~a~n~nResponse: ~a~n" cmd (mtext cmd)))


; ***************************************************************************************
; User Functions
; ***************************************************************************************

; mtext
; string -> document or #f
; parses and compiles the string command into XML and executes it.
(define (mtext)
  (display "mtext broken for now!"))
#;(define (mtext cmd)
    (m (evalxml cmd)))

; m
; XML string -> document or #f
; Sends the given XML to java. Returns #f if the engine has not been started.
; Uses *buffered* string ports to avoid overhead due to excessive concatenation.
(define (m cmd)
  (if (equal? java-process-list #f) 
      (begin
        (printf "Could not send Margrave command because engine was not started. Call the start-margrave-engine function first.~n")
        #f)
      (begin 
        ; (printf "~a;~n" cmd)
        
        ; Send the command XML
        (display (string-append cmd ";") output-port)
        (flush-output output-port)        
        
        ; Deal with the result
        (let ([command-buffer (open-output-string)]
              [error-buffer (open-output-string)]) 
          (local ((define (flush-error)  ; read until nothing is left. This WILL block.
                    (let ([next-char (read-char err-port)])                                                
                      (when (not (equal? next-char eof))
                        (begin
                          (write-string (string next-char) error-buffer)
                          (flush-error)))))
                  
                  (define (finish-error)
                    (when (char-ready? err-port)  ; If there is a character waiting, read it.
                      (let ([next-char (read-char err-port)])                                                
                        (write-string (string next-char) error-buffer)
                        (finish-error))))
                  
                  (define (fetch-result)
                    (let ([next-char (read-char input-port)])
                      (cond [(equal? next-char #\nul)
                             ; End of command's response. Finish any error data that may be waiting.
                             (finish-error)
                             #t]
                            [(equal? next-char eof)
                             ; Port closed. Read error until eof.
                             (flush-error)
                             #f]
                            [else 
                             ; In progress. Keep reading.                             
                             (write-string (string next-char) command-buffer)
                             (fetch-result)]))))
            
            ; Populate the buffered ports            
            
            ; Handle the results
            (let* ([port-status (fetch-result)]
                   [result (get-output-string command-buffer)]
                   [error-str (get-output-string error-buffer)])
              (when (> (string-length error-str) 0)
                (printf "~n**************************************************~nAdditional ERROR information received:~n ~a~n**************************************************~n" error-str))
              (if (equal? port-status #t)
                  
                  (begin
                    ; Comment out this line to stop printing the XML
                    (printf "~a~n" result)                    
                    ; Parse the reply and return the document struct
                    (read-xml (open-input-string result)))
                  
                  (begin
                    ; Got eof, the port has been cloed.
                    (printf "Margrave engine has closed. EOF reached. No document to return.")      
                    
                    ; !!! TODO: Throw exception here. Should stop even in the middle of a load-policy.
                    ; !!! TODO: Once that is done, it'l be safe to call cleanup below. (Right now, it's
                    ;           spamming with "The engine is not started..."
                    ;(cleanup-margrave-engine)
                    
                    #f))))))))






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


;****************************************************************
;;Pretty Printing returned XML

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

;Takes a document with <MARGRAVE-RESPONSE> as its outer type
(define (pretty-print-model xml-model)
  (let ((annotation-string "")) ;to set later
    ;First, go through the XML and update the 2 hashes. Then print them out.
    (local [(define (helper content) ;content is a list of alternating pcdatas and RELATION elements
              (cond [(empty? content) void]
                    [(pcdata? (first content)) (helper (rest content))]
                    [(element? (first content))
                     (if (equal? 'RELATION (element-name (first content)))
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
                             (helper (rest content))))
                         ;Otherwise, Annotation. For now, print as is
                         (begin (set! annotation-string 
                                      (pcdata-string (first (element-content (first content)))))
                                (helper (rest content))))]
                    [else "Error in pretty-print-model!!"]))
            (define (print-statistics stat-xml) ;stat xml is the statistics element
              ;computed-max-size=\"1\" max-size=\"1\" result-id=\"0\" user-max-size=\"6\"/>
              (display "STATISTICS: \n")
              (display (string-append "Computed max size: " (get-attribute-value stat-xml 'computed-max-size) "\n"))
              (display (string-append "Max size: " (get-attribute-value stat-xml 'max-size) "\n"))
              (display (string-append "Result ID: " (get-attribute-value stat-xml 'result-id) "\n"))
              (display (string-append "User max size: " (get-attribute-value stat-xml 'user-max-size) "\n"))
              )]
      (begin (set! atom-hash (make-hash)) ;First reset the hashes
             (set! predicate-hash (make-hash))
             (set! model-size (attribute-value (first (element-attributes xml-model))))
             (helper (element-content (second (element-content xml-model))))
             (display (string-from-hash))
             (display (string-append "Annotation: " annotation-string "\n"))
             (if (< 2 (length (element-content xml-model)))
                 (print-statistics (fourth (element-content xml-model)))
                 "")))))

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

; Get value for attribute name-symbol of element ele
; Element -> Symbol -> String
(define (get-attribute-value ele name-symbol)
  (attribute-value (first (filter (lambda (attr) (equal? (attribute-name attr) name-symbol))
                                  (element-attributes ele)))))
;************ Pretty Print Info *******************

;Pass this function an xml Document with a MARGRAVE-RESPONSE root element
;For now, acceptable types are "model" "sysinfo" "collection-info" or "vocabulary-info"
(define (pretty-print-response-xml response-doc)
  (let* ((response-element (document-element response-doc))
         (type (get-attribute-value response-element 'type)))
    (cond [(equal? type "model") (pretty-print-model response-element)] 
          [(equal? type "sysinfo") (pretty-print-sys-info-xml response-element)]
          [(equal? type "collection-info") (pretty-print-collection-info-xml response-element)]
          [(equal? type "vocabulary-info") (pretty-print-vocab-info-xml response-element)])))


;Pass this function a <MARGRAVE-RESPONSE type="sysinfo"> element
(define (pretty-print-sys-info-xml info-element)
  (let ([string-buffer (open-output-string)])
    (local ((define (write s)
              (write-string s string-buffer)))
      (begin
        (write "Info:\n")
        (write (string-append "Type: " (get-attribute-value info-element 'type) "\n"))
        (let* ((info-content (element-content info-element))
               (manager-element (second info-content))
               (heap-element (second (element-content manager-element)))
               (non-heap-element (second (element-content manager-element)))
               (vocab-element (fourth info-content))
               (collections-element (sixth info-content))
               (results-element (eighth info-content)))
          (local ((define (get-manager-attribute s)
                    (get-attribute-value manager-element s)))
            (write (string-append "Atoms: " (get-manager-attribute 'atoms) "\n"))
            (write (string-append "Conjunctions: " (get-manager-attribute 'conjunctions) "\n"))
            (write (string-append "Decls: " (get-manager-attribute 'decls) "\n"))
            (write (string-append "Disjunctions: " (get-manager-attribute 'disjunctions) "\n"))
            (write (string-append "Multiplicity: " (get-manager-attribute 'multiplicity) "\n"))
            (write (string-append "Negations: " (get-manager-attribute 'negations) "\n"))
            (write (string-append "Num-variables: " (get-manager-attribute 'num-variables) "\n"))
            (write (string-append "Q-exists: " (get-manager-attribute 'q-exists) "\n"))
            (write (string-append "Q-forall: " (get-manager-attribute 'q-forall) "\n"))
            (write (string-append "Relations: " (get-manager-attribute 'relations) "\n"))
            (write (string-append "Total-Formulas: " (get-manager-attribute 'total-formulas) "\n"))
            (write (string-append "Total-Reclaimed: " (get-manager-attribute 'total-reclaimed) "\n"))
            (write (string-append "Variable-Tuples: " (get-manager-attribute 'variable-tuples) "\n")))
          
          ;Heap Usage
          (write "\nHeap Usage:\n")
          (write (string-append "Init: " (get-attribute-value heap-element 'init ) "\n"))
          (write (string-append "Max: " (get-attribute-value heap-element 'max) "\n"))
          (write (string-append "Units: " (get-attribute-value heap-element 'units) "\n"))
          (write (string-append "Used: " (get-attribute-value heap-element 'used) "\n"))
          
          ;Non heap usage
          (write "\nNon-heap Usage:\n")
          (write (string-append "Init: " (get-attribute-value non-heap-element 'init) "\n"))
          (write (string-append "Max: " (get-attribute-value non-heap-element 'max) "\n"))
          (write (string-append "Units: " (get-attribute-value non-heap-element 'units) "\n"))
          (write (string-append "Used: " (get-attribute-value non-heap-element 'used) "\n"))
          
          (write (string-append "\nVocabularies count: " (get-attribute-value vocab-element 'count )))
          (write (string-append "\nCollections count: " (get-attribute-value collections-element 'count)))
          (write (string-append "\nCached Results count: " (get-attribute-value results-element 'count )))
          (display (get-output-string string-buffer)))))))

;Pass this function a <MARGRAVE-RESPONSE type=\"collection-info\"> element
(define (pretty-print-collection-info-xml info-element)
  (let ([string-buffer (open-output-string)])
    (local ((define (write s)
              (write-string s string-buffer)))
      (begin
        (write "Collection Info:\n")
        (let* ((info-content (element-content info-element))
               (policy-leaf-element (second info-content))
               (idbs (second (element-content policy-leaf-element)))
               (free-variables (fourth (element-content policy-leaf-element))))
          (write (string-append "Policy Name: " (get-attribute-value policy-leaf-element 'name)
                                "\nCombine rule: " (get-attribute-value policy-leaf-element 'rule-combine) "\n"))
          (map (lambda(elem)
                 (write (string-append "IDB: Base name: " (get-attribute-value elem 'base-name) "\n")))
               (filter (lambda(elem) (element? elem))
                       (element-content idbs)))
          (write "Free Variables: \n")
          (map (lambda(elem)
                 (write (string-append "Variable name: " (pcdata-string (first (element-content elem))) "\n")))
               (filter (lambda(elem) (element? elem))
                       (element-content free-variables))))
        (display (get-output-string string-buffer))))))

;Pass this function a <MARGRAVE-RESPONSE type=\"vocabulary-info\"> element
(define (pretty-print-vocab-info-xml info-element)
  (let ([string-buffer (open-output-string)])
    (local ((define (write s)
              (write-string s string-buffer)))
      (begin
        (write "Vocabulary Info:\n")
        (let* ((info-content (element-content info-element))
               (vocab-element (second info-content))
               (sorts-element (second (element-content vocab-element)))
               (req-vector-element (fourth (element-content vocab-element)))
               (axioms-element (sixth (element-content vocab-element))))
          (write (string-append "Vocabulary Name: " (get-attribute-value vocab-element 'name) "\n"))
          (write "Sorts:\n")
          (map (lambda(elem)
                 (write (string-append "Sort name: " (get-attribute-value elem 'name) "\n"
                                       (if (< 1 (length (element-content elem)))
                                           (foldr (lambda(elem rest)
                                                    (string-append "\tSubsort: " (get-attribute-value elem 'name) "\n" rest))
                                                  ""
                                                  (filter (lambda(elem) (element? elem))
                                                          (element-content elem)))
                                           ""))))
               (filter (lambda(elem) (element? elem))
                       (element-content sorts-element)))
          (write "Req-Vector:\n")
          (map (lambda(elem)
                 (write (string-append "Variable: " (pcdata-string (first (element-content elem))) " order: " (get-attribute-value elem 'order) "\n")))
               (filter (lambda(elem) (element? elem))
                       (element-content req-vector-element)))
          (write "Axioms:\n")
          (map (lambda(elem)
                 (write (symbol->string (element-name elem))))
               (filter (lambda(elem) (element? elem))
                       (element-content axioms-element))))
        (display (get-output-string string-buffer))))))

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