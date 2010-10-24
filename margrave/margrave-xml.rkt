;    Copyright (c) 2009-2010 Brown University and Worcester Polytechnic Institute.
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

#lang racket

(require xml)
(require margrave/helpers)

(provide 
 pretty-print-model
 pretty-print-response-xml 
 get-attribute-value
 response-is-success?
 response-is-error?
 response-is-exception?
 response-is-unsat?
 get-response-type
 get-response-error-type
 get-response-error-subtype
 get-response-error-descriptor
 
 xml-explore-result->id
 xml-set-response->list
 xml-list-response->list
 xml-map-response->map
 xml-bool-response->bool
 
 xml-make-load-xacml
 xml-make-load-sqs
 
 ; XML construction commands (used by load-policy in margrave.rkt AND the compiler here)
 ; They are the correct way to construct XML
 xml-make-true-condition
 xml-make-decision
 xml-make-decision-type
 xml-make-rule-list
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
 xml-make-compare-command
 xml-make-is-possible-command
 xml-make-debug
 xml-make-publish
 xml-make-tupling
 xml-make-ceiling
 xml-make-is-guaranteed-command
 xml-make-identifiers-list
 xml-make-type
 xml-make-id
 xml-make-include
 xml-make-under
 xml-make-create-policy-leaf-command
 xml-make-policy-identifier
 xml-make-vocab-identifier
 xml-make-constraint
 xml-make-subset
 xml-make-predicate
 xml-make-relations-list
 xml-make-conjunct-chain
 xml-make-rule
 xml-make-load-xacml-command
 xml-make-load-sqs-command
 xml-make-count-command
 xml-make-count-with-size-command
 xml-make-size
 xml-make-file-name
 xml-make-schema-file-name
 xml-make-load
 xml-make-load-with-schema
 xml-make-quit
 xml-make-show-realized-command
 xml-make-show-unrealized-command
 xml-make-forcases
 xml-make-parent-identifier
 xml-make-child-identifier
 xml-make-get-rules-command
 xml-make-get-qrules-command
 xml-make-equals-formula
 xml-make-and
 xml-make-or
 xml-make-implies
 xml-make-iff
 xml-make-not
 xml-policy-info->req-vector)

; ********************************************************


(define (xml-policy-info->req-vector response-doc)
  (define response-element (document-element response-doc))
  
  ; TODO: only cover the 3 cases, should have a failure case for else
  (define policy-leaf-element (get-child-element response-element 'POLICY-LEAF))
  (define policy-set-element (get-child-element response-element 'POLICY-SET))
  (define saved-query-element (get-child-element response-element 'SAVED-QUERY))
  (define free-vars-element (cond [(and (empty? policy-leaf-element) (empty? policy-set-element))
                                   (get-child-element saved-query-element 'FREE-VARIABLES)]
                                  [(empty? policy-leaf-element)
                                   (get-child-element policy-set-element 'FREE-VARIABLES)]
                                  [else (get-child-element policy-leaf-element 'FREE-VARIABLES)]))
  (define vars-elements (get-child-elements free-vars-element 'VARIABLE))
  
  (map (lambda (var-element) 
         (pcdata-string (first (element-content var-element)))) 
       vars-elements))


;****************************************************************
;;XML

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
(define (response-is-unsat? doc)
  (equal? (get-response-type doc)
          "unsat"))


; Fetch various error properties
; Document -> String
(define (get-response-error-type doc)
  (get-attribute-value (first (get-child-elements (document-element doc) 'ERROR)) 'type))
(define (get-response-error-subtype doc)
  (get-attribute-value (first (get-child-elements (document-element doc) 'ERROR)) 'subtype))
(define (get-response-error-descriptor doc)
  (pcdata-string (first (element-content (first (get-child-elements (document-element doc) 'ERROR))))))

;Helper function
(define (string-contains? str phrase)
  (cond [(< (string-length str) (string-length phrase)) false]
        [else (or (equal? (substring str 0 (string-length phrase)) phrase)
                  (string-contains? (substring str 1) phrase))]))


;****************************************************************
;;Pretty Printing returned XML

;name is the name of the atom, such as "s" or "r" (doesn't include the $),
; and list of types is a list of types (which are really predicates that
; have only one atom in (predicate-list-of-atoms))
(define-struct atom (name 
                     display-name 
                     list-of-types                     
                     ) #:mutable)

;Note that a type is also a predicate, but with only one atom.
(define-struct predicate (name arity list-of-tuples) #:mutable)

;Maps strings (such as "s") to their corresponding atoms
;(define atom-hash (make-hash))

;Maps name of predicates (strings) to their corresponding predicate structs
;(define predicate-hash (make-hash))

;Takes a document with <MARGRAVE-RESPONSE> as its outer type
;This function goes through the xml and updates atom-hash and predicate-hash
;It then calls (string-from-hash) which creates a string based on atom-hash and predicate-hash
(define (pretty-print-model xml-response)
  (let* ([atom-hash (make-hash)]
         [predicate-hash (make-hash)]
         [model-element (get-child-element xml-response 'MODEL)]
         [relation-elements (get-child-elements model-element 'RELATION)]
         [universe-element (get-child-element model-element 'UNIVERSE)]         
         [model-size (get-attribute-value model-element 'size)] 
         [annotation-elements (get-child-elements model-element 'ANNOTATION)]
         [statistics-element (get-child-element xml-response 'STATISTICS)]         
         [string-buffer (open-output-string)]) 
    
    ; Initialize the atom hash with everything in UNIVERSE
    ; Initial display name is equal to atom-name    
    (for-each (lambda (an-atom-element)
                (let ([atom-name (pcdata-string (first (element-content an-atom-element)))])
                  (hash-set! atom-hash atom-name (make-atom atom-name atom-name empty))))
              (get-child-elements universe-element 'atom))
    
    ; go through the XML and update the 2 hashes. Then afterwards print them out.
    (local ((define (write s)
              (write-string s string-buffer))
            
            (define (handle-relation relation)                   
              (let* ([relation-arity (string->number (get-attribute-value relation 'arity))]
                     [relation-name  (get-attribute-value relation 'name)]
                     [relation-is-sort (equal? "sort" (get-attribute-value relation 'type))])
                (begin
                  
                  ;if the relation (predicate) doesn't exist in the hash yet, create it
                  (when (not (hash-ref predicate-hash relation-name #f))
                    (hash-set! predicate-hash relation-name (make-predicate relation-name relation-arity empty)))                    
                  
                  (let* ([predicate-struct (hash-ref predicate-hash relation-name)] ;should definitely exist, since we just created it if it didn't
                         [tuple-elements (get-child-elements relation 'TUPLE)]) 
                    
                    (local [; Insert a tuple into the current relation
                            (define (insert-tuple tuple-element)
                              (when (or (not relation-is-sort)
                                        (equal? "true" (get-attribute-value tuple-element 'not-in-subsort)))
                                (set-predicate-list-of-tuples! predicate-struct (cons (parse-tuple-contents (get-child-elements tuple-element 'ATOM)) 
                                                                                      (predicate-list-of-tuples predicate-struct)))))
                            ; Produce a tuple (list of atom structs) from XML
                            (define (parse-tuple-contents atom-elements)
                              (if (empty? atom-elements)
                                  '()
                                  (let* ([an-atom-element (first atom-elements)]
                                         [atom-element-name (pcdata-string (first (element-content an-atom-element)))])                                                                    
                                    (begin   
                                      
                                      ; to be removed
                                      ;if the atom doesn't exist in the hash yet, create it
                                      ; (regardless of whether this relation is to be printed)
                                     ; (when (not (hash-ref atom-hash atom-name #f))
                                     ;   (hash-set! atom-hash atom-name (make-atom atom-name empty)))
                                      
                                      ; Get the atom struct for this atom
                                      ;should definitely exist, since we just created it if it didn't
                                      (let ([atom-struct (hash-ref atom-hash atom-element-name)]) 
                                        
                                        ; If this is a $var relation, change displayed name for the atom
                                        ; otherwise, add to the appropriate relation                                       
                                        (if (equal? (string-ref relation-name 0) #\$)
                                            (if (equal? (atom-name atom-struct)
                                                        (atom-display-name atom-struct))                                                
                                                (set-atom-display-name! atom-struct (substring relation-name 1))
                                                (set-atom-display-name! atom-struct (string-append (atom-display-name atom-struct)
                                                                                           "="
                                                                                           (substring relation-name 1))))
                                            
                                            ; Not a $var. Is it a sort or predicate?
                                            (if (= relation-arity 1) 
                                                
                                                ; Sort (tuple is unary)                                                  
                                                (begin 
                                                  ; Note that this atom belongs to this sort
                                                  (when relation-is-sort
                                                    (set-atom-list-of-types! atom-struct (cons relation-name (atom-list-of-types atom-struct))))                             
                                                  ; A tuple is a list of atom-structs. This is a unary tuple. Return a singleton list.
                                                  (list atom-struct)) 
                                                
                                                
                                                ; Predicate (tuple is k-ary, where k is the arity of the pred)                           
                                                (cons atom-struct (parse-tuple-contents (rest atom-elements) )))))))))]
                      
                      (for-each insert-tuple tuple-elements)))))))
      
      (begin
        (for-each handle-relation relation-elements)
        (write (string-from-hash atom-hash predicate-hash model-size))
        (when (> (length annotation-elements) 0)
          (write "\n    -> Also:\n"))
        (print-annotations string-buffer annotation-elements)
        (when (not (equal? empty statistics-element))
          (write (print-statistics statistics-element)))
        (write "********************************************************")
        
        
        ; Debugging (let the caller decide whether to print or not)
        ;(display (get-output-string string-buffer))
        (get-output-string string-buffer)))))

(define (print-annotations buffer list-of-annot)
  (for-each (lambda (annotation-element)
              (write-string (pcdata-string (first (element-content annotation-element))) buffer)
              (write-string "\n" buffer))
            list-of-annot)) 

;Returns a string to display based on atom-hash and predicate-hash
(define (string-from-hash atom-hash predicate-hash model-size)
  (local [(define (atom-helper hash-pos)
            (cond [(false? hash-pos) ""]
                  [else (let ((atom (hash-iterate-value atom-hash hash-pos)))
                          (string-append
                           (atom-display-name atom)
                           ": "
                           (foldl (lambda (type rest) (string-append type " " rest)) "" (atom-list-of-types atom))
                           "\n"
                           (atom-helper (hash-iterate-next atom-hash hash-pos))))]))
          (define (predicate-helper hash-pos)
            (cond [(false? hash-pos) ""]
                  [else (let ((predicate (hash-iterate-value predicate-hash hash-pos)))
                          (if (= (predicate-arity predicate) 1) ;If type, continue, otherwise print
                              (predicate-helper (hash-iterate-next predicate-hash hash-pos))
                              (string-append
                               (predicate-name predicate)
                               " = {"
                               ; For each tuple (as a list of atoms) 
                               (foldl (lambda (a-tuple so-far) 
                                        ;(printf "l: ~a ~a ~a ~a ~n" a-tuple so-far (map atom-name a-tuple) (fold-append-with-separator (map atom-name a-tuple) ", " ))
                                        (string-append 
                                         "[" 
                                         (fold-append-with-separator (map atom-display-name a-tuple) ", ")
                                         (if (not (equal? so-far ""))
                                             "], "
                                             "]") 
                                         so-far))
                                      ""
                                      (predicate-list-of-tuples predicate))
                               "}"
                               "\n"
                               (predicate-helper (hash-iterate-next predicate-hash hash-pos)))))]))]
    (string-append "********* SOLUTION FOUND at size = " model-size " ******************\n"
                   (atom-helper (hash-iterate-first atom-hash))
                   (predicate-helper (hash-iterate-first predicate-hash)))))

; Get value for attribute name-symbol of element ele
; Element -> Symbol -> String
(define (get-attribute-value ele name-symbol)
  (let ([that-attribute-list (filter (lambda (attr) (equal? (attribute-name attr) name-symbol))
                                     (element-attributes ele))])
    (if (empty? that-attribute-list)
        ""
        (attribute-value (first that-attribute-list)))))

; element -> list
(define (get-child-elements element name-symb-or-str)
  (if (empty? element)
      empty
      (let* ([name (string-downcase (if (symbol? name-symb-or-str)
                                        (symbol->string name-symb-or-str)
                                        name-symb-or-str))]
             [elements (filter (lambda (maybe-elem) (element? maybe-elem))
                               (element-content element))]
             [result (filter (lambda (element) (equal? name (string-downcase (symbol->string (element-name element)))))
                             elements)])
       ; (printf "get-child-elements: ~a~n~a~n~a~n~a~n" name-symb-or-str name elements result)
        result)))

;Pass name a symbol or a string, case doesn't matter
;Returns either an element, if found, or an empty list
;Note that if you pass this element an empty list instead of an element, instead of halting it will return empty
(define (get-child-element element name-symb-or-str)
  (let ([result (get-child-elements element name-symb-or-str)])
    (if (empty? result)
        result
        (first result))))

(define (get-pc-data elem)
  (pcdata-string (first (element-content elem))))

(define (print-statistics stat-element) ;stat xml is the statistics element
  (let* ((string-buffer (open-output-string)))
    (local ((define (write s)
              (write-string s string-buffer)))
      (write "\nSTATISTICS: \n")
      (let* ([computed-max (get-attribute-value stat-element 'computed-max-size)]
             [user-provided-max (get-attribute-value stat-element 'user-max-size)]
             [used-max (get-attribute-value stat-element 'max-size)]
             [computed-max-num (string->number computed-max)]
             [user-provided-max-num (string->number user-provided-max)]
             [used-max-num (string->number used-max)])
        
        (if (<= computed-max-num 0)
            (write "Margrave could not calculate a size ceiling for this query.\n")            
            (write (format "Margrave computed that ~a would be a sufficient size ceiling.\n" computed-max)))

        (if (< user-provided-max-num 0)
            (write "No ceiling explicitly provided in the query's CEILING clause.\n")
            (write (format "Size ceiling given in the query's CEILING clause: ~a.\n" user-provided-max)))
        
        (write (format "Used size ceiling: ~a\n" used-max))
        
        (cond [(< computed-max-num 0)
               (write "WARNING:  Margrave could not calculate a sufficient ceiling size. Completeness is not guaranteed! You may need to check for larger scenarios.\n")]
              [(< used-max-num computed-max-num)
               (write (format "WARNING: Margrave will not check over ceiling size ~a without your permission. To increase the scenario size, use the CEILING clause of EXPLORE.\n" used-max-num))])
        
        (get-output-string string-buffer)))))

;************ Pretty Print Info *******************

;Pass this function an xml Document with a MARGRAVE-RESPONSE root element
; document? or element? or string? -> string?
(define (pretty-print-response-xml the-response)
  (cond [(false? the-response)
         "Engine stopped!"]
        
        [(document? the-response)
         (pretty-print-response-xml (document-element the-response))]
        
        [(element? the-response)
         (let* ([type (get-attribute-value the-response 'type)])
           ;Debugging: (display (xexpr->string (xml->xexpr (document-element response-doc))))
           (cond [(equal? type "model") (pretty-print-model the-response)] 
                 [(equal? type "sysinfo") (pretty-print-sys-info-xml the-response)]
                 [(equal? type "collection-info") (pretty-print-collection-info-xml the-response)]
                 [(equal? type "vocabulary-info") (pretty-print-vocab-info-xml the-response)]
                 [(equal? type "error") (pretty-print-error-xml the-response)]
                 [(equal? type "exception") (pretty-print-exception-xml the-response)]
                 [(equal? type "explore-result") (pretty-print-explore-xml the-response)]
                 [(equal? type "unsat") (pretty-print-unsat-xml the-response)]
                 [(equal? type "boolean") (pretty-print-boolean-xml the-response)]
                 [(equal? type "string") (pretty-print-string-xml the-response)]
                 [(equal? type "set") (pretty-print-set-xml the-response)]
                 [(equal? type "list") (pretty-print-list-xml the-response)]
                 [(equal? type "map") (pretty-print-map-xml the-response)]
                 [(equal? type "success") "Success\n"]))]
        
        [(string? the-response) the-response]
        
        [else the-response]))

; element -> string
(define (pretty-print-string-xml element)
  (let* ((string-buffer (open-output-string))
         (string-element (get-child-element element 'string)))
    (local ((define (write s)
              (write-string s string-buffer)))
      (begin 
        (write (get-attribute-value string-element 'value))
        (get-output-string string-buffer)))))

; element -> string
(define (pretty-print-boolean-xml element)
  (let* ((string-buffer (open-output-string))
         (statistics-element (get-child-element element 'statistics))
         (boolean-element (get-child-element element 'boolean)))
    (local ((define (write s)
              (write-string s string-buffer)))
      (begin 
        (write (get-attribute-value boolean-element 'value))
        (when (not (empty? statistics-element))
          (write (string-append "\n" (print-statistics statistics-element))))
        (get-output-string string-buffer)))))

(define (xml-bool-response->bool element-or-document)
  (let* ([response-element (maybe-document->element element-or-document)]
         [boolean-element (get-child-element response-element 'boolean)])
    (equal? "true" (get-attribute-value boolean-element 'value))))

; element -> string
(define (pretty-print-set-xml element)
  (let* ((string-buffer (open-output-string))
         (statistics-element (get-child-element element 'statistics))
         (set-element (get-child-element element 'SET))
         (item-elements (get-child-elements set-element 'ITEM)))
    (local ((define (write s)
              (write-string s string-buffer)))
      (begin 
        (write "{\n")
        (for-each (lambda (item-element)             
                    (write (string-append "  " (pcdata-string (first (element-content item-element))) "\n")))
                  item-elements)
        (write "}\n")                 
        
        (when (not (empty? statistics-element))
          (write (string-append "\n" (print-statistics statistics-element))))
        
        (get-output-string string-buffer)))))

; May have the response XML document, or its response element
; get the element if needed
(define (maybe-document->element response-element-or-document)
  (if (document? response-element-or-document)
      (document-element response-element-or-document)
      response-element-or-document))

; XML <MARGRAVE-RESPONSE type="set">  --> list
(define (xml-set-response->list response-element-or-document)
  (let* ([response-element (maybe-document->element response-element-or-document)]
         [set-element (get-child-element response-element 'SET)]
         [item-elements (get-child-elements set-element 'ITEM)])
    (map (lambda (item-element)             
           (pcdata-string (first (element-content item-element))))
         item-elements)))

; XML <MARGRAVE-RESPONSE type="list">  --> list
; Need to preserve ordering, even if XML has messed it up.
(define (xml-list-response->list response-element-or-document)
  (let* ([response-element (maybe-document->element response-element-or-document)]
         [list-element (get-child-element response-element 'LIST)]
         [list-size (string->number (get-attribute-value list-element 'size))]
         [item-elements (get-child-elements list-element 'ITEM)]
         [mut-vector (make-vector list-size)])
    (for-each (lambda (item-element)
                (let ([item-posn (string->number (get-attribute-value item-element 'order))])
                  (vector-set! mut-vector
                               (- item-posn 1) ; list is 1-based; vector is 0-based
                               (pcdata-string (first (element-content item-element))))))
              item-elements)
    (vector->list mut-vector)))

; XML <MARGRAVE-RESPONSE type="list">  --> string
(define (pretty-print-list-xml element)
  (let* ([rkt-list (xml-list-response->list element)]
         [string-buffer (open-output-string)])
    (local ((define (write s)
              (write-string s string-buffer)))
      (begin 
        (write "<\n")
        (for-each (lambda (item)             
                    (write (string-append "  " item "\n")))
                  rkt-list)
        (write ">\n") 
        (get-output-string string-buffer)))))

; XML <MARGRAVE-RESPONSE type="map">  --> hash table
; the MAP element maps each key to a set of values
(define (xml-map-response->map response-element-or-document)
  (let* ([response-element (maybe-document->element response-element-or-document)]
         [map-element (get-child-element response-element 'MAP)]
         [entry-elements (get-child-elements map-element 'ENTRY)]
         [mut-hashtable (make-hash)])
    ; For each entry
    (for-each (lambda (entry-element)
                (let ([entry-key (get-attribute-value entry-element 'key)]
                      [entry-values (get-child-elements entry-element 'value)])    ; 
                  (hash-set! mut-hashtable
                             entry-key
                             ; for each value
                             (map (lambda (val) (pcdata-string (first (element-content val))))
                                  entry-values))))
              entry-elements)
    mut-hashtable))

; XML <MARGRAVE-RESPONSE type="map">  --> string
(define (pretty-print-map-xml element)
  (let ([the-hashtable (xml-map-response->map element)])
    (pretty-print-hashtable the-hashtable)))


; hash table -> string
(define (pretty-print-hashtable thetable)
  (let* ([string-buffer (open-output-string)])
    (local ((define (write s)
              (write-string s string-buffer))) 
      (write "{\n")
      (hash-for-each thetable
                     (lambda (k v) (write (string-append k " -> " (fold-append-with-separator v ", ") "\n"))))
      (write "}\n")
      (get-output-string string-buffer))))



; element -> string
(define (pretty-print-unsat-xml element)
  (let* ((string-buffer (open-output-string))
         (statistics-element (get-child-element element 'statistics)))
    (local ((define (write s)
              (write-string s string-buffer)))
      (begin 
        (write "---> No more solutions! <---\n")
        (write (print-statistics statistics-element))
        (get-output-string string-buffer)))))

; element -> string
(define (pretty-print-explore-xml element)
  (let* ((string-buffer (open-output-string))
         (result-element (get-child-element element 'result-handle)))
    (begin 
      ; Omit result handle since only zero is ever returned (for the moment).
      (write-string (string-append "Query created successfully.") string-buffer) ;Result handle was: " (get-pc-data result-element) "\n") string-buffer)
      ; debug
      ;(display (get-output-string string-buffer))
      (get-output-string string-buffer))))

(define (xml-explore-result->id doc-or-ele)
  (let* ([response-element (maybe-document->element doc-or-ele)]        
         [result-element (get-child-element response-element 'result-handle)])
    (get-pc-data result-element)))

; element -> string
(define (xml-id->id element)
  (let* ((string-buffer (open-output-string))
         (result-element (get-child-element element 'result-handle)))
    (begin 
      (write-string (string-append "Query created. Result handle was: " (get-pc-data result-element) "\n") string-buffer)
      ; debug
      ;(display (get-output-string string-buffer))
      (get-output-string string-buffer))))

; element -> string
(define (pretty-print-exception-xml element)
  (let* ([string-buffer (open-output-string)]
         (exception-element (get-child-element element 'exception))
         (exception-attributes (element-attributes exception-element))
         (message-element (get-child-element exception-element 'message))
         (message-text (if (empty? (element-content message-element))
                           ""
                           (pcdata-string (first (element-content message-element)))))
         (location-element (get-child-element exception-element 'location))
         (command-element (get-child-element exception-element 'command))) ;TODO Don't have an example of this, so not implemented (VS)
    (local ((define (write s)
              (write-string s string-buffer)))
      (begin
        (cond
          ; Special handling for user exceptions
          ; Nothing interesting for now, later will add more
          ; (Should have fields depending on the exception type)
          [(or (string-contains? (get-attribute-value exception-element 'class) "edu.wpi.margrave.MUserException")
               (string-contains? (get-attribute-value exception-element 'class) "edu.wpi.margrave.MGEUnknownIdentifier")
               (string-contains? (get-attribute-value exception-element 'class) "edu.wpi.margrave.MGETuplingFailure")
               (string-contains? (get-attribute-value exception-element 'class) "edu.wpi.margrave.MGECombineVocabs")
               (string-contains? (get-attribute-value exception-element 'class) "edu.wpi.margrave.MUnsupportedFormulaException")
               (string-contains? (get-attribute-value exception-element 'class) "edu.wpi.margrave.MNotASortException")
               (string-contains? (get-attribute-value exception-element 'class) "edu.wpi.margrave.MGEArityMismatch")
               (string-contains? (get-attribute-value exception-element 'class) "edu.wpi.margrave.MGEManagerException")
               (string-contains? (get-attribute-value exception-element 'class) "edu.wpi.margrave.MGEVariableAlreadyBound")               
               (string-contains? (get-attribute-value exception-element 'class) "edu.wpi.margrave.MGEBadCombinator")
               (string-contains? (get-attribute-value exception-element 'class) "edu.wpi.margrave.MGEBadIdentifierName")
               (string-contains? (get-attribute-value exception-element 'class) "edu.wpi.margrave.MGEUnsupportedXACML"))
           (write (string-append message-text "\n"))]
          
          
          ;Otherwise just raw print the returned exception. it must be something serious.
          [else (begin     
                  (write "Could not process Margrave command; The Java engine returned an exception:\n")
                  (write (string-append "Class: " (get-attribute-value exception-element 'class) "\n"))
                  (write (string-append "Stack Trace: " (get-attribute-value exception-element 'stack-trace) "\n"))
                  (when (not (empty? message-element))
                    (write (string-append "Message: " message-text "\n")))
                  (when (not (empty? location-element))
                    (write (string-append "Location of Problem: " (get-attribute-value location-element 'problem) "\n"))))])
        
        ;(display (get-output-string string-buffer))
        (get-output-string string-buffer)))))

;Pass this function a <MARGRAVE-RESPONSE type="error"> element
; element -> string
(define (pretty-print-error-xml element)
  (define string-buffer (open-output-string))
  (define error-element (get-child-element element 'error))
  (define more-details (if (empty? (element-content error-element))
                           ""
                           (pcdata-string (first (element-content error-element)))))
  
  (local ((define (write s)
            (write-string s string-buffer)))
    (begin
      (write "Margrave encountered an error: ")
      (write (get-attribute-value error-element 'type))
      (write " ")
      (write (get-attribute-value error-element 'subtype))
      (when (> (string-length more-details) 0)        
        (write (string-append ": " more-details)))
      (get-output-string string-buffer))))


;Pass this function a <MARGRAVE-RESPONSE type="sysinfo"> element
; element -> string
(define (pretty-print-sys-info-xml info-element)
  (let ([string-buffer (open-output-string)])
    (local ((define (write s)
              (write-string s string-buffer)))
      (begin
        (write "System Information:\n")             
        (let* ((manager-element (get-child-element info-element 'manager))
               (heap-element (get-child-element manager-element 'heap-usage))
               (non-heap-element (get-child-element manager-element 'non-heap-usage))
               (vocab-element (get-child-element info-element 'vocabularies))
               (collections-element (get-child-element info-element 'collections))
               (results-element (get-child-element info-element 'cached-results)))
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
          
          ; debug
          ; (display (get-output-string string-buffer))
          (get-output-string string-buffer))))))


;Pass this function a <MARGRAVE-RESPONSE type=\"collection-info\"> element
; element -> string
(define (pretty-print-collection-info-xml info-element)
  (let ([string-buffer (open-output-string)])
    (local ((define (write s)
              (write-string s string-buffer)))
      (begin
        (write "~~~~~~~~~~ Information ~~~~~~~~~~\n")                
        (let* ((policy-leaf-element (get-child-element info-element 'policy-leaf))
               (policy-set-element (get-child-element info-element 'policy-set))
               (saved-query-element (get-child-element info-element 'saved-query)))
          
          (when (not (empty? policy-leaf-element))
            (write (pretty-print-policy-leaf policy-leaf-element)))
          
          (when (not (empty? policy-set-element))
            (write (pretty-print-policy-set policy-set-element)))
          
          (when (not (empty? saved-query-element))
            (write (pretty-print-saved-query saved-query-element)))
          
          (write "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n")                
          ; debug
          ;(display (get-output-string string-buffer))
          (get-output-string string-buffer))))))

(define (pretty-print-provided-idbs idbs-element)
  (let ([string-buffer (open-output-string)])
    (write-string "IDBs provided: \n" string-buffer)
    (map (lambda(elem)
           (write-string (string-append "    " (get-attribute-value elem 'base-name) "\n") string-buffer))
         (filter (lambda(elem) (element? elem))
                 (element-content idbs-element)))
    (write-string "\n" string-buffer)
    (get-output-string string-buffer)))

(define (pretty-print-free-variables vars-element)
  (let ([string-buffer (open-output-string)])
    (write-string "Variables (in order): \n" string-buffer)
    (map (lambda(elem)
           (write-string (string-append "    " (get-pc-data elem) "\n") string-buffer))
         (filter (lambda(elem) (element? elem))
                 (element-content vars-element)))
    (get-output-string string-buffer)))

(define (pretty-print-saved-query the-element)
  (let* ([string-buffer (open-output-string)]
         (idbs (get-child-element the-element 'idbs))
         (free-variables (get-child-element the-element 'free-variables)))
    
    (write-string "This is a saved query.\n" string-buffer)
    
    (when (not (empty? idbs))
      (write-string (pretty-print-provided-idbs idbs)) string-buffer)
    
    (when (not (empty? free-variables))
      (write-string (pretty-print-free-variables free-variables) string-buffer))
    
    (get-output-string string-buffer)))

(define (pretty-print-policy-leaf the-element)
  (let* ([string-buffer (open-output-string)]
         (idbs (get-child-element the-element 'idbs))
         (free-variables (get-child-element the-element 'free-variables)))
    
    (write-string (string-append "This is a policy named: " (get-attribute-value the-element 'name) "\n") string-buffer)
    (write-string "  This policy is a LEAF; it contains rules and no sub-policies.\n" string-buffer)
    
    (when (not (empty? idbs))
      (write-string (pretty-print-provided-idbs idbs) string-buffer))
    
    (when (not (empty? free-variables))
      (write-string (pretty-print-free-variables free-variables) string-buffer))
    
    (get-output-string string-buffer)))

(define (pretty-print-policy-set the-element)
  (let* ([string-buffer (open-output-string)]
         (idbs (get-child-element the-element 'idbs))
         (free-variables (get-child-element the-element 'free-variables)))
    
    (write-string (string-append "This is a policy named: " (get-attribute-value the-element 'name) "\n") string-buffer)
    (write-string "  This policy is a SET; it contains sub-policies and no rules.\n" string-buffer)
    
    (when (not (empty? idbs))
      (write-string (pretty-print-provided-idbs idbs)) string-buffer)
    
    (when (not (empty? free-variables))
      (write-string (pretty-print-free-variables free-variables) string-buffer))
    
    (get-output-string string-buffer)))

;Pass this function a <MARGRAVE-RESPONSE type=\"vocabulary-info\"> element
; element -> string
(define (pretty-print-vocab-info-xml info-element)
  (let ([string-buffer (open-output-string)])
    (local ((define (write s)
              (write-string s string-buffer)))
      (begin
        (write "Vocabulary Information:\n")
        (let* ((vocab-element (get-child-element info-element 'vocabulary))
               (sorts-element (get-child-element vocab-element 'sorts))
               (req-vector-element (get-child-element vocab-element 'req-vector))
               (axioms-element (get-child-element vocab-element 'axioms)))
          (write (string-append "Vocabulary Name: " (get-attribute-value vocab-element 'name) "\n"))
          (write "Sorts:\n")
          (local ((define (write-sorts elem)
                    (write (string-append "Sort name: " (get-attribute-value elem 'name) "\n"
                                          (if (< 1 (length (element-content elem)))
                                              (foldr (lambda(elem rest)
                                                       (string-append "\tSubsort: " (get-attribute-value elem 'name) "\n" rest))
                                                     ""
                                                     (filter (lambda(elem) (element? elem))
                                                             (element-content elem)))
                                              ""))))
                  (define (write-sorts-disj elem)
                    (write (string-append 
                            (if (< 1 (length (element-content elem)))
                                (foldr (lambda (other-elem rest)
                                         (string-append "    " (get-attribute-value elem 'name) " -DISJ- " (get-attribute-value other-elem 'name) "\n" rest))
                                       ""
                                       (filter (lambda(e) (element? e))
                                               (element-content elem)))
                                "")))))
            (map write-sorts
                 (filter (lambda(elem) (element? elem))
                         (element-content sorts-element)))
            (write "Req-Vector:\n")
            (map (lambda(elem)
                   (write (string-append "Variable: " (pcdata-string (first (element-content elem))) " order: " (get-attribute-value elem 'order) "\n")))
                 (filter (lambda(elem) (element? elem))
                         (element-content req-vector-element)))
            (write "Axioms:\n")
            (map (lambda(elem)
                   (let ((elem-name (symbol->string (element-name elem))))
                     (begin (write (string-append (symbol->string (element-name elem)) "\n"))
                            (cond
                              [(equal? elem-name "DISJOINT")
                               (map write-sorts-disj
                                    (filter (lambda(elem) (element? elem))
                                            (element-content elem)))]
                              [(equal? elem-name "SUBSETS")
                               (map (lambda (subset-elem)
                                      (begin
                                        (write (string-append "Parent: " (get-attribute-value subset-elem 'parent) "\n" 
                                                              "Children: \n"))
                                        (map (lambda (child-elem)
                                               (write (string-append "\tChild: " (get-attribute-value child-elem 'name) "\n")))
                                             (filter (lambda (element) (element? element))
                                                     (element-content subset-elem)))
                                        (write "\n")))
                                    (filter (lambda(element) (element? element))
                                            (element-content elem)))]))))
                 (filter (lambda(elem) (element? elem))
                         (element-content axioms-element))))
          ; debug
          ;(display (get-output-string string-buffer))
          (get-output-string string-buffer))))))

;load policy ./tests/conference1.p; 
;
;rename ConferencePolicy1 conf1; 
;
;explore readpaper(a) and paper(r) and conf1:permit(s,a,r);
;
;get one ;
;get next 0;
;get next 0;
;get next 0;
;get next ;
;get next ;
;get next ;
;get next ;
;get next ;
;get next ;
;get next ;
;get next ;
;
;count;
;is possible?;
;get ceiling;

;************************************************************************
;;These functions return x-exprs for parts of command
(define (xml-make-info-id id)
  `(INFO ((id ,id))))

(define (xml-make-info-id-command id)
  (xml-make-command "INFO" (list (xml-make-info-id id))))

(define (xml-make-info)
  `(INFO))

(define (xml-make-info-command)
  (xml-make-command "INFO" (list (xml-make-info))))

(define (xml-make-get-rules-command polid (decid-str ""))
  (xml-make-command "GET-INFO" (list (xml-make-get-rules "RULES" polid decid-str))))
(define (xml-make-get-qrules-command polid (decid-str ""))
  (xml-make-command "GET-INFO" (list (xml-make-get-rules "QUALIFIED-RULES" polid decid-str))))

(define (xml-make-load-xacml fn sfn)
  (xml-make-command "LOAD XACML POLICY" (list (xml-make-xacml-load fn sfn))))

(define (xml-make-load-sqs fn)
  (xml-make-command "LOAD SQS POLICY" (list (xml-make-sqs-load fn))))


(define (xml-make-xacml-load fn sfn)
  `(LOAD ((file-name ,fn) (schema-file-name ,sfn))))

(define (xml-make-sqs-load fn)
    `(LOAD ((file-name ,fn))))

(define (xml-make-get-rules get-type polid decid-str)
  (if (not (equal? "" decid-str))
      `(GET-INFO ((type ,get-type)) ,(xml-make-policy-identifier (symbol->string polid)) ,(xml-make-decision decid-str))
      `(GET-INFO ((type ,get-type)) ,(xml-make-policy-identifier (symbol->string polid)))))

(define (xml-make-policy-identifier policy-name)
  `(POLICY-IDENTIFIER ((pname ,policy-name))))

(define (xml-make-vocab-identifier vocab-name)
  `(VOCAB-IDENTIFIER ((vname ,vocab-name))))

(define (xml-make-parent-identifier parent-name)
  `(PARENT-IDENTIFIER ((name ,(symbol->string/safe parent-name)))))

(define (xml-make-child-identifier child-name)
  `(CHILD-IDENTIFIER ((name ,(symbol->string/safe child-name)))))

(define (xml-make-predicate pred-name)
  `(PREDICATE ((name ,pred-name))))

(define (xml-make-rule rule-name dtype rule-list)
  `(RULE ((name ,(symbol->string/safe rule-name))) ,dtype ,rule-list))

;rule-list is of the form ((!Conflicted s r) (ReadPaper a) (Paper r)), or true
(define (xml-make-rule-list orig-rule-list)
  ; (printf "~a~n" rule-list)
  
  ; Don't keep un-necessary 'true
  (let ([rule-list (filter (lambda (relation) (not (equal? 'true relation)))
                           orig-rule-list)])
    
    (if (empty? rule-list)
        `(RELATIONS) 
        `(RELATIONS ,@(map (lambda (relation)
                             (let* ((relation-name (symbol->string/safe (first relation)))
                                    (negation? (starts-with-exclamation relation-name)))
                               `(RELATION ((name ,(if negation? ;Take out the exclamation point
                                                      (substring relation-name 1)
                                                      relation-name))
                                           (sign ,(if negation?
                                                      "false"
                                                      "true")))
                                          ,(xml-make-identifiers-list (rest relation)))))
                           
                           rule-list)))))

(define (starts-with-exclamation string)
  (if (equal? "!" (substring string 0 1))
      true
      false))

(define (xml-make-decision-type decision-type)
  `(DECISION-TYPE ((type ,(symbol->string/safe decision-type)))))

(define (xml-make-decision decision)
  `(DECISION ((name ,(symbol->string/safe decision)))))

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

(define (xml-make-subset parent child)
  `(CONSTRAINT ((type ,"SUBSET")) ,parent ,child))

(define (xml-make-rename id1 id2)
  `(RENAME ((id1 ,id1) (id2 ,id2))))

(define (xml-make-rename-command id1 id2)
  (xml-make-command "RENAME" (list (xml-make-rename id1 id2))))

;;Count isn't fully implemented/tested!
(define (xml-make-count id)
  `(COUNT (,id)))

(define (xml-make-count-command id)
  (xml-make-command "COUNT" (list (xml-make-count id))))

(define (xml-make-count-with-size id size)
  `(COUNT (,id ,size)))

(define (xml-make-count-with-size-command id size)
  (xml-make-command "COUNT" (list (xml-make-count-with-size id size))))

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

(define (xml-make-under list-of-policies)
  `(UNDER ,@list-of-policies))

(define (xml-make-create-policy-leaf policy vocab)
  `(CREATE-POLICY-LEAF ,policy ,vocab))

(define (xml-make-create-policy-leaf-command policy vocab)
  (xml-make-command "CREATE POLICY LEAF" (list (xml-make-create-policy-leaf policy vocab))))

(define (xml-make-is-possible xml-id)
  `(IS-POSSIBLE (,xml-id)))

(define (xml-make-is-possible-command xml-id)
  (xml-make-command "IS-POSSIBLE" (list (xml-make-is-possible xml-id))))

(define (xml-make-is-guaranteed id)
  `(IS-GUARANTEED ((id ,id))))

(define (xml-make-is-guaranteed-command id)
  (xml-make-command "IS-GUARANTEED" (list (xml-make-is-guaranteed id))))

(define (xml-make-publish list-of-identifiers)
  `(PUBLISH ,list-of-identifiers))

(define (xml-make-include list-of-atomic-formulas)
  `(IDBOUTPUT ,@list-of-atomic-formulas))

; TN: left populated as XML data rather than changing it, since "realized" isn't final either
(define (xml-make-show-realized-command id childlist)
  (xml-make-command "SHOW" (list `(SHOW ((type "POPULATED") ,id) ,@childlist))))

(define (xml-make-show-unrealized-command id childlist)
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
(define (xml-make-equals-formula v1-name v2-name)
  `(EQUALS ((v1 ,v1-name) (v2 ,v2-name))))

(define (xml-make-atomic-formula-n relName xml-identifier-list)
  `(ATOMIC-FORMULA-N ((relation-name ,(symbol->string/safe relName))) ,xml-identifier-list))  

(define (xml-make-atomic-formula-y collName relName xml-identifier-list)
  ;(printf "~n~nY: ~a ~a ~n" collName relName)
  (if (empty? xml-identifier-list)
      `(ATOMIC-FORMULA-Y ((collection-name ,(symbol->string/safe collName)) (relation-name ,(symbol->string/safe relName))))
      `(ATOMIC-FORMULA-Y ((collection-name ,(symbol->string/safe collName)) (relation-name ,(symbol->string/safe relName))) ,xml-identifier-list))) 

;;EXPLORE
;Makes an xexpr for a list of atomic formulas (can be of size 1). symbol can be "and" or "or"
(define (xml-make-atomic-formulas-list symbol list-of-atomic-formulas)
  (if (equal? 1 (length list-of-atomic-formulas))
      (first list-of-atomic-formulas)
      (foldr (lambda (atomic-formula rest)
               (list symbol atomic-formula rest))
             (first list-of-atomic-formulas)
             (rest list-of-atomic-formulas))))

;Atomic Formulas must already be xexprs
(define (xml-make-explore list-of-atomic-formulas list-of-modifiers)
  `(EXPLORE (CONDITION 
             ,(if (equal? 1 (length list-of-atomic-formulas))
                  (first list-of-atomic-formulas)
                  (foldl (lambda (atomic-formula rest)
                           `(AND ,atomic-formula ,rest))
                         (first list-of-atomic-formulas)
                         (rest list-of-atomic-formulas))))
            ,@list-of-modifiers))

(define (xml-make-true-condition)
  '(TRUE))

(define (xml-make-explore-command list-of-atomic-formulas list-of-modifiers)
  (xml-make-command "EXPLORE" (list (xml-make-explore list-of-atomic-formulas list-of-modifiers))))

(define (xml-make-compare-command pol1 pol2 list-of-modifiers)
  (xml-make-command "COMPARE" (list `(COMPARE (POLICY1 ,pol1) 
                                              (POLICY2 ,pol2) 
                                              ,@list-of-modifiers))))

;;LISTS
(define (xml-make-generic-list list-name element-name attribute-name list-of-attribute-values)
  `(,list-name
    ,@(map (lambda (attribute-value)
             `(,element-name ((,attribute-name ,(if (symbol? attribute-value)
                                                    (symbol->string attribute-value)
                                                    attribute-value)))))
           list-of-attribute-values)))

;algs-list is a list of strings decribing the combine algorithms
(define (xml-make-combine-algs algs-list)
  (xml-make-generic-list 'COMBINE-ALGS 'COMBINE-ALG 'desc algs-list))

(define (xml-make-conjunct-chain conjs-list)
  (xml-make-generic-list 'CONJUNCTCHAIN 'CONJUNCT 'name conjs-list))

(define (xml-make-identifiers-list idents-list)
  (xml-make-generic-list 'IDENTIFIERS 'IDENTIFIER 'name idents-list))

(define (xml-make-relations-list relations-list)
  (xml-make-generic-list 'RELATIONS 'RELATION 'name relations-list))


(define (xml-make-quit)
  (xml-make-command "QUIT" empty))


(define (xml-make-and p1 p2)
  `(AND ,p1 ,p2))
(define (xml-make-or p1 p2)
  `(OR ,p1 ,p2))
(define (xml-make-implies p1 p2)
  `(IMPLIES (ANTE ,p1) (CONS ,p2)))
(define (xml-make-iff p1 p2)
  `(IFF ,p1 ,p2))
(define (xml-make-not p1)
  `(NOT ,p1))
 


;Takes a command type (string) and a list of children (x-exprs) and returns a margrave-command xexpr
(define (xml-make-command command-type list-of-children)
   `(MARGRAVE-COMMAND ((type ,command-type)) ,@list-of-children))