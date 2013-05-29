;    Copyright (c) 2009-2012 Brown University and Worcester Polytechnic Institute.
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

(require xml
         "helpers.rkt"
         rackunit
         (only-in srfi/13 
                  string-contains
                  string-drop
                  string-take))

(provide 
 (all-defined-out))

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
(define/contract (get-response-type maybe-doc)
  [(or/c document? element?) . -> . string?]
  (define ele (cond [(document? maybe-doc)
                     (document-element maybe-doc)]
                    [(element? maybe-doc) maybe-doc]
                    [else #f]))
  (if ele      
      (get-attribute-value ele 'type)
      ""))


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
;****************************************************************
;;Pretty Printing returned XML

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; xml->scenario consumes an xml element and produces an m-scenario 
; struct OR an m-unsat struct. If the XML given did not contain
; the proper sort of reply, this function returns #f.
(define/contract (xml->scenario xml-response)
  (element? . -> . (or/c m-unsat? m-scenario?))
  
  (define (handle-statistics ele)
    (define computed-max (get-attribute-value ele 'computed-max-size))
    (define user-provided-max (get-attribute-value ele 'user-max-size))
    (define used-max (get-attribute-value ele 'max-size))
    (define computed-max-num (string->number computed-max))
    (define user-provided-max-num (string->number user-provided-max))
    (define used-max-num (string->number used-max))
    (define warnings-element (get-child-element ele 'WARNINGS))
    (define used-element (get-child-element ele 'USED))    
    (define warnings (xml-set-element->list warnings-element))
    (define used (flatten-singleton-string-lists-in-map (xml-map-element->map used-element)))    
    (m-statistics computed-max-num user-provided-max-num used-max-num warnings used))
  
  ; Is this the right type of response?
  (define response-type (get-response-type xml-response))    
  (define model-element (get-child-element xml-response 'MODEL))
  
  (cond [(and (not (equal? response-type "model"))
              (not (equal? response-type "unsat")))
         ; Invalid response type
         #f]
        [(empty? model-element)                  
         ; Unsatisfiable response!
         (define statistics-element (get-child-element xml-response 'STATISTICS))         
         (define query-id (get-attribute-value statistics-element 'query-id))
         (m-unsat (handle-statistics statistics-element) query-id)]
        [else
         ; Satisfiable!           
         (define relation-elements (get-child-elements model-element 'RELATION))
         (define universe-element (get-child-element model-element 'UNIVERSE))
         (define model-size (string->number (get-attribute-value model-element 'size)))
         (define annotation-elements (get-child-elements model-element 'ANNOTATION))
         (define statistics-element (get-child-element xml-response 'STATISTICS))
         (define query-id (get-attribute-value statistics-element 'query-id))    
         
         (define mutable-atoms-hash (make-hash))
         
         (define (handle-atom ele)
           (define atom-element-name (pcdata-string (first (element-content ele))))
           (hash-set! mutable-atoms-hash atom-element-name #t)
           atom-element-name)
         
         (define (handle-tuple ele)
           (define atoms-elements (get-child-elements ele 'ATOM))
           (map handle-atom atoms-elements))
         
         (define (handle-relation ele)
           (define tuple-elements (get-child-elements ele 'TUPLE))
           (define reltype (get-attribute-value ele 'type))
           (define relation-is-sort (equal? "sort" reltype))
           (define relation-is-constant (equal? "constant" reltype))
           (define relation-is-function (equal? "function" reltype))
           (define relation-arity (string->number (get-attribute-value ele 'arity)))
           (define relation-name  (get-attribute-value ele 'name))   
           (m-relation relation-name 
                       (cond [relation-is-sort 'sort]
                             [relation-is-constant 'constant]
                             [relation-is-function 'function]
                             [(equal? (string-ref relation-name 0) #\$) 'skolem]
                             [else 'relation])
                       (map handle-tuple tuple-elements)))                    
         
         (define (handle-annotation ele)
           (define the-annotation (pcdata-string (first (element-content ele))))
           the-annotation)
         
         (define the-relations (map handle-relation relation-elements))
         (define the-sorts (filter (lambda (rel) (equal? 'sort (m-relation-reltype rel))) the-relations))
         (define the-skolems (filter (lambda (rel) (equal? 'skolem (m-relation-reltype rel))) the-relations))
         (define the-others (filter (lambda (rel) (or (equal? 'relation (m-relation-reltype rel))
                                                      (equal? 'constant (m-relation-reltype rel))
                                                      (equal? 'function (m-relation-reltype rel)))) the-relations))
         
         (m-scenario model-size 
                     (hash-keys mutable-atoms-hash)
                     the-sorts
                     the-skolems
                     the-others
                     (handle-statistics statistics-element) 
                     (map handle-annotation annotation-elements)
                     query-id)]))

(define (flatten-singleton-string-lists-in-map thehash)  
  (for/hash ([key (hash-keys thehash)])
    (define valuelist (hash-ref thehash key))
    (if (and (equal? (length valuelist) 1)
             (string? (first valuelist)))           
        (values key (string->number (first valuelist)))
        ; default to not changing the values
        (values key valuelist))))
; (flatten-singleton-string-lists-in-map (hash "A" '("1") "B" '("2" "3") "C" '() "D" '(5)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; m-scenario->string
; Consume an m-scenario and pretty-print it. 
(define/contract (m-scenario->string a-response #:brief [brief #f])
  [->* ((or/c m-unsat? m-scenario?))
       (#:brief boolean?)
       string?]
  
  (define buffer (open-output-string)) 
    
  ;;;;;;;;;;;;;;;;;;;;;;;;
  (define (print-statistics statistics)
    (unless (empty? (m-statistics-warnings statistics))   
      (cond [brief (write-string "*** WARNING!*** Margrave may not be able to guarantee completeness. Details omitted due to #:brief mode.\n" buffer)]
            [else
             (write-string "WARNING: Margrave may not be able to guarantee completeness:\n" buffer)            
             (for-each (lambda (warn) (write-string (string-append warn "\n") buffer))
                       (sort (m-statistics-warnings statistics)
                             string<=?))
             (write-string "Used these upper-bounds on sort sizes:\n" buffer)
             (write-string (pretty-print-hashtable (m-statistics-used statistics)) buffer)])))
  
  ;;;;;;;;;;;;;;;;;;;;;;;;
  (define/contract (internal-process-unsat an-unsat)
    [m-unsat? . -> . string?]
    ; Preamble
    (write-string "************** NO MORE SOLUTIONS FOUND! ****************\n" buffer)
    (print-statistics (m-unsat-statistics an-unsat))
    (write-string "********************************************************\n" buffer)
    (get-output-string buffer))
  
  ;;;;;;;;;;;;;;;;;;;;;;;;
  (define/contract (internal-process-scenario a-scenario)
    [m-scenario? . -> . string?]        
    ; Preamble
    (write-string (string-append "********* SOLUTION FOUND at size = " 
                                 (number->string (m-scenario-size a-scenario))
                                 " ******************\n") buffer)
    
    (define const-relations (filter (lambda (rel) (equal? 'constant (m-relation-reltype rel))) (m-scenario-relations a-scenario)))
    (define non-const-relations (filter (lambda (rel) (not (equal? 'constant (m-relation-reltype rel)))) (m-scenario-relations a-scenario)))
    (define ordered-non-const-relations (sort non-const-relations
                                              (lambda (r1 r2) (string<=? (m-relation-name r1) (m-relation-name r2)))))
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ; Decide on a name for each atom. Is it bound to a constant, etc.?
    (define atoms-with-names 
      (foldl (lambda (e sofar) 
               ; Remove Skolem prefix
               (define cxname (cond [(equal? "$" (string-take (m-relation-name e) 1))
                                     (string-drop (m-relation-name e) 1)]
                                    [else (m-relation-name e)]))
               ; Error if malformed relation
               (unless (equal? 1 (length (m-relation-tuples e)))
                 (error 'm-scenario->string (format "The constant or variable relation ~v contained ~v tuples. Expected only a single tuple." cxname (length (m-relation-tuples e)))))
               (define cxatom (first (first (m-relation-tuples e))))
               (cond [(hash-has-key? sofar cxatom)
                      (define sofar-name (hash-ref sofar cxatom))
                      (hash-set sofar cxatom (string-append sofar-name "=" cxname))]
                     [else (hash-set sofar cxatom cxname)]))
             (make-immutable-hash '())
             (append 
              (m-scenario-skolems a-scenario)
              const-relations)))  
    
    ; Atoms that have no name after that will just be printed in their raw form. E.g. "Subject#1"    
    (define atom-names (foldl (lambda (a sofar)
                                (cond [(hash-has-key? sofar a) sofar]
                                      [else (hash-set sofar a a)]))
                              atoms-with-names
                              (m-scenario-atoms a-scenario)))        
    (define atoms (m-scenario-atoms a-scenario))
  
    
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ; What sorts do each atom belong to?
    ; atom-sorts is a hash from string? to (setof string?)
    (define/contract (handle-atoms-sorting the-atoms sofar relname)
      [(listof string?) hash? string? . -> . hash?]
      (foldl (lambda (an-atom sofar-inner) 
               (cond [(hash-has-key? sofar-inner an-atom)
                      (hash-set sofar-inner an-atom (set-union (set relname) 
                                                               (hash-ref sofar-inner an-atom)))]
                     [else
                      (hash-set sofar-inner an-atom (set relname))])) 
             sofar
             the-atoms))
    (define atom-sorts (foldl (lambda (e sofar)
                                (define the-tuples (m-relation-tuples e))
                                (define relname (m-relation-name e))
                                ; Sorts are all unary
                                (define the-atoms (map first the-tuples))
                                (handle-atoms-sorting the-atoms sofar relname))
                              (make-immutable-hash '())
                              (m-scenario-sorts a-scenario)))
    
    
    ; TODO. Most specific! That info is lost when converting from XML...  
    ; !!! TODO
    
    ; !!! TODO: Why not use functions, too? E.g. Identify Atom#12 as f(c) if it is such.
    ; We already have a function to dereference terms...
    
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ; What atoms should be omitted?They must be
    ; (1) denoted by a constant
    ; (2) in only one sort (which must be the sort they were declared in!)
    (define omit-atoms    
      (foldl (lambda (rel sofar)
               (define the-atom (first (first (m-relation-tuples rel))))
               (define the-atom-sorts (hash-ref atom-sorts the-atom))
               (cond [(equal? (set-count the-atom-sorts) 1) (set-union sofar (set the-atom))]
                     [else sofar]))
             (set )
             const-relations))
    
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ; Print sort information for atoms
    ; unless the atom has been flagged for omission.
    (define/contract (print-atom-sorts an-atom)
      [string? . -> . any/c]
      (unless (set-member? omit-atoms an-atom)
        (write-string (hash-ref atom-names an-atom) buffer)
        (write-string ": " buffer)
        (define set-of-sorts (hash-ref atom-sorts an-atom))
        (define ordered-list-of-sorts (sort (set->list set-of-sorts) string<=?))
        (write-string (string-join ordered-list-of-sorts ", ") buffer)
        (write-string "\n" buffer)))
    ; sort alphabetically by displayed name, not actual atom string:
    (for-each print-atom-sorts (sort (hash-keys atom-sorts) 
                                     (lambda (a1 a2) (string<=? (hash-ref atom-names a1)
                                                                (hash-ref atom-names a2)))))
    (unless (set-empty? omit-atoms)
      (define named-omit-atoms (map (lambda (a) (hash-ref atom-names a))
                                    (set->list omit-atoms)))
      (define sorted-named-omit-atoms (sort named-omit-atoms string<=?))
      (define pretty-omit-atoms (string-join sorted-named-omit-atoms ", "))
      (write-string "----------------------------------------\n" buffer)
      (write-string (format "Omitted sorts for atoms denoted by constants that did not appear outside their native type:~n~a~n" pretty-omit-atoms) buffer))
    
    (write-string "----------------------------------------\n" buffer)
    
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ; Print non-sort, non-constant relation membership information for atoms  
    (define/contract (print-tuple tup)
      [(listof string?) . -> . any/c]
      (write-string "<" buffer)
      (define translated-atoms-list (map (lambda (a) (hash-ref atom-names a)) tup))    
      (write-string (string-join translated-atoms-list ", ") buffer)
      (write-string "> " buffer))
    (define/contract (print-relation rel)
      [m-relation? . -> . any/c]
      (write-string (m-relation-name rel) buffer)
      (write-string ": { " buffer)
      (for-each print-tuple (m-relation-tuples rel))
      (write-string " }\n" buffer))
    (for-each print-relation ordered-non-const-relations)
    
    (write-string "----------------------------------------\n" buffer)
    
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ; End the scenario with annotations and statistics
    (define annotations (m-scenario-annotations a-scenario))
    (unless (or brief (empty? annotations))
      (write-string "\n    -> The scenario also contained these annotations:\n" buffer)
      (for-each (lambda (ann) (write-string (string-append ann "\n") buffer))
                annotations))
    
    ; Call this even if in brief mode. Print a one-line warning if no completeness guarantee.
    (print-statistics (m-scenario-statistics a-scenario))
    
    (write-string "********************************************************\n\n" buffer)        
    (get-output-string buffer))
  ; ^ End of internal func. to handle scenarios
  
  ; Is this input scenario or an unsatisfiable response?
  (cond [(m-scenario? a-response) 
         (internal-process-scenario a-response)]
        [else 
         (internal-process-unsat a-response)]))

(define (success->policy-sexpr response)
  (define polelement (get-child-element (maybe-document->element response) 'POLICY))
  (cond [(empty? polelement) #f]
        [else (get-attribute-value polelement 'sexpr)]))
(define (success->theory-sexpr response)
  (define polelement (get-child-element (maybe-document->element response) 'THEORY))
  (cond [(empty? polelement) #f]
        [else (get-attribute-value polelement 'sexpr)]))


; Get value for attribute name-symbol of element ele
; Element -> Symbol -> String
(define/contract (get-attribute-value ele name-symbol)
  [element? symbol? . -> . string?]
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

(define (element-has-children-named element name-symb-or-str)
  (not (empty? (get-child-elements element name-symb-or-str))))

(define (get-pc-data elem)
  (pcdata-string (first (element-content elem))))

(define (print-statistics stat-element) 
  (define string-buffer (open-output-string))
  (define (write s)
    (write-string s string-buffer))
  
  (define computed-max (get-attribute-value stat-element 'computed-max-size))
  (define user-provided-max (get-attribute-value stat-element 'user-max-size))
  (define used-max (get-attribute-value stat-element 'max-size))
  (define computed-max-num (string->number computed-max))
  (define user-provided-max-num (string->number user-provided-max))
  (define used-max-num (string->number used-max))
  (define warnings-element (get-child-element stat-element 'WARNINGS))
  (define used-element (get-child-element stat-element 'USED))
  
  (when (element-has-children-named warnings-element 'ITEM)    
    (write "WARNING: Margrave may not be able to guarantee completeness:\n")            
    (write (format "~a~n" (xml-set-element->list warnings-element))))
  
  (define used-hashtable (xml-map-element->map used-element))
  (write "Used these upper-bounds on sort sizes:\n")
  (write (pretty-print-hashtable used-hashtable))
  
  (get-output-string string-buffer))


;************ Pretty Print Info *******************

;Pass this function an xml Document with a MARGRAVE-RESPONSE root element
; document? or element? or string? -> string?
(define (response->string the-response)
  (define result 
    (cond [(false? the-response)
           "Engine stopped!"]
          
          [(document? the-response)
           (response->string (document-element the-response))]
          
          [(element? the-response)
           (let* ([type (get-attribute-value the-response 'type)])
             ;Debugging: (display (xexpr->string (xml->xexpr (document-element response-doc))))
             (cond [(equal? type "model") (m-scenario->string (xml->scenario the-response))] 
                   [(equal? type "sysinfo") (pretty-print-sys-info-xml the-response)]
                   [(equal? type "collection-info") (pretty-print-collection-info-xml the-response)]
                   [(equal? type "vocabulary-info") (pretty-print-vocab-info-xml the-response)]
                   [(equal? type "error") (pretty-print-error-xml the-response)]
                   [(equal? type "exception") (pretty-print-exception-xml the-response)]
                   [(equal? type "explore-result") (pretty-print-explore-xml the-response)]
                   [(equal? type "unsat") (m-scenario->string (xml->scenario the-response))]
                   [(equal? type "boolean") (pretty-print-boolean-xml the-response)]
                   ; [(equal? type "string") (pretty-print-string-xml the-response)]
                   [(equal? type "string") (xml-string-response->string the-response)]
                   [(equal? type "set") (pretty-print-set-xml the-response)]
                   [(equal? type "list") (pretty-print-list-xml the-response)]
                   [(equal? type "map") (pretty-print-map-xml the-response)]
                   [(equal? type "success") "Success!\n"]))]
          
          [(string? the-response) the-response]
          
          [else the-response]))
  ; space here to add post-formatting if desired
  result)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; xml-string-response
; Takes an engine reply containing a string and produces the string.
(define/contract (xml-string-response->string xml-response)
  [(or/c document? element?) . -> . string?]
  (define response-element (maybe-document->element xml-response))
  (define string-element (get-child-element response-element 'string))
  ; Don't insert ~n, because the results of this func are passed to (e.g.) string->number
  (get-attribute-value string-element 'value))

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
  (define response-element (maybe-document->element response-element-or-document))
  (define set-element (get-child-element response-element 'SET))
  (xml-set-element->list set-element))

(define (xml-set-element->list set-element)
  (define item-elements (get-child-elements set-element 'ITEM))
  (map (lambda (item-element)             
         (pcdata-string (first (element-content item-element))))
       item-elements))

; XML --> string
; Extracts the extra output info from the response. This element contains
; anything added to System.out while the engine was running.
(define (get-response-extra-out response-element-or-document)  
  (let* ([response-element (maybe-document->element response-element-or-document)]
         [extra-element (get-child-element response-element 'EXTRA-OUT)])   
    ; may be no EXTRA-OUT element
    ; also may be there but empty. account for both
    (if (or (empty? extra-element) (empty? (element-content extra-element)))
        ""
        (fold-append-with-spaces (map pcdata-string (filter pcdata? (element-content extra-element)))))))

; XML --> string
; Extracts the extra error info from the response. This element contains
; anything added to System.err while the engine was running.
(define (get-response-extra-err response-element-or-document)  
  (let* ([response-element (maybe-document->element response-element-or-document)]
         [extra-element (get-child-element response-element 'EXTRA-ERR)])    
    (if (or (empty? extra-element) (empty? (element-content extra-element)))
        ""
        (fold-append-with-spaces (map pcdata-string (filter pcdata? (element-content extra-element)))))))


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
  (define response-element (maybe-document->element response-element-or-document))
  (define map-element (get-child-element response-element 'MAP))  
  (xml-map-element->map map-element))

(define (xml-map-element->map map-element)
  (define entry-elements (get-child-elements map-element 'ENTRY))
  (define mut-hashtable (make-hash))
  
  (define (process-entry entry-element)
    (define entry-key (get-attribute-value entry-element 'key))
    (define entry-values (get-child-elements entry-element 'value))
    (define value-list (map (lambda (val) (pcdata-string (first (element-content val))))
                            entry-values))
    (hash-set! mut-hashtable
               entry-key                              
               value-list))
  
  (for-each process-entry entry-elements)
  mut-hashtable)

; XML <MARGRAVE-RESPONSE type="map">  --> string
(define (pretty-print-map-xml element)
  (let ([the-hashtable (xml-map-response->map element)])
    (pretty-print-hashtable the-hashtable)))

; element -> string
(define (pretty-print-explore-xml element)
  (let* ((string-buffer (open-output-string))
         (result-element (get-child-element element 'result-handle)))
    (begin 
      ; Omit result handle since only zero is ever returned (for the moment).
      (write-string (string-append "Query created successfully.\n") string-buffer) ;Result handle was: " (get-pc-data result-element) "\n") string-buffer)
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
;************************************************************************
;************************************************************************
;************************************************************************

;;These functions return x-exprs for commands, formulas, etc.

(define (xml-make-info-id id)
  `(INFO ((id ,(->string id)))))

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

(define (xml-make-load-xacml polid fn sfn)
  (xml-make-command "LOAD XACML POLICY" (list (xml-make-xacml-load polid fn sfn) 
                                              (xml-make-policy-identifier polid))))

(define (xml-make-load-sqs polid fn)
  (xml-make-command "LOAD SQS POLICY" (list (xml-make-sqs-load polid fn)
                                            (xml-make-policy-identifier polid))))


(define (xml-make-xacml-load polid fn sfn)
  `(LOAD ((file-name ,(->string fn)) (schema-file-name ,(->string sfn))) ))

(define (xml-make-sqs-load polid fn)
  `(LOAD ((file-name ,(->string fn)))))

(define (xml-make-get-rules get-type polid decid-str)
  (if (not (equal? "" decid-str))
      `(GET-INFO ((type ,(->string get-type))) ,(xml-make-policy-identifier (->string polid)) ,(xml-make-decision decid-str))
      `(GET-INFO ((type ,(->string get-type))) ,(xml-make-policy-identifier (->string polid)))))

(define (xml-make-policy-identifier policy-name)
  `(POLICY-IDENTIFIER ((pname ,(->string policy-name)))))

(define (xml-make-vocab-identifier vocab-name)
  `(VOCAB-IDENTIFIER ((vname ,(->string vocab-name)))))

(define (xml-make-parent-identifier parent-name)
  `(PARENT-IDENTIFIER ((name ,(->string parent-name)))))

(define (xml-make-child-identifier child-name)
  `(CHILD-IDENTIFIER ((name ,(->string child-name)))))

(define (xml-make-predicate pred-name)
  `(PREDICATE ((name ,(->string pred-name)))))

(define (xml-make-target formula)
  `(TARGET ,formula))

(define (xml-make-rule rule-name dtype rule-list)
  `(RULE ((name ,(->string rule-name))) ,dtype ,rule-list))

;rule-list is of the form ((!Conflicted s r) (ReadPaper a) (Paper r)), or true
(define (xml-make-rule-list orig-rule-list)
  ; (printf "~a~n" rule-list)
  
  ; Don't keep un-necessary 'true
  (let ([rule-list (filter (lambda (relation) (not (equal? 'true relation)))
                           orig-rule-list)])
    
    (if (empty? rule-list)
        `(RELATIONS) 
        `(RELATIONS ,@(map (lambda (relation)
                             (let* ((relation-name (->string (first relation)))
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

(define (xml-make-decision-type idb-name var-order-list)
  `(DECISION-TYPE ((type ,(->string idb-name)))
                  ,@(map xml-make-id-element var-order-list)))

(define (xml-make-decision decision)
  `(DECISION ((name ,(->string decision)))))

(define (xml-make-sort sort-name)
  `(SORT ((name ,(->string sort-name)))))

(define (xml-make-subsort parent child)
  `(SUBSORT ((parent ,(->string parent)) (child ,(->string child)))))

(define (xml-make-type-with-subs tname clist)
  `(SORT-WITH-CHILDREN ((name ,(->string tname))) ,@(map xml-make-sort clist)))

(define (xml-make-request-var rvname rvsort)
  `(REQUESTVAR ((name ,(->string rvname)) (sort ,(->string rvsort)))))

(define (xml-make-constraint constraint-type list-of-relations)
  `(CONSTRAINT ((type ,(->string constraint-type))) ,(xml-make-relations-list list-of-relations)))

(define (xml-make-custom-fmla-constraint fmlaxexpr)
  `(CONSTRAINT ((type "FORMULA")) ,fmlaxexpr))

(define (xml-make-count id)
  `(COUNT ((id ,(->string id)))))

(define (xml-make-count-command id)
  (xml-make-command "COUNT" (list (xml-make-count id))))

(define (xml-make-count-with-size id size)
  `(COUNT (,id ,size)))

(define (xml-make-count-with-size-command id size)
  (xml-make-command "COUNT" (list (xml-make-count-with-size id size))))

(define (xml-make-size size)
  `(size ,(->string size)))


(define (xml-make-file-name file-name)
  `(file-name ,(->string file-name)))

(define (xml-make-schema-file-name schema-file-name)
  `(schema-file-name ,(->string schema-file-name)))

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

(define (xml-make-id-element id)  
  `(ID ((id ,(->string id)))))

(define (xml-make-get type id options)
  `(SHOW (,type (id ,id)) ,@options))

(define (xml-make-reset id)
  `(RESET ((id,id))))

(define (xml-make-get-command type id options)
  (xml-make-command "SHOW" (list (xml-make-get type id options))))

(define (xml-make-reset-command id)
  (xml-make-command "RESET" (list (xml-make-reset id))))

(define (xml-make-under list-of-policies)
  `(UNDER ,@list-of-policies))

(define (xml-make-create-policy-leaf policy vocab)
  `(CREATE-POLICY-LEAF ,policy ,vocab))

(define (xml-make-create-policy-leaf-command policy vocab)
  (xml-make-command "CREATE POLICY LEAF" (list (xml-make-create-policy-leaf policy vocab))))

(define (xml-make-is-possible xml-id)
  `(IS-POSSIBLE ((id ,xml-id))))

(define (xml-make-is-possible-command xml-id)
  (xml-make-command "IS-POSSIBLE" (list (xml-make-is-possible xml-id))))

(define (xml-make-is-guaranteed id)
  `(IS-GUARANTEED ((id ,id))))

(define (xml-make-is-guaranteed-command id)
  (xml-make-command "IS-GUARANTEED" (list (xml-make-is-guaranteed id))))

(define (xml-make-include list-of-atomic-formulas)
  `(INCLUDE ,@list-of-atomic-formulas))

(define (xml-make-show-realized-command id childlist)
  (xml-make-command "SHOW" (list `(SHOW ((type "REALIZED") (id ,id)) ,@childlist))))

(define (xml-make-show-unrealized-command id childlist)
  (xml-make-command "SHOW" (list `(SHOW ((type "UNREALIZED") (id ,id)) ,@childlist))))

(define (xml-make-forcases the-cases)
  `(FORCASES ,@the-cases))

;(define (xml-make-tupling) ;Just defaults to true, if you don't want tupling don't include
;  `(TUPLING ((value "true")))) ;Value isn't actually used right now. Perhaps useless?

(define (xml-make-debug debug-level)
  `(DEBUG ((debug-level ,(->string debug-level)))))

(define (xml-make-ceilings list-of-ceiling-elements)
  `(CEILINGS ,@list-of-ceiling-elements))

(define (xml-make-a-ceiling sort-name ceiling-level)
  `(CEILING ((sort ,(->string sort-name)) 
             (value ,(->string ceiling-level)))))

(define (xml-make-a-ceiling-from-pair ceil-pair)
  (xml-make-a-ceiling (first ceil-pair) (second ceil-pair)))

;Atomic Formulas
(define (xml-make-equals-formula t1 t2)
  `(EQUALS ,t1 ,t2))

(define (xml-make-isa-formula term s f)
  ;`(ISA ((var ,(->string v)) (sort ,(->string s))) ,f))
  `(ISA ((sort ,(->string s))) (TERM ,term) (FORMULA ,f)))

(define (xml-make-variable-declaration v s)
  `(VARIABLE-DECLARATION ((sort ,(->string s)) (varname ,(->string v)))))

(define (xml-make-variable-term id)
  `(VARIABLE-TERM ((id ,(->string id)))))

(define (xml-make-constant-term id)
  `(CONSTANT-TERM ((id ,(->string id)))))

(define (xml-make-function-term fid subterm-xml-list)
  ;(printf "Function term: ~a ~a ~n" fid subterm-xml-list)
  `(FUNCTION-TERM ((func ,(->string fid))) ,@subterm-xml-list))

(define (xml-make-atomic-formula compound-id-list-maybe term-list)
  (define compound-id-list
    (if (list? compound-id-list-maybe)
        compound-id-list-maybe
        (list compound-id-list-maybe)))
  `(ATOMIC-FORMULA (RELATION-NAME ,@(map xml-make-id-element compound-id-list))
                   (TERMS ,@term-list)) ) 


(define (xml-make-constant-decl cname ctype)
  `(CONSTANT ((name ,(->string cname)) (type ,(->string ctype)))))
(define (xml-make-function-decl fname ftlist)
  `(FUNCTION ((name ,(->string fname))) ,ftlist))

(define (xml-make-fa typelist)
  `(FA ,@(map xml-make-id-element typelist)))
(define (xml-make-over under-type over-list)
  `(OVERRIDES ((decision ,(->string under-type))) ,@(map xml-make-id-element over-list)))

(define (xml-make-comb-list comb-list)
  `(COMB-LIST ,@comb-list))


;(define (xml-make-atomic-formula-n relName xml-identifier-list)
;  `(ATOMIC-FORMULA-N ((relation-name ,(->string relName))) ,xml-identifier-list))  

;(define (xml-make-atomic-formula-y collName relName xml-identifier-list)
;(printf "~n~nY: ~a ~a ~n" collName relName)
;  (if (empty? xml-identifier-list)
;      `(ATOMIC-FORMULA-Y ((collection-name ,(->string collName)) (relation-name ,(->string relName))))
;      `(ATOMIC-FORMULA-Y ((collection-name ,(->string collName)) (relation-name ,(->string relName))) ,xml-identifier-list))) 

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
(define (xml-make-explore query-id free-vars-list list-of-atomic-formulas list-of-modifiers)
  `(EXPLORE ((id ,query-id))
            (CONDITION 
             ,(if (equal? 1 (length list-of-atomic-formulas))
                  (first list-of-atomic-formulas)
                  (foldl (lambda (atomic-formula rest)
                           `(AND ,atomic-formula ,rest))
                         (first list-of-atomic-formulas)
                         (rest list-of-atomic-formulas))))
            ,(xml-make-publish free-vars-list)
            ,@list-of-modifiers))

(define (xml-make-true-condition)
  '(TRUE))

(define (xml-make-false-condition)
  '(FALSE))

(define (xml-make-publish list-of-identifiers)
  `(PUBLISH ,@list-of-identifiers))

(define (xml-make-explore-command query-id free-vars-list list-of-atomic-formulas list-of-modifiers)
  (xml-make-command "EXPLORE" (list (xml-make-explore query-id free-vars-list list-of-atomic-formulas list-of-modifiers))))

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

;(define (xml-make-relations-list relations-list)
;  (xml-make-generic-list 'RELATIONS 'RELATION 'name relations-list))

(define (xml-make-relation relname)
  `(RELATION ((name ,(->string relname)))))
(define (xml-make-relations-list relations-list)
  `(RELATIONS ,@(map xml-make-relation relations-list)))


(define (xml-make-quit)
  (xml-make-command "QUIT" empty))

(define (xml-make-forall x s f)
  `(FORALL ((var ,(->string x)) (sort ,(->string s))) ,f))
(define (xml-make-exists x s f)
  `(EXISTS ((var ,(->string x)) (sort ,(->string s))) ,f))


(define (xml-make-and p1 p2)
  `(AND ,p1 ,p2))

(define (xml-make-and* conjuncts)
  (cond 
    [(empty? conjuncts) 
     (xml-make-true-condition)]
    [(= (length conjuncts) 1)
     (first conjuncts)]
    [(= (length conjuncts) 2)
     (xml-make-and (first conjuncts) (second conjuncts))]
    [else (xml-make-and (first conjuncts) (xml-make-and* (rest conjuncts)))]))

(define (xml-make-or p1 p2)
  `(OR ,p1 ,p2))
(define (xml-make-or* disjuncts)
  (cond 
    [(empty? disjuncts) 
     (xml-make-false-condition)]
    [(= (length disjuncts) 1)
     (first disjuncts)]
    [(= (length disjuncts) 2)
     (xml-make-or (first disjuncts) (second disjuncts))]
    [else (xml-make-or (first disjuncts) (xml-make-or* (rest disjuncts)))]))

(define (xml-make-implies p1 p2)
  `(IMPLIES (ANTE ,p1) (CONS ,p2)))
(define (xml-make-iff p1 p2)
  `(IFF ,p1 ,p2))
(define (xml-make-not p1)
  `(NOT ,p1))

;Takes a command type (string) and a list of children (x-exprs) and returns a margrave-command xexpr
(define (xml-make-command command-type list-of-children)
  `(MARGRAVE-COMMAND ((type ,command-type)) ,@list-of-children))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define/contract 
  (m-type->cmd vocab-id atype)  
  [string? m-type? . -> . xexpr?]
  (cond [(empty? (m-type-child-names atype))            
         (xml-make-command "ADD" (list (xml-make-vocab-identifier vocab-id)
                                       (xml-make-sort (m-type-name atype))))]
        [else                                   
         (xml-make-command "ADD" (list (xml-make-vocab-identifier vocab-id) 
                                       (xml-make-type-with-subs (m-type-name atype) (m-type-child-names atype))))]))


(define/contract 
  (m-predicate->cmd vocab-id apred)  
  [string? m-predicate? . -> . xexpr?]
  (xml-make-command "ADD" (list (xml-make-vocab-identifier vocab-id) 
                                (xml-make-predicate (m-predicate-name apred)) 
                                (xml-make-relations-list (m-predicate-arity apred)))))

(define/contract 
  (m-constant->cmd vocab-id aconst)  
  [string? m-constant? . -> . xexpr?]
  (xml-make-command "ADD" (list (xml-make-vocab-identifier vocab-id) 
                                (xml-make-constant-decl (m-constant-name aconst) 
                                                        (m-constant-type aconst)))))

(define/contract 
  (m-function->cmd vocab-id afunc)  
  [string? m-function? . -> . xexpr?]
  (xml-make-command "ADD" (list (xml-make-vocab-identifier vocab-id) 
                                (xml-make-function-decl (m-function-name afunc)
                                                        (xml-make-relations-list (append (m-function-arity afunc)
                                                                                         (list (m-function-result afunc))))))))  


; Is this a dotted identifier? Must have a . somewhere.
(define (dotted-id? sym)
  (and (symbol? sym)
       (string-contains (symbol->string sym) ".")))

; Return list of components
; (One or more non-dot characters)
(define (handle-dotted-pred sym)
  (define str (symbol->string sym))
  (regexp-match* #rx"[^.]+" str))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Call m-term->xexpr instead of m-term? in the patterns below.
; Reason: m-term? does not throw errors. Recursively calling self
; allows for better error messages. E.g.:
; "Incorrect term expression 2"
; versus "Incorrect term expression (f 'c (g 2))"
(define/contract (m-term->xexpr sexpr)
  [any/c . -> . xexpr?]  
  (match sexpr
    [(or `(,(? valid-function? funcid) ,@(list (? m-term->xexpr terms) ...))
         (syntax-list-quasi ,(? valid-function? funcid) ,@(list (? m-term->xexpr terms) ...)))
     (xml-make-function-term (->string funcid) 
                             (map m-term->xexpr terms))]
    ; Will be '(quote constid). Grab SECOND in that pair (unless new form constant with $)
    ; extract-constant-id will take this into account
    [(? valid-constant? cid) 
       (xml-make-constant-term (->string (extract-constant-id sexpr)))]
    [(? valid-variable? vid) (xml-make-variable-term (->string sexpr))]
    [else (margrave-error "Incorrect term expression" sexpr)]))    

; Note some clauses have extra calls to validity checker functions.
; These special functions will throw appropriate errors for invalid 
; syntax, returning void.
(define/contract (m-formula->xexpr sexpr #:syntax [src #f])
  [any/c . -> . (or/c xexpr? void?)]  
  
  (match sexpr 
    [(maybe-identifier true)
     (xml-make-true-condition)]        
    [(maybe-identifier false)
     (xml-make-false-condition)]
    
    [(m-op-case = t1 t2)
     (xml-make-equals-formula (m-term->xexpr t1) (m-term->xexpr t2))]    
    [(m-op-case and args ...)
     (xml-make-and* (map m-formula->xexpr args))]    
    [(m-op-case or args ...)
     (xml-make-or* (map m-formula->xexpr args))]
    
    [(m-op-case implies arg1 arg2)
     (xml-make-implies (m-formula->xexpr arg1) (m-formula->xexpr arg2))]   
    
    [(m-op-case iff arg1 arg2)
     (xml-make-iff (m-formula->xexpr arg1) (m-formula->xexpr arg2))]   
    
    [(m-op-case not arg)
     (xml-make-not (m-formula->xexpr arg))]   
    
    [(m-op-case forall vname sname fmla)
     (valid-variable?/err vname)
     (valid-sort?/err sname)
     (xml-make-forall vname sname (m-formula->xexpr fmla))] 
    
    [(m-op-case exists vname sname fmla)
     (valid-variable?/err vname)
     (valid-sort?/err sname)
     (xml-make-exists vname sname (m-formula->xexpr fmla))]           
    
    [(m-op-case isa t sname fmla)     
     (valid-sort?/err sname)
     (xml-make-isa-formula (m-term->xexpr t) sname (m-formula->xexpr fmla))]      
    
    ; Don't require vars.
    [(maybe-syntax-list-quasi ,(maybe-syntax-list-quasi ,@(list pids ... idbname)) ,@(list terms ...))    
     ;(valid-predicate?/err idbname)
     ; the above check prevents reference to prior queries when the prior query name is capitalized.
     ;; TODO finalize lexical spec     
     (xml-make-atomic-formula (append pids (list idbname))
                              (map m-term->xexpr terms))]
    
    
    [(maybe-syntax-list-quasi ,dbname ,term0 ,@(list terms ...))
     (valid-sort-or-predicate?/err dbname)
     (cond
       [(and (valid-sort? dbname) 
             (empty? terms)) ; only one term!             
        (xml-make-isa-formula (m-term->xexpr term0) dbname (xml-make-true-condition))]
       [(valid-sort? dbname) 
        (margrave-error "Atomic formulas (S t), where S is a sort name, must contain only a single term t." sexpr)]
       
       [else (xml-make-atomic-formula (list dbname)
                                      (map m-term->xexpr (cons term0 terms)))])]
    [else (margrave-error "Incorrect formula expression" sexpr)]))


; Avoid duplicate code. Defer to m-formula->xexpr
(define (m-formula? sexpr)
  (with-handlers ([(lambda (e) (exn:fail:syntax? e))
                   (lambda (e) #f)]
                  [(lambda (e) (exn:fail:user? e))
                   (lambda (e) #f)])
    (if (m-formula->xexpr sexpr)
        #t
        #f)))

; Avoid duplicate code. Defer to m-term->xml
(define (m-term? sexpr)
  (with-handlers ([(lambda (e) (exn:fail:syntax? e))
                   (lambda (e) #f)]
                  [(lambda (e) (exn:fail:user? e))
                   (lambda (e) #f)])
    (if (m-term->xexpr sexpr)
        #t
        #f)))

(define (make-axiom-command vocab axiom)
  (define vocab-xexpr (xml-make-vocab-identifier vocab))
  (xml-make-command "ADD" (list vocab-xexpr (m-axiom->xexpr axiom))))

(define (m-axiom->xexpr axiom)           
  (match axiom
    [(m-op-case atmostone-all id)
     (valid-sort-or-predicate?/err id)
     (xml-make-constraint 'ATMOSTONE-ALL (list id))]        
    [(m-op-case atmostone id)
     (valid-sort-or-predicate?/err id)
     (xml-make-constraint 'ATMOSTONE (list id))]
    
    [(m-op-case singleton-all id)
     (valid-sort-or-predicate?/err id)
     (xml-make-constraint 'SINGLETON-ALL (list id))]        
    [(m-op-case singleton id)
     (valid-sort-or-predicate?/err id)
     (xml-make-constraint 'SINGLETON (list id))]
    
    [(m-op-case nonempty-all id)
     (valid-sort-or-predicate?/err id)
     (xml-make-constraint 'NONEMPTY-ALL (list id))]        
    [(m-op-case nonempty id)
     (valid-sort-or-predicate?/err id)
     (xml-make-constraint 'NONEMPTY (list id))]     
    
    [(m-op-case abstract id)
     (valid-sort-or-predicate?/err id)
     (xml-make-constraint 'ABSTRACT (list id))]
    
    [(m-op-case partial-function id)
     (valid-sort-or-predicate?/err id)
     (xml-make-constraint 'PARTIAL-FUNCTION (list id))]       
    
    [(m-op-case total-relation id)
     (valid-predicate?/err id)
     (xml-make-constraint 'TOTAL-RELATION (list id))]  
    
    
    ; Should allow this to be NON-SORT EDBs of comparable arities only. (Sorts have the hierarchy innately.)
    ; parent child
    [(m-op-case subset id1 id2)
     (valid-predicate?/err id1)
     (valid-predicate?/err id2)
     (xml-make-constraint 'SUBSET (list id1 id2))]
    
    ; (constants-cover S) --- Everything in S must be equal to one of the constants of sort S (or subsort S' of S)
    [(m-op-case constants-cover id) 
     (xml-make-constraint 'CONSTANTS-COVER (list id))]
    
    ; (constants-neq 'c 'd) --- c and d never denote the same element
    [(m-op-case constants-neq id1 id2) 
     `(CONSTRAINT ((type "CONSTANTS-NEQ")) ,(xml-make-relations-list (list (extract-constant-id id1) (extract-constant-id id2))))]
    
    
    [(m-op-case disjoint pred1 pred2) 
     (valid-predicate?/err pred1)
     (valid-predicate?/err pred2)
     `(CONSTRAINT ((type "DISJOINT")) ,(xml-make-relations-list (list pred1 pred2)))]
        
    ; (constants-neq-all S) --- All constants of sort S are pairwise non-equal in all models.
    [(m-op-case constants-neq-all id) 
     (xml-make-constraint 'CONSTANTS-NEQ-ALL (list id))]
    
    [(m-op-case formula fmla)
     ; Allow m-formula->xexpr to give a more detailed error.
     ;(unless (m-formula? fmla)
     ;  (margrave-error "The formula axiom did not contain a valid formula." axiom))
     (xml-make-custom-fmla-constraint (m-formula->xexpr fmla))]
    
    [else (margrave-error "The axiom was neither a formula nor a constraint declaration" axiom)]))



(define (m-axiom? sexpr)
  (with-handlers ([(lambda (e) (exn:fail:syntax? e))
                   (lambda (e) #f)]
                  [(lambda (e) (exn:fail:user? e))
                   (lambda (e) #f)])
    (if (m-axiom->xexpr sexpr)
        #t
        #f)))
