#lang racket/gui

(provide mg-model% apply-model apply-model/pos)

(require "modelgraph.rkt" "visxml.rkt" xml "../../margrave-xml.rkt")

; Checks to see if a named entity matches a specific label (ipsrc or ipdest, generally)
; This is what we're looking for:
; <RELATION name="whatever">
;   <TUPLE>
;     <ATOM>ipsrc</ATOM>
;   </TUPLE>
; </RELATION>
(define (check-src-dest xml name label)
  (and (element? xml)
       (and (symbol=? (element-name xml) 'RELATION)
            (and (string=? (get-attribute-value xml 'name) name)
                 (string=? (get-pc-data (get-child-element (get-child-element xml 'TUPLE) 'ATOM)) label)))))

; Returns a list of firewall policy decisions for a named entity
; This is from IDBOUTPUT, and we have to parse the string because the info we need is
; just in the pcdata of an <ANNOTATION> element
(define (get-fwps loa pname)
  (cond [(empty? loa) empty]
        [else
         (let ([ss (regexp-split #rx":" (get-pc-data (first loa))) ])
           (if (string=? (first ss) pname)
               (cons (hash-ref results-hash (string->symbol (first (regexp-split #rx" is" (second ss))))) (get-fwps (rest loa) pname))
               (get-fwps (rest loa) pname))
           )
         ]))

; Begins at start and tries to find a path exhausting every node in lon. 
(define (edge-path mg start lon)
  (cond [(empty? lon) #f]
        [(= (length lon) 1)
         (begin
           (print (send start get-name))
           (print (send (first lon) get-name))
         (send (send mg find-edge start (first lon)) set-active! #t)
         (send (send mg find-edge start (first lon)) set-blocked! (list? (memq result-deny (send start get-results)))))]
        [else
         (let ([next (first (filter (lambda (n) (send mg find-edge start n)) lon))])
           (begin
             (send (send mg find-edge start next) set-active! #t)
             (send (send mg find-edge start next) set-blocked! (list? (memq result-deny (send start get-results))))
             (edge-path mg next (remv next lon))))]))

; This class stores the xml model and provides functions for deriving model information from xml data. 
(define mg-model%
  (class object%
    (init-field
     [keyword-map (make-hash)] ; Not every policy will use the same vocabulary
     [xml null]
     )
    
    ; Returns the list of policy decisions made by that entity or empty
    ; if the entity was not involved in the model
    (define/public (get-entity-data policyname)
      (get-fwps (get-child-elements xml 'ANNOTATION) policyname))
    
    ; Updates a modelgraph so the edges have correct active and blocked status
    ; It must return the model. 
    (define/public (set-edge-data mg)
      (let ([srcl (filter (lambda (n) (send n is-source?)) (send mg get-nodes))]
            [destl (filter (lambda (n) (send n is-dest?)) (send mg get-nodes))]
            [active-ents (filter (lambda (n) (not (empty? (send n get-results)))) (send mg get-nodes))])
        (if (and (not (empty? srcl)) (not (empty? destl)))
        (begin
          (edge-path mg (first srcl) (append active-ents (list (first destl))) )
          mg) mg)
        ))
    
    ; Returns true if the supplied entity is the src host
    (define/public (is-src? entname)
      (ormap (lambda (x) (check-src-dest x entname (hash-ref keyword-map 'ipsrc))) (element-content xml)))
    
    ; Returns true if the supplied entity is the desination
    (define/public (is-dest? entname)
      (ormap (lambda (x) (check-src-dest x entname (hash-ref keyword-map 'ipdest))) (element-content xml)))
    
    (define/public (get-xml) xml)
    
    (super-new)
    ))

; Helper functions for apply-model

; Consumes a netgraph-edge and a model
; Returns a modelgraph-edge
(define (convert-edge e model nodemap)
  (new modelgraph-edge%
       [from (hash-ref nodemap (send e get-from))]
       [to (hash-ref nodemap (send e get-to))]
       [active #f]
       [blocked #f]
       ))

; Consumes a netgraph-node and a model
; Returns a modelgraph-node
(define (convert-node n model nodemap)
  (let ([newnode  
         (new modelgraph-node%
              [name (send n get-name)]
              [type (send n get-type)]       
              [policy (send n get-policy)]
              [vocabname (send n get-vocabname)]       
              [subgraph (if (null? (send n get-subgraph)) null (apply-model (send n get-subgraph) model))]       
              [source? (send model is-src? (send n get-vocabname))]
              [dest? (send model is-dest? (send n get-vocabname))]       
              [results empty])])
    (begin 
      (hash-set! nodemap n newnode)
      newnode)))    

; Consumes a pos-netgraph-node and a model
; Returns a pos-modelgraph-node
(define (convert-node/pos n model nodemap)
  (let ([newnode
         (new pos-modelgraph-node%
              [name (send n get-name)]
              [type (send n get-type)]
              [policy (send n get-policy)]
              [vocabname (send n get-vocabname)]
              [subgraph (if (null? (send n get-subgraph)) null (apply-model/pos (send n get-subgraph) model))]   
              [results (if (null? (send n get-policy)) empty (send model get-entity-data (send n get-policy)))]
              [source? (send model is-src? (send n get-vocabname))]
              [dest? (send model is-dest? (send n get-vocabname))]
              [x (send n get-x)]
              [y (send n get-y)]
              )])
    (begin 
      (hash-set! nodemap n newnode)
      newnode)))

; Consumes a netgraph and a model
; Returns a modelgraph with the model details applied to the nodes and edges
(define (apply-model ng model)
  (let ( [nodemap (make-hash)] )
    (_apply-model ng model
                  (lambda (n) (convert-node n model nodemap)) 
                  (lambda (e) (convert-edge e model nodemap)))))

; Consumes a netgraph (with positional nodes) and a model
; Returns a modelgraph with position data on the nodes
(define (apply-model/pos ng model)
  (let ( [nodemap (make-hash)] )  
    (_apply-model ng model
                  (lambda (n) (convert-node/pos n model nodemap)) 
                  (lambda (e) (convert-edge e model nodemap)))))

; Helper for apply-model functions
(define (_apply-model ng model nf ef)
  (send model set-edge-data (new modelgraph% 
                                 [nodes (map nf (send ng get-nodes))]
                                 [edges (map ef (send ng get-edges))]
                                 )))
