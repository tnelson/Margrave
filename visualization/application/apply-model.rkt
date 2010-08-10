#lang racket/gui

(provide mg-model% apply-model apply-model/pos)

(require "modelgraph.rkt" "visxml.rkt" xml "../../margrave-xml.rkt")

(define (check-src-dest xml name label)
  (and (element? xml)
  (and (symbol=? (element-name xml) 'RELATION)
   (and (string=? (get-attribute-value xml 'name) name)
        (string=? (get-pc-data (get-child-element (get-child-element xml 'tuple) 'atom)) label)))))

(define mg-model%
  (class object%
    (init-field
     [keyword-map (make-hash)]
     [xml #f]
     )
    
    ; Returns the list of policy decisions made by that entity or empty
    ; if the entity was not involved in the model
    (define/public (get-entity-data entname)
      #f)
    
    ; Returns (list bool bool) where the first value is if the edge is part of the model
    ; and the second is if the edge is blocked by an entity denying the traffic
    (define/public (get-edge-data entn1 entn2) #f)
    
    ; Returns true if the supplied entity is the src host
    (define/public (is-src? entname)
      (ormap (lambda (x) (check-src-dest x entname (hash-ref keyword-map 'ipsrc))) (element-content xml)))
    
    ; Returns true if the supplied entity is the desination
    (define/public (is-dest? entname)
      (ormap (lambda (x) (check-src-dest x entname (hash-ref keyword-map 'ipdest))) (element-content xml)))
    
    (super-new)
    ))

; Helper functions for apply-model

; Consumes a netgraph-edge and a model
; Returns a modelgraph-edge
(define (convert-edge e model nodemap)
  (new modelgraph-edge%
       [from (hash-ref nodemap (send e get-from))]
       [to (hash-ref nodemap (send e get-to))]
       ;[active (if (= 0 (random 2)) #t #f)]
       ;[blocked (if (= 0 (random 2)) #t #f)]
       [active #f]
       [blocked #f]
       ))

; Consumes a netgraph-node and a model
; Returns a modelgraph-node
(define (convert-node n model nodemap)
  (new modelgraph-node%
       [name (send n get-name)]
       [type (send n get-type)]       
       [policy (send n get-policy)]
       [subgraph (if (null? (send n get-subgraph)) null (apply-model (send n get-subgraph) model))]       
       [results empty]))

; Consumes a pos-netgraph-node and a model
; Returns a pos-modelgraph-node
(define (convert-node/pos n model nodemap)
  (let ([newnode
         (new pos-modelgraph-node%
              [name (send n get-name)]
              [type (send n get-type)]
              [policy (send n get-policy)]
              [subgraph (if (null? (send n get-subgraph)) null (apply-model/pos (send n get-subgraph) model))]       
              ;[results (filter (lambda (r) (= 0 (random 2))) (list result-accept result-deny result-modify))]
              
              [results empty]
              [x (send n get-x)]
              [y (send n get-y)]
              )])
    (begin 
      (hash-set! nodemap n newnode)
      (print (send model is-src? (send n get-name)))
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
  (new modelgraph% 
       [nodes (map nf (send ng get-nodes))]
       [edges (map ef (send ng get-edges))]
       ))
