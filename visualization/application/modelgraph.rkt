#lang racket/gui

(provide modelgraph-node% modelgraph-edge% modelgraph% pos-modelgraph-node%
         apply-model apply-model/pos)

(require "netgraph.rkt")

; Result types
(define model-result%
  (class object%
    (init-field
     [bitmap null]
     [name "UNNAMED"])
    
    (super-new)
    
    (define/public (get-bitmap) bitmap)
    (define/public (get-name) name)))

(define result-accept
  (make-object model-result%
    (make-object bitmap% "../images/icon_accept.png")
    "Accept"))

(define result-deny
  (make-object model-result%
    (make-object bitmap% "../images/icon_deny.png")
    "Deny"))

(define result-modify
  (make-object model-result%
    (make-object bitmap% "../images/icon_modify.png")
    "Modify"))



; Node in a modelgraph
; Has a list of policy results (blocked, accepted, etc)
(define modelgraph-node%
  (class netgraph-node%
    (init-field
     [results empty])
    (super-new)
    
    (define/public (get-results) results)
    
    ))

; Edge
; Knows if it is active (part of the model) and if it's blocked.
(define modelgraph-edge%
  (class netgraph-edge%
    (init-field
     [active #f]
     [blocked #f])
    
    (define/public (is-active?) active)
    
    (super-new)))

; The graph
; TODO: make sure the nodes and edges that are a part of it are modelgraph type not
; netgraph type.
(define modelgraph%
  (class netgraph%
    (super-new)))

; Positional component mixed in
(define pos-modelgraph-node% (pos-mixin modelgraph-node%))

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