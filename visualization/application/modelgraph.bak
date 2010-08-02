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
(define (convert-edge e model)
  (new modelgraph-edge%
       [from (send (send e get-from) get-mnode)]
       [to (send (send e get-to) get-mnode)]
       [active (if (= 0 (random 2)) #t #f)]
       [blocked (if (= 0 (random 2)) #t #f)]))

(define (convert-node n model)
  (new modelgraph-node%
       [name (send n get-name)]
       [policy (send n get-policy)]
       [subgraph (if (null? (send n get-subgraph)) null (apply-model (send n get-subgraph) model))]       
       [results empty]))

(define (convert-node/pos n model)
  (let ([newnode
         (new pos-modelgraph-node%
              [name (send n get-name)]
              [policy (send n get-policy)]
              [subgraph (if (null? (send n get-subgraph)) null (apply-model/pos (send n get-subgraph) model))]       
              [results (filter (lambda (r) (= 0 (random 2))) (list result-accept result-deny result-modify))]
              [x (send n get-x)]
              [y (send n get-y)]
              )])
    (begin 
      (send n set-mnode! newnode)
      newnode)))

(define (apply-model ng model)
  (_apply-model ng model
                (lambda (n) (convert-node n model)) 
                (lambda (e) (convert-edge e model))))

(define (apply-model/pos ng model)
  (_apply-model ng model
                (lambda (n) (convert-node/pos n model)) 
                (lambda (e) (convert-edge e model))))

(define (_apply-model ng model nf ef)
  (new modelgraph% 
       [nodes (map nf (send ng get-nodes))]
       [edges (map ef (send ng get-edges))]
       ))