#lang racket

(provide modelgraph-node% modelgraph-edge% modelgraph% pos-modelgraph-node%
         apply-model apply-model/pos)

(require "netgraph.rkt")

(define modelgraph-node%
  (class netgraph-node%
    (init-field
     [results empty])
    (super-new)))

(define modelgraph-edge%
  (class netgraph-edge%
    (init-field
     [active #f]
     [blocked #f])
    
    (define/public (is-active?) active)

    (super-new)))

(define modelgraph%
  (class netgraph%
    (super-new)))

(define pos-modelgraph-node% (pos-mixin modelgraph-node%))

(define (convert-edge e model)
  (new modelgraph-edge%
       [from (send (send e get-from) get-mnode)]
       [to (send (send e get-to) get-mnode)]
       [active #f]
       [blocked #f]))

(define (convert-node n model)
  (new modelgraph-node%
       [name (send n get-name)]
       [policy (send n get-policy)]
       [results empty]))

(define (convert-node/pos n model)
  (let ([newnode
         (new pos-modelgraph-node%
              [name (send n get-name)]
              [policy (send n get-policy)]
              [results empty]
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