#lang racket

(provide netgraph% ng-node% ng-edge%)

; Netgraph Node
(define ng-node%
  (class object%
    (init-field
     [name "entity"]
     [policy null])
    
    (super-new)
    
    (define/public (get-name) name)
    ))

; Netgraph edge
(define ng-edge%
  (class object%
    (init-field from to)
    
    (super-new)    

    (define/public (get-from) from)
    (define/public (get-to) to)))

; The graph
(define netgraph% 
  (class object%
    (init-field 
     [nodes empty]
     [edges empty])
    (super-new)
    
    ; Is it better to mutate or to return a new object? I've seen it done both ways
    (define/public (add-node n)
      (new netgraph% [nodes (cons n nodes)] [edges edges]))
    
    (define/public (add-node! n)
      (set! nodes (cons n nodes)))
    
    (define/public (add-edge n1 n2)
      (new netgraph% [nodes nodes] [edges (cons (new ng-edge% [from n1] [to n2]) edges)]))
    
    (define/public (add-edge! n1 n2)
      (set! edges (cons (new ng-edge% [from n1] [to n2]) edges)))
    
    (define/public (get-nodes) nodes)
    (define/public (get-edges) edges) ))
