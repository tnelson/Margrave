#lang racket

(provide netgraph% pos-node% ng-node% ng-edge%)

; Netgraph Node
(define ng-node%
  (class object%
    (init-field
     [name "entity"]
     [policy null]
     [result empty])
    
    (super-new)
    
    (define/public (get-name) name)
    (define/public (get-result) result)
    ))

; Netgraph edge
(define ng-edge%
  (class object%
    (init-field
     from
     to
     [active #t])
    
    (super-new)    

    (define/public (get-from) from)
    (define/public (get-to) to)
    (define/public (active?) active)))

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
    
    (define/public (add-edge n1 n2 active)
      (new netgraph% [nodes nodes] [edges (cons (new ng-edge% [from n1] [to n2] [active active]) edges)]))
    
    (define/public (add-edge! n1 n2 active)
      (set! edges (cons (new ng-edge% [from n1] [to n2] [active active]) edges)))
    
    (define/public (get-nodes) nodes)
    (define/public (get-edges) edges) ))


; Mixin for giving nodes 'n things a position component
(define (pos-mixin %)
  (class % (super-new)
    (init-field
     [x 0]
     [y 0])
    (define/public (get-x) x)
    (define/public (get-y) y)
    (define/public (set-x! nx) (set! x nx))
    (define/public (set-y! ny) (set! y ny))
    ))

; Create a positional node class
(define pos-node% (pos-mixin ng-node%))