#lang racket

(provide netgraph% netgraph-node% netgraph-edge% pos-mixin pos-netgraph-node%)

; Netgraph Node
(define netgraph-node%
  (class object%
    (init-field
     [name "entity"]
     [policy null]
     [type 'host]
     [vocabname ""]
     [subgraph null])
    
    (super-new)
    
    (define/public (get-name) name)
    (define/public (get-type) type)
    (define/public (get-policy) policy)
    (define/public (get-vocabname) vocabname)
    (define/public (set-subgraph! sg) (set! subgraph sg))
    (define/public (get-subgraph) subgraph)
    ))

; Netgraph edge
(define netgraph-edge%
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
    
    (define/public (add-node n)
      (new netgraph% [nodes (cons n nodes)] [edges edges]))
    
    (define/public (add-node! n)
      (set! nodes (cons n nodes)))
    
    (define/public (add-edge n1 n2)
      (new netgraph% [nodes nodes] [edges (cons (new netgraph-edge% [from n1] [to n2]) edges)]))
    
    (define/public (add-edge! n1 n2)
      (set! edges (cons (new netgraph-edge% [from n1] [to n2]) edges)))
    
    (define/public (find-edge n1 n2)
      (let ([edge (filter (lambda (e) (and (eq? n1 (send e get-from)) (eq? n2 (send e get-to)))) edges)])
        (if (empty? edge) #f (first edge))))
    
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

(define pos-netgraph-node% (pos-mixin netgraph-node%))
