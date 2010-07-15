#lang racket/gui

(provide visualize pos-node% model-graph)

(require "netgraph.rkt" "controls.rkt")

; Node in the context of a specific model.
; Knows about the results of the policy decisions (accept/deny/etc)
(define model-node%
         (class ng-node%
           (init-field 
            [results empty])
           (super-new)
           
           (define/public (get-results) results)))

; Edge that knows if it is active in the model
(define model-edge%
         (class ng-edge%
           (init-field
            [active #f]
            [blocked #f])
           (super-new)
           
           (define/public (is-active?) active)
           (define/public (is-blocked?) blocked)))

(define modelgraph%
         (class netgraph%
           (inherit-field nodes edges)
           (super-new)
           ))

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

(define pos-model-node% (pos-mixin model-node%))

;;;;;; BROKEN, the edges have the old nodes as to and from. hrm :\
(define (model-graph ng model)
  (new modelgraph%
    [nodes (map (lambda (n) (new pos-model-node%
                                 [x (send n get-x)]
                                 [y (send n get-y)]
                                 [name (send n get-name)]
                                 [results empty]
                                 )) (send ng get-nodes))]
    [edges (map (lambda (e) (new model-edge%
                                 [from (send e get-from)]
                                 [to (send e get-to)]
                                 [active #f]
                                 [blocked #f])) (send ng get-edges))]))

; Adds a node to the pasteboard.
; It creates a snip and positions it based on the node's location
(define (place-node pb node)
  (let
      ((newnode (new entity-snip%
                     [updatef (lambda (x y) 
                                (send node set-x! x)
                                (send node set-y! y))]
                     [name (send node get-name)]
                     [icons empty]
                     [bitmap (make-object bitmap% "../images/ent_computer.png")])))
    (send pb insert newnode (send node get-x) (send node get-y))))

; For now, just places the nodes and edges.
(define (visualize pb ng)
  (map (lambda (node) (place-node pb node)) (send ng get-nodes))
  (send pb set-edges! (send ng get-edges)))