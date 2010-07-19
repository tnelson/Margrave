#lang racket

(provide modelgraph-node% modelgraph-edge% modelgraph% pos-modelgraph-node%
         apply-model apply-model/pos)

(require "netgraph.rkt")

(define modelgraph-node%
  (class netgraph-node%
    (super-new)))

(define modelgraph-edge%
  (class netgraph-edge%
    (super-new)))

(define modelgraph%
  (class netgraph%
    (super-new)))

(define pos-modelgraph-node% (pos-mixin modelgraph-node%))

(define (apply-model ng model)
  (_apply-model ng model (lambda (n) n) (lambda (e) e)))

(define (apply-model/pos ng model)
  (_apply-model ng model (lambda (n) n) (lambda (e) e)))

(define (_apply-model ng model nf ef)
  (new modelgraph% 
  [nodes (map nf (send ng get-nodes))]
  [edges (map ef (send ng get-edges))]
  ))