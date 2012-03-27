#lang racket

(provide topology load-topology)
(require "netgraph.rkt")

; I guess I need a namespace to give eval
(define-namespace-anchor a)

; Macro to create netgraphs based on the topology file format
(define-syntax topology
  (syntax-rules ()
    [(topology
      ((node N1 [key val] ...) ...)
      ((edge A1 A2) ...))
     (let ([ng (new netgraph% )]
           [N1 (new pos-netgraph-node% [key val] ...)] ...)
       (begin
         (begin
           (send ng add-node! N1)
           ...
           )
         (begin
           (send ng add-edge! A1 A2)
           ...
           )
         ng
         ))]))

; Loads a netgraph by using eval and (hopefully!) the macro. Is this a good idea?!
(define (load-topology filename)
  (eval (with-input-from-file filename read) (namespace-anchor->namespace a))
  )