#lang racket

(provide topology load-topology)
(require "netgraph.rkt")

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

(define (load-topology filename)
  (eval (with-input-from-file filename read)))