#lang setup/infotab

(define name "Margrave")
(define scribblings '(["scribblings/margrave.scrbl" () (tool) ]))

;(multi-page)  ; multi-page would cause each top-level sect to get its own html page
; see raco setup docs

(define compile-omit-paths '("examples" "examples/scripts" "tests"))

(define homepage "www.margrave-tool.org")