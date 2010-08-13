#lang racket

(provide get-child-elements get-child-element get-pc-data)

(require "../../margrave.rkt" xml "../../margrave-xml.rkt")

; These are functions from margrave-xml that were not provided by it. I didn't want to mess with Margrave so I just pulled them out to here.

; element -> list
(define (get-child-elements element name-symb-or-str)
  (if (empty? element)
      empty
      (let* ((name (string-downcase (if (symbol? name-symb-or-str)
                                        (symbol->string name-symb-or-str)
                                        name-symb-or-str)))
             (result (filter (lambda(element) (equal? name (string-downcase (symbol->string (element-name element)))))
                             (filter (lambda (maybe-elem) (element? maybe-elem))
                                     (element-content element)))))
        result)))

;Pass name a symbol or a string, case doesn't matter
;Returns either an element, if found, or an empty list
;Note that if you pass this element an empty list instead of an element, instead of halting it will return empty
(define (get-child-element element name-symb-or-str)
  (let ([result (get-child-elements element name-symb-or-str)])
    (if (empty? result)
        result
        (first result))))

(define (get-pc-data elem)
  (pcdata-string (first (element-content elem))))