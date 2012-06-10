#lang racket

(require rackunit
         ; Relative paths to avoid refreshing the Margrave collection every time something changes in dev.
         "../margrave.rkt"
         "../margrave-xml.rkt"
         "../margrave-ios.rkt"
         "../test-utils.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Parsing and loading IOS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(printf "Parsing and loading tests for IOS...~n")

(start-margrave-engine #:margrave-params '("-log")
                       #:margrave-path "..")

; Test the 3 functions provided.

(check-not-exn (lambda () 
                 (parse-and-load-ios "config1.txt" "IOS" "test" "1")))
               
(check-not-exn (lambda ()
                 (parse-and-load-ios-by-filename "IOS/config1.txt" #:prefix "test" #:suffix "2")))

; Used to include multiple config files in the same policy cluster.
(check-not-exn (lambda ()
                (parse-and-load-multi-ios '("tasconfig.txt" "bazconfig.txt") "IOS" #:prefix "test" #:suffix "3")))

; All loaded separately?
(check-equal? 30 (hash-count cached-policies))
(check-equal? 9 (hash-count cached-prior-queries))
; Theories for first two identical (same file)
; Theory of third is different.
(check m-theory-equal? 
       (m-policy-theory (hash-ref cached-policies "testInboundACL1"))
       (m-policy-theory (hash-ref cached-policies "testInboundACL2")))
(check (lambda (t1 t2) (not (m-theory-equal? t1 t2)))
       (m-policy-theory (hash-ref cached-policies "testInboundACL1"))
       (m-policy-theory (hash-ref cached-policies "testInboundACL3")))

; TODO: NOT really what we want. Over-writing the cached theory...
;(check-equal? 1 (hash-count cached-theories))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Running queries against IOS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; TODO
; Fill in once we've reduced the arity of decisions...


;(stop-margrave-engine)

