#lang scheme/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Cisco IOS Policy Compilation
;; Copyright (C) 2009-2010 Christopher Barratt & Brown University
;; All rights reserved.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require scheme/list)
(require scheme/class)
(require scheme/file)
(require scheme/pretty)
(require "ios.ss")
(require "ios-parse.ss")

(provide compile-configurations)

(define-syntax combine-rules
  (syntax-rules ()
    [(_ configurations accessor)
     (apply append (map (λ (configuration)
                          (send configuration accessor))
                        configurations))]))

;; string (listof string) boolean -> void
;; pass filename only if there is more than one configuration to do
(define (compile-configurations root-path filenames default-ACL-permit)
  (let* [(configurations (map (λ (filename)
                                (parse-IOS (open-input-file (make-path root-path filename)
                                                            #:mode
                                                            'text)
                                           default-ACL-permit))
                         filenames))
         (inbound-ACL (combine-rules configurations inbound-ACL-rules))
         (outbound-ACL (combine-rules configurations outbound-ACL-rules))
         (inside-NAT (combine-rules configurations inside-NAT-rules))
         (outside-NAT (combine-rules configurations outside-NAT-rules))
         (local-switch (combine-rules configurations local-switching-rules))
         (network-switch (combine-rules configurations network-switching-rules))
         (static-route (combine-rules configurations static-route-rules))
         (policy-route (combine-rules configurations policy-routing-rules))
         (default-policy-route (combine-rules configurations default-policy-routing-rules))
         (encryption (combine-rules configurations encryption-rules))]
    (begin
      (store (policy 'InboundACL inbound-ACL) (make-path root-path "InboundACL.p"))
      (store (policy 'OutboundACL outbound-ACL) (make-path root-path "OutboundACL.p"))
      (store (policy 'InsideNAT inside-NAT) (make-path root-path "InsideNAT.p"))
      (store (policy 'OutsideNAT outside-NAT) (make-path root-path "OutsideNAT.p"))
      (store (policy 'LocalSwitching local-switch) (make-path root-path "LocalSwitching.p"))
      (store (policy 'NetworkSwitching network-switch) (make-path root-path "NetworkSwitching.p"))
      (store (policy 'StaticRoute static-route) (make-path root-path "StaticRoute.p"))
      (store (policy 'PolicyRoute policy-route) (make-path root-path "PolicyRoute.p"))
      (store (policy 'DefaultPolicyRoute default-policy-route) (make-path root-path "DefaultPolicyRoute.p"))
      (store (policy 'Encryption encryption) (make-path root-path "Encryption.p"))
      (store (vocabulary (append inbound-ACL
                                 outbound-ACL
                                 inside-NAT
                                 outside-NAT
                                 local-switch
                                 network-switch
                                 static-route
                                 policy-route
                                 default-policy-route
                                 encryption))
                         (make-path root-path "IOS-vocab.v")))))

;; string string -> path
(define (make-path base file)
  (build-path (string->path base) (string->path file)))

;; any path -> void
(define (store contents path)
  (begin
    (let [(port (open-output-file path #:mode 'text #:exists 'replace))]
      (pretty-print contents port)
      (close-output-port port))))
