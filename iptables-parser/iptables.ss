#lang scheme

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; iptables Modeling
;; Copyright (C) 2009-2010 Christopher Barratt  All rights reserved.
;;
;;  This file is part of Margrave.
;;
;;  Margrave is free software: you can redistribute it and/or modify
;;  it under the terms of the GNU Lesser General Public License as published by
;;  the Free Software Foundation, either version 3 of the License, or
;;  (at your option) any later version.
;;
;;  Margrave is distributed in the hope that it will be useful,
;;  but WITHOUT ANY WARRANTY; without even the implied warranty of
;;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;  GNU Lesser General Public License for more details.
;;
;;  You should have received a copy of the GNU Lesser General Public License
;;  along with Margrave.  If not, see <http://www.gnu.org/licenses/>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require scheme/class)
(require scheme/list)
(require "margrave.ss")
(require "ip.ss")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Interfaces
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; symbol -> boolean
;;   Recognizes interfaces in symbolic form
(define (net-interface? interf)
  (and (symbol? interf)
       (regexp-match #px"if-(\\w+)" (symbol->string interf))))

;; symbol -> atom<%>
;;   Parses a network interface
(define (parse-net-interface interf)
  (if (eq? interf 'if-any)
      (new-root-atom 'if-any 'Interface)
      (new-atom interf 'Interface)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Chains
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; symbol -> boolean
;;   Recognizes packet-processing chains in symbolic form
(define (chain? chain)
  (and (symbol? chain)
       (or (eq? chain 'chain-any)
           (eq? chain 'INPUT)
           (eq? chain 'OUTPUT)
           (eq? chain 'FORWARD)
           (eq? chain 'PREROUTING)
           (eq? chain 'POSTROUTING))))

;; symbol -> atom<%>
;;   Parses a packet-processing chain
(define (parse-chain chain)
  (if (eq? chain 'chain-any)
      (new-root-atom 'chain-any 'Chain)
      (new-atom chain 'Chain)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TCP States and Control Flags
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; symbol -> boolean
;;   Recognizes a TCP state
(define (state? state)
  (and (symbol? state)
       (or (eq? state 'state-any)
           (eq? state 'INVALID)
           (eq? state 'NEW)
           (eq? state 'ESTABLISHED)
           (eq? state 'RELATED))))

;; symbol -> atom<%>
;;   Parses a TCP state
(define (parse-state state)
  (if (eq? state 'state-any)
      (new-root-atom 'state-any 'State)
      (new-atom state 'State)))

;; symbol -> boolean
;;   Recognizes a TCP control flag
(define (flag? flag)
  (and (symbol? flag)
       (or (eq? flag 'flag-any)
           (eq? flag 'NONE)
           (eq? flag 'ALL)
           (eq? flag 'SYN)
           (eq? flag '!SYN)
           (eq? flag 'ACK)
           (eq? flag 'RST)
           (eq? flag 'FIN)
           (eq? flag 'PSH)
           (eq? flag 'URG))))

;; symbol -> atom<%>
;;   Parses a TCP control flag
(define (parse-flag flag)
  (if (eq? flag 'flag-any)
      (new-root-atom 'flag-any 'Flag)
      (new-atom flag 'Flag)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Policy Parsing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define types `((chain Chain)
                (in-interface Interface)
                (src-addr Address)
                (src-port Port)
                (protocol Protocol)
                (message Message)
                (state State)
                (flag Flag)
                (dest-addr Address)
                (dest-port Port)
                (out-interface Interface)))

;; symbol -> atom<%>
;;   Parses an atom
(define (parse-atom atom)
  (cond [(icmp-message? atom) (parse-icmp-message atom)]
        [(address? atom) (parse-address atom)]
        [(protocol? atom) (parse-protocol atom)]
        [(net-interface? atom) (parse-net-interface atom)]
        [(chain? atom) (parse-chain atom)]
        [(state? atom) (parse-state atom)]
        [(flag? atom) (parse-flag atom)]
        [(port? atom) (parse-port atom)]
        [else (error "Unrecognized atom" atom)]))

;; S-expr -> rule<%>
;;   Parses a rule
(define (parse-rule s-expr)
  (new-rule (first s-expr)
            (first (third s-expr))
            (map (λ (name)
                   (new-required-variable name
                                          (second (assoc name types))))
                 (rest (third s-expr)))
            (map (λ (predicate)
                   (local [(define variable (second predicate))]
                     (new-predicate (parse-atom (first predicate))
                                    (new-required-variable variable
                                                           (second (assoc variable types))))))
                 (drop s-expr 4))))

;; (listof S-expr) -> policy<%>
;;   Parses a policy
(define (parse-policy s-expr)
  (new-policy (second s-expr)
              (fourth s-expr)
              (map (λ (rule)
                     (parse-rule rule))
                   (rest (sixth s-expr)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Vocabulary Generation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define addresses (new-atom-tree (new-ipv4-cidr-network 0 0)))
(define ports (new-atom-tree (new-port-range 0 65535)))
(define protocols (new-atom-tree (new-protocol 0)))
(define messages (new-atom-tree (parse-icmp-message 'icmp-any)))
(define states (new-atom-tree (new-root-atom 'state-any 'State)))
(define flags (new-atom-tree (new-root-atom 'flag-any 'Flags)))
(define chains (new-atom-tree (new-root-atom 'chain-any 'Chain)))
(define interfaces (new-atom-tree (new-root-atom 'if-any 'Interface)))

;; string string -> void
;;   Compiles a vocabulary for a policy file
(define (compile-vocabulary policy-file vocabulary-file)
  (local [(define input (open-input-file (string->path policy-file) #:mode 'text))
          (define output (open-output-file (string->path vocabulary-file) #:mode 'text #:exists 'replace))]
    (begin
      (pretty-print (send (build-vocabulary (parse-policy (read input))) get-text) output)
      (close-input-port input)
      (close-output-port output))))

;; policy<%> -> vocabulary<%>
;;   Builds a vocabulary for a policy
(define (build-vocabulary policy)
  (printf "Building vocab~n")
  (new-vocabulary 'iptables-firewall
                  (build-types policy)
                  (list 'ACCEPT 'LOG 'DROP 'REJECT 'SNAT)
                  '()
                  (map (λ (variable)
                         (new-required-variable (first variable) (second variable)))
                       types)
                  (send policy get-constraints)))

;; policy<%> -> hash
;;   Builds the type definitions for a policy
(define (build-types policy)
  (local [(define rules (send policy get-rules))]
    (make-immutable-hash `((Chain ,@(build-type chains rules))
                           (Interface ,@(build-type interfaces rules))
                           (Address ,@(build-type addresses rules))
                           (Port ,@(build-type ports rules))
                           (Protocol ,@(build-type protocols rules))
                           (Message ,@(build-type messages rules))
                           (State ,@(build-type states rules))
                           (Flag ,@(build-type flags rules))))))

;; atom-tree<%> (listof rule<%>) -> atom-tree<%>
;;   Builds a type definition from a list of rules
(define (build-type type-tree rules)
  (if (empty? rules)
      type-tree
      (build-type (foldl (λ (atom result-tree)
                           (if (eq? (send atom get-type-name)
                                    (send result-tree get-type-name))
                               (send result-tree insert atom)
                               result-tree))
                         type-tree
                         (send (first rules) get-atoms))
                  (rest rules))))
