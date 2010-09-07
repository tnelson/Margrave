#lang scheme

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; IP Modeling
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

(require rnrs/arithmetic/bitwise-6)
(require "margrave.ss")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Parsing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; any regex -> boolean
;;   Returns whether the given value is a symbol that matches
;;   a regular expression
(define (matching-symbol? value regex)
  (and (symbol? value)
       (regexp-match regex (symbol->string value))))

;; any regex -> (listof any)
;;   Parses the given symbolic value according to a regular expression
(define (parse-symbol-parts value regex)
  (rest (regexp-match regex (symbol->string value))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Addresses
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; An address<%> is an IP address.
;;   get-address : -> number
;;     Returns this IP address
;;   get-network : -> number
;;     Returns the network portion of this IP address
;;   get-netmask : -> number
;;     Returns the network mask for this IP address
(define address<%> (interface (atom<%>) get-address get-network get-netmask))

;; number number -> address<%>
;;   Creates an address<%> from an IPv4 host address
(provide/contract
 [new-ipv4-host (-> number? (is-a?/c address<%>))])

(define (new-ipv4-host address)
  (new ipv4-address%
       [address address]
       [prefix 32]
       [cidr-form? #f]))

;; number number -> address<%>
;;   Creates an address<%> from an IPv4 network address and mask
(provide/contract
 [new-ipv4-network (-> number? number? (is-a?/c address<%>))])

(define (new-ipv4-network address netmask)
  (new ipv4-address%
       [address address]
       [prefix (bitwise-bit-count netmask)]
       [cidr-form? #t]))

;; number number -> address<%>
;;   Creates an address<%> from an IPv4 network address and a CIDR prefix
(provide/contract
 [new-ipv4-cidr-network (-> number? number? (is-a?/c address<%>))])

(define (new-ipv4-cidr-network address prefix)
  (new ipv4-address% [address address] [prefix prefix] [cidr-form? #t]))

;; symbol -> boolean
;;   Recognizes addresses in symbolic form
(provide/contract
 [address? (-> symbol? boolean?)])

(define dotted-cidr-rx #px"(\\d+)\\.(\\d+)\\.(\\d+)\\.(\\d+)/(\\d+)")
(define dashed-network-rx #px"ip-(\\d+)-(\\d+)-(\\d+)-(\\d+)/ip-(\\d+)-(\\d+)-(\\d+)-(\\d+)")
(define dashed-cidr-rx #px"ip-(\\d+)-(\\d+)-(\\d+)-(\\d+)/(\\d+)")
(define dotted-address-rx #px"(\\d+)\\.(\\d+)\\.(\\d+)\\.(\\d+)")
(define dashed-address-rx #px"ip-(\\d+)-(\\d+)-(\\d+)-(\\d+)")

(define (address? address)
  (with-handlers [([λ (exn)
                     (exn:fail? exn)]
                   [λ (exn)
                     #f])]
    (begin
      (parse-address address)
      #t)))

;; symbol -> address<%>
;;   Creates an address<%> from either dotted-octet notation or the
;;   atomic name thereof
(provide/contract
 [parse-address (-> symbol? (is-a?/c address<%>))])

(define (parse-address address)
  (cond
    ;; An IPv4 network address in dotted-octet notation
    [(matching-symbol? address dotted-cidr-rx)
     (local [(define octets (parse-symbol-parts address dotted-cidr-rx))]
       (new-ipv4-cidr-network (octets->number (take octets 4))
                              (octets->number (drop octets 4))))]
    
    ;; An IPv4 network address in dashed-octet notation (i.e., a Margrave atom]
    [(matching-symbol? address dashed-network-rx)
     (local [(define octets (parse-symbol-parts address dashed-network-rx))]
       (new-ipv4-network (octets->number (take octets 4))
                         (octets->number (drop octets 4))))]
    
    ;; An IPv4 network address in dashed-octet notation (i.e., a Margrave atom]
    [(matching-symbol? address dashed-cidr-rx)
     (local [(define octets (parse-symbol-parts address dashed-cidr-rx))]
       (new-ipv4-cidr-network (octets->number (take octets 4))
                              (octets->number (drop octets 4))))]
    
    ;; An IPv4 address in dotted-octet notation
    [(matching-symbol? address dotted-address-rx)
     (local [(define octets (parse-symbol-parts address dotted-address-rx))]
       (new-ipv4-host (octets->number octets)))]
    
    ;; An IPv4 address in dashed-octet notation (i.e., a Margrave atom)
    [(matching-symbol? address dashed-address-rx)
     (local [(define octets (parse-symbol-parts address dashed-address-rx))]
       (new-ipv4-host (octets->number octets)))]
    
    ;; An unrecognized address form
    [else
     (error "Unsupported address form" address)]))

;; number number boolean
;;   An IPv4 address
(define ipv4-address%
  (class* object% (address<%>)
    (init-field address prefix cidr-form?)
    (super-new)
    
    (define/public (equal-to? rhs equal-proc)
      (and (is-a? rhs atom<%>)
           (eq? (get-type-name) (send rhs get-type-name))
           (eq? (get-name) (send rhs get-name))))
    
    (define/public (equal-hash-code-of hash-proc)
      address)
    
    (define/public (equal-secondary-hash-code-of hash2-proc)
      prefix)
    
    (define/public (get-constraints)
      (if (single?)
          `((atmostone ,(get-name)))
          `((disjoint-all ,(get-name))))) ;; TN added disjoint all; was '()
    
    (define/public (get-name)
      (local [(define address-text (string-append "ip-" (octets->string 3)))]
        (if cidr-form?
            (string->symbol address-text)
            (string->symbol (string-append address-text "/" (number->string prefix))))))
    
    (define/public (single?)
      (zero? (get-suffix)))
    
    (define/public (covers? rhs)
      (and (eq? (get-type-name) (send rhs get-type-name))
           (not (equal-to? rhs equal?))
           (eqv? (get-network) (bitwise-and (send rhs get-address)
                                            (get-netmask)))))
    
    (define/public (get-type-name)
      'Address)
    
    (define/public (get-address)
      address)
    
    (define/public (get-prefix)
      prefix)
    
    (define/public (get-network)
      (bitwise-and address (get-netmask)))
    
    (define/public (get-netmask)
      (shifted-ones prefix (get-suffix)))

    (define/private (octets->string last-index)
      (local [(define shift (* 8 last-index))]
        (local [(define text
                  (number->string
                   (bitwise-and (arithmetic-shift address (- shift)) #xFF)))]
          (if (zero? last-index)
              text
              (string-append text "-" (octets->string (sub1 last-index)))))))
    
    (define/private (get-suffix)
      (- 32 prefix))
    ))

;; (listof string) -> number
;;   Converts a list of octets into a number
(define (octets->number octets)
  (if (empty? octets)
      0
      (bitwise-ior (arithmetic-shift (string->number (first octets))
                                     (* (sub1 (length octets)) 8))
                   (octets->number (rest octets)))))

;; number number -> number
;;   Returns a shifted bit string of ones
(define (shifted-ones length shift)
  (arithmetic-shift (sub1 (arithmetic-shift 1 length)) shift))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Protocols
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A protocol<%> is an IP protocol.
;;   get-protocol : -> number
;;     Returns the number for this protocol
(define protocol<%> (interface (atom<%>) get-protocol))

;; symbol -> protocol<%>
;;   Creates a protocol<%> from a number
(provide/contract
 [new-protocol (-> number? (is-a?/c protocol<%>))])

(define (new-protocol protocol)
  (new protocol% [protocol protocol]))

;; symbol -> boolean
;;   Recognizes protocols in symbolic form
(provide/contract
 [protocol? (-> symbol? boolean?)])

(define (protocol? protocol)
  (with-handlers [([λ (exn)
                     (exn:fail? exn)]
                   [λ (exn)
                     #f])]
    (begin
      (parse-protocol protocol)
      #t)))

;; symbol -> protocol<%>
;;   Creates a protocol<%> from the atomic name thereof
(provide/contract
 [parse-protocol (-> symbol? (is-a?/c protocol<%>))])

(define (parse-protocol protocol)
  (new protocol% [protocol (protocol/symbol->number protocol)]))

;; number
;;   An IP protocol
(define protocol%
  (class* object% (protocol<%>)
    (init-field protocol)
    (super-new)
    
    (define/public (equal-to? rhs equal-proc)
      (and (is-a? rhs atom<%>)
           (eq? (get-type-name) (send rhs get-type-name))
           (eq? (get-name) (send rhs get-name))))
    
    (define/public (equal-hash-code-of hash-proc)
      protocol)
    
    (define/public (equal-secondary-hash-code-of hash2-proc)
      protocol)
    
    (define/public (get-constraints)
      `((atmostone ,(get-name))))
    
    (define/public (get-name)
      (protocol/number->symbol protocol))
    
    (define/public (covers? rhs)
      (zero? protocol))
    
    (define/public (single?)
      #t)
    
    (define/public (get-type-name)
      'Protocol)
    
    (define/public (get-protocol)
      protocol)
    ))

;; number -> symbol
;;   Converts a protocol number to a name
(define (protocol/number->symbol number)
  (cond [(eqv? number 0) 'ip]
        [(eqv? number 1) 'icmp]
        [(eqv? number 6) 'tcp]
        [(eqv? number 17) 'udp]
        [(eqv? number 47) 'gre]
        [(eqv? number 50) 'esp]
        [(eqv? number 51) 'ah]
        [else (error "Unsupported protocol" number)]))

;; symbol -> number
;;   Converts a protocol name to a number
(define (protocol/symbol->number symbol)
  (cond [(eq? symbol 'ip) 0]
        [(eq? symbol 'icmp) 1]
        [(eq? symbol 'tcp) 6]
        [(eq? symbol 'udp) 17]
        [(eq? symbol 'gre) 47]
        [(eq? symbol 'esp) 50]
        [(eq? symbol 'ah) 51]
        [else (error "Unsupported protocol" symbol)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ICMP
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A icmp-message<%> is an ICMP message.
;;   get-message : -> number
;;     Returns the number for this message
(define icmp-message<%> (interface (atom<%>) get-message))

;; number -> icmp-message<%>
;;   Creates an icmp-message<%> from a number
(provide/contract
 [new-icmp-message (-> number? (is-a?/c icmp-message<%>))])

(define (new-icmp-message message)
  (new icmp-message% [message message] [symbolic-form? #t]))

;; (or symbol number) -> boolean
;;   Recognizes ICMP messages
(provide/contract
 [icmp-message? (-> (or/c symbol? number?) boolean?)])

(define (icmp-message? message)
  (with-handlers [([λ (exn)
                     (exn:fail? exn)]
                   [λ (exn)
                     #f])]
    (begin
      (parse-icmp-message message)
      #t)))

;; (or symbol number) -> icmp-message<%>
;;   Creates an icmp-message<%> from the atomic name thereof
(provide/contract
 [parse-icmp-message (-> (or/c symbol? number?) (is-a?/c icmp-message<%>))])

(define icmp-any-rx #px"icmp-any")

(define (parse-icmp-message message)
  (cond
    ;; An unspecified message
    [(matching-symbol? message icmp-any-rx)
     (new any-icmp-message%)]
    
    ;; A message in symbolic form
    [(symbol? message)
     (new icmp-message%
          [message (message/symbol->number message)]
          [symbolic-form? #t])]
    
    ;; A message in numeric form
    [(number? message)
     (new icmp-message% [message message] [symbolic-form? #f])]
    
    ;; An unsupported message form
    [else (error "Unsupported ICMP message form")]))

;; number boolean
;;   An ICMP message
(define icmp-message%
  (class* object% (icmp-message<%>)
    (init-field message symbolic-form?)
    (super-new)
    
    (define/public (equal-to? rhs equal-proc)
      (and (is-a? rhs atom<%>)
           (eq? (get-type-name) (send rhs get-type-name))
           (eq? (get-name) (send rhs get-name))))
    
    (define/public (equal-hash-code-of hash-proc)
      message)
    
    (define/public (equal-secondary-hash-code-of hash2-proc)
      message)
    
    (define/public (get-constraints)
      `((atmostone ,(get-name))))
    
    (define/public (get-name)
      (if symbolic-form?
          (message/number->symbol message)
          message))
    
    (define/public (covers? rhs)
      #f)
    
    (define/public (single?)
      #t)
    
    (define/public (get-type-name)
      'Message)
    
    (define/public (get-message)
      message)
    ))

;; An unspecified ICMP message
(define any-icmp-message%
  (class* object% (icmp-message<%>)
    (super-new)
    
    (define/public (equal-to? rhs equal-proc)
      (and (is-a? rhs atom<%>)
           (eq? (get-type-name) (send rhs get-type-name))
           (eq? (get-name) (send rhs get-name))))
    
    (define/public (equal-hash-code-of hash-proc)
      0)
    
    (define/public (equal-secondary-hash-code-of hash2-proc)
      0)
    
    (define/public (get-constraints)
      `())
    
    (define/public (get-name)
      'icmp-any)
    
    (define/public (covers? rhs)
      (not (equal-to? rhs equal?)))
    
    (define/public (single?)
      #f)
    
    (define/public (get-type-name)
      'Message)
    
    (define/public (get-message)
      0)
    ))

;; number -> symbol
;;   Converts an ICMP message number to a name
(define (message/number->symbol number)
  (cond [(eqv? number 0) 'echo-reply]
        [(eqv? number 3) 'destination-unreachable]
        [(eqv? number 4) 'source-quench]
        [(eqv? number 5) 'redirect]
        [(eqv? number 6) 'alternate-address]
        [(eqv? number 8) 'echo-request]
        [(eqv? number 9) 'router-advertisement]
        [(eqv? number 10) 'router-solicitation]
        [(eqv? number 11) 'time-exceeded]
        [(eqv? number 12) 'parameter-problem]
        [(eqv? number 13) 'timestamp-request]
        [(eqv? number 14) 'timestamp-reply]
        [(eqv? number 15) 'information-request]
        [(eqv? number 16) 'information-reply]
        [(eqv? number 17) 'mask-request]
        [(eqv? number 18) 'mask-reply]
        [(eqv? number 30) 'traceroute]
        [(eqv? number 31) 'conversion-error]
        [(eqv? number 32) 'mobile-redirect]
        [else (error "Unsupported ICMP message" number)]))

;; symbol -> number
;;   Converts an ICMP message name to a number
(define (message/symbol->number symbol)
  (cond [(eq? symbol 'echo-reply) 0]
        [(eq? symbol 'destination-unreachable) 3]
        [(eq? symbol 'source-quench) 4]
        [(eq? symbol 'redirect) 5]
        [(eq? symbol 'alternate-address) 6]
        [(eq? symbol 'echo-request) 8]
        [(eq? symbol 'router-advertisement) 9]
        [(eq? symbol 'router-solicitation) 10]
        [(eq? symbol 'time-exceeded) 11]
        [(eq? symbol 'parameter-problem) 12]
        [(eq? symbol 'timestamp-request) 13]
        [(eq? symbol 'timestamp-reply) 14]
        [(eq? symbol 'information-request) 15]
        [(eq? symbol 'information-reply) 16]
        [(eq? symbol 'mask-request) 17]
        [(eq? symbol 'mask-reply) 18]
        [(eq? symbol 'traceroute) 30]
        [(eq? symbol 'conversion-error) 31]
        [(eq? symbol 'mobile-redirect) 32]
        [else (error "Unsupported ICMP message" symbol)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TCP/UDP
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A port<%> is a TCP/UDP port.
;;   get-port : -> number
;;     Returns the number for this port
(define port<%> (interface (atom<%>) get-port))

;; symbol -> port<%>
;;   Creates a port<%> from a number
(provide/contract
 [new-port (-> number? (is-a?/c port<%>))])

(define (new-port port)
  (new port% [port port] [symbolic-form? #t]))

;; number number -> port<%>
;;   Creates a port<%> from a range
(provide/contract
 [new-port-range (-> number? number? (is-a?/c port<%>))])

(define (new-port-range start end)
  (new port-range% [start start] [end end]))

;; symbol -> boolean
;;   Recognizes ports in symbolic form
(provide/contract
 [port? (-> symbol? boolean?)])

(define (port? port)
  (with-handlers [([λ (exn)
                     (exn:fail? exn)]
                   [λ (exn)
                     #f])]
    (begin
      (parse-port port)
      #t)))

;; symbol -> port<%>
;;   Creates a port<%> from the atomic name thereof
(provide/contract
 [parse-port (-> symbol? (is-a?/c port<%>))])

(define port-rx #px"port-(\\d+)")
(define port-range-rx #px"port-(\\d+):(\\d+)")
(define port-name-rx #px"port-([[:alnum:]-]+)")

(define (parse-port port)
  (cond
    ;; A port range
    [(matching-symbol? port port-range-rx)
     (local [(define ports (parse-symbol-parts port port-range-rx))]
       (new port-range%
            [start (string->number (first ports))]
            [end (string->number (second ports))]))]
    
    ;; A numeric port
    [(matching-symbol? port port-rx)
     (local [(define ports (parse-symbol-parts port port-rx))]
       (new port% [port (string->number (first ports))] [symbolic-form? #f]))]
    
    ;; A port name in atomic form
    [(matching-symbol? port port-name-rx)
     (local [(define ports (parse-symbol-parts port port-name-rx))]
       (new port%
            [port (port/symbol->number (string->symbol (first ports)))]
            [symbolic-form? #t]))]
    
    ;; A port name
    [else (new port% [port port] [symbolic-form? #t])]))

;; number boolean
;;   A TCP/UDP port
(define port%
  (class* object% (port<%>)
    (init-field port symbolic-form?)
    (super-new)
    
    (define/public (equal-to? rhs equal-proc)
      (and (is-a? rhs atom<%>)
           (eq? (get-type-name) (send rhs get-type-name))
           (eq? (get-name) (send rhs get-name))))
    
    (define/public (equal-hash-code-of hash-proc)
      port)
    
    (define/public (equal-secondary-hash-code-of hash2-proc)
      port)
    
    (define/public (get-constraints)
      `((atmostone ,(get-name))))
    
    (define/public (get-name)
      (string->symbol (string-append "port-" (if symbolic-form?
                                                 (symbol->string (port/number->symbol port))
                                                 (number->string port)))))
    
    (define/public (covers? rhs)
      #f)
    
    (define/public (single?)
      #t)
    
    (define/public (get-type-name)
      'Port)
    
    (define/public (get-port)
      port)
    ))

;; number number
;;   A port range
;;
;; N.B.: This should be re-designed; the covers? relation
;; is not well-defined
(define port-range%
  (class* object% (port<%>)
    (init-field start end)
    (super-new)
    
    (define/public (equal-to? rhs equal-proc)
      (and (is-a? rhs atom<%>)
           (eq? (get-type-name) (send rhs get-type-name))
           (eq? (get-name) (send rhs get-name))))
    
    (define/public (equal-hash-code-of hash-proc)
      start)
    
    (define/public (equal-secondary-hash-code-of hash2-proc)
      end)
    
    (define/public (get-constraints)
      `((disjoint-all ,(get-name))))
    
    (define/public (get-name)
      (string->symbol (string-append "port-"
                                     (number->string start)
                                     ":"
                                     (number->string end))))
    
    (define/public (covers? rhs)
      (if (send rhs single?)
          (and (>= (send rhs get-port) start)
               (>= end (send rhs get-port)))
          (and (>= (get-field start rhs) start)
               (>= end (get-field end rhs)))))
    
    (define/public (single?)
      #f)
    
    (define/public (get-type-name)
      'Port)
    
    (define/public (get-port)
      (error "Unsupported operation: get-port"))
    ))

;; number -> symbol
;;   Converts a TCP/UDP port number to a name
(define (port/number->symbol number)
  (cond [(eqv? number 21) 'ftp]
        [(eqv? number 22) 'ssh]
        [(eqv? number 23) 'telnet]
        [(eqv? number 25) 'smtp]
        [(eqv? number 53) 'domain]
        [(eqv? number 67) 'bootps]
        [(eqv? number 69) 'tftp]
        [(eqv? number 80) 'http]
        [(eqv? number 80) 'www]
        [(eqv? number 111) 'sunrpc]
        [(eqv? number 113) 'auth]
        [(eqv? number 123) 'ntp]
        [(eqv? number 161) 'snmp]
        [(eqv? number 162) 'snmp-trap]
        [(eqv? number 443) 'https]
        [(eqv? number 465) 'smtps]
        [(eqv? number 500) 'isakmp]
        [(eqv? number 602) 'syslog]
        [(eqv? number 636) 'ldaps]
        [(eqv? number 993) 'imaps]
        [(eqv? number 995) 'pop3s]
        [(eqv? number 1194) 'openvpn]
        [(eqv? number 3306) 'mysql]
        [(eqv? number 5432) 'postgresql]
        [else (error "Unsupported TCP/UDP port" number)]))

;; symbol -> number
;;   Converts a TCP/UDP port name to a number
(define (port/symbol->number symbol)
  (cond [(eq? symbol 'ftp) 21]
        [(eq? symbol 'ssh) 22]
        [(eq? symbol 'telnet) 23]
        [(eq? symbol 'smtp) 25]
        [(eq? symbol 'domain) 53]
        [(eq? symbol 'bootps) 67]
        [(eq? symbol 'tftp) 69]
        [(eq? symbol 'http) 80]
        [(eq? symbol 'www) 80]
        [(eq? symbol 'sunrpc) 111]
        [(eq? symbol 'auth) 113]
        [(eq? symbol 'ntp) 123]
        [(eq? symbol 'snmp) 161]
        [(eq? symbol 'snmp-trap) 162]
        [(eq? symbol 'https) 443]
        [(eq? symbol 'smtps) 465]
        [(eq? symbol 'isakmp) 500]
        [(eq? symbol 'syslog) 602]
        [(eq? symbol 'ldaps) 636]
        [(eq? symbol 'imaps) 993]
        [(eq? symbol 'pop3s) 995]
        [(eq? symbol 'openvpn) 1194]
        [(eq? symbol 'mysql) 3306]
        [(eq? symbol 'postgresql) 5432]
        [else (error "Unsupported TCP/UDP port" symbol)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Packet Attributes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A length<%> is a packet length.
;;   get-length : -> number
;;     Returns this length, in bytes
(define length<%> (interface (atom<%>) get-length))

;; number -> length<%>
;;   Creates a length<%> from a packet length
(provide/contract
 [new-length (-> number? (is-a?/c length<%>))])

(define (new-length len)
  (new length% [length len]))

;; symbol -> boolean
;;   Recognizes lengths in symbolic form
(provide/contract
 [length? (-> symbol? boolean?)])

(define (length? len)
  (with-handlers [([λ (exn)
                     (exn:fail? exn)]
                   [λ (exn)
                     #f])]
    (begin
      (parse-length len)
      #t)))

;; symbol -> length<%>
;;   Creates a length<%> from the atomic name thereof
(provide/contract
 [parse-length (-> symbol? (is-a?/c length<%>))])

(define length-rx #px"l-(\\d+)")

(define (parse-length len)
  (cond [(matching-symbol? len length-rx)
         (local [(define lengths (parse-symbol-parts len length-rx))]
           (new length% [length (string->number (first lengths))]))]
        [else (error "Unsupported length" len)]))

;; number
;;   A packet length
(define length%
  (class* object% (length<%>)
    (init-field length)
    (super-new)
    
    (define/public (equal-to? rhs equal-proc)
      (and (is-a? rhs atom<%>)
           (eq? (get-type-name) (send rhs get-type-name))
           (eq? (get-name) (send rhs get-name))))
    
    (define/public (equal-hash-code-of hash-proc)
      length)
    
    (define/public (equal-secondary-hash-code-of hash2-proc)
      length)
    
    (define/public (get-constraints)
      `((atmostone ,(get-name))))
    
    (define/public (get-name)
      (string->symbol (string-append "l-" (number->string length))))
    
    (define/public (covers? rhs)
      (and (is-a? rhs length<%>)
           (> length (send rhs get-length))))
    
    (define/public (single?)
      #t)
    
    (define/public (get-type-name)
      'Length)
    
    (define/public (get-length)
      length)
    ))
