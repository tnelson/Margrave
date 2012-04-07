; Copyright © 2009-2012 Brown University and Worcester Polytechnic Institute.
;
; This file is part of Margrave.

; Margrave is free software: you can redistribute it and/or modify
; it under the terms of the GNU Lesser General Public License as published by
; the Free Software Foundation, either version 3 of the License, or
; (at your option) any later version.
;
; Margrave is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
; GNU Lesser General Public License for more details.
;
; You should have received a copy of the GNU Lesser General Public License
; along with Margrave. If not, see <http://www.gnu.org/licenses/>.

#lang racket

(require "margrave.rkt"
         "IOS-parser/ios-compile.rkt")

; Others provided via provide/contract
(provide load-ios-policies)

; Routed Packets query for IOS parser
; tn april 2010
; Updates by tn through 2012


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide/contract (parse-and-load-ios-by-filename
                   ([string?]
                    [#:prefix string? #:suffix string? #:syntax syntax?]
                    . ->* . string?)))

(define (parse-and-load-ios-by-filename raw-filename #:prefix [prefix ""] #:suffix [suffix ""] #:syntax [src-syntax #f])

  ;; *MARGRAVE* --> margrave collection path
  (define the-filename (resolve-margrave-filename-keyword raw-filename))
  
  (file-exists?/error the-filename src-syntax (format "Could not find IOS configuration file: ~a" the-filename))       
  
  (define-values (fn-path fn-filepath must-dir) (split-path the-filename))
  (define fn-file (path->string fn-filepath))
  (when must-dir
    (raise-user-error (format "Expected a filename, but given: ~a." the-filename)))
  
  ; 'relative for the path if no "path" given

  ;(printf "~a ~a ~a ~a ~a~n" fn-file fn-path margrave-home-path fn-filepath the-filename)
  (if (equal? 'relative fn-path)
      (parse-and-load-ios fn-file (string->path ".") prefix suffix)
      (parse-and-load-ios fn-file fn-path prefix suffix))
  
  (string-append "Success: loaded IOS configuration at: " 
                 the-filename))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; parse-and-load-ios: First parses an IOS config file
;  and then calls load-ios-policies on the directory
;  containing the sub-policies.
(provide/contract (parse-and-load-ios
                   (string? (or/c path? string?) string? string?
                    . -> . void?)))

(define (parse-and-load-ios config-file-name dirpath prefix suffix)
  (printf "Parsing IOS configuration ~a in directory ~a ...~n" config-file-name dirpath) 
  
  ; May be a #path or a string. IOS parser expects a string.
  (cond [(string? dirpath)
         (compile-configurations dirpath (list config-file-name) #f)]
        [(path? dirpath)
         (compile-configurations (path->string dirpath) (list config-file-name) #f)]
        [else (raise-user-error (format "Expected a directory path in the second argument of parse-and-load-ios; given: ~a." dirpath))])
      
  (load-ios-policies dirpath prefix suffix))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; parse-and-load-multi-ios: Same as parse-and-load-ios, but
; allows multiple filenames per config

(provide/contract (parse-and-load-multi-ios 
                   ([(listof string?) (or/c path? string?)]
                    [#:prefix string? #:suffix string? #:syntax syntax?]
                    . ->* . string?)))
(define (parse-and-load-multi-ios config-file-name-list pre-dirpath #:prefix [prefix ""] #:suffix [suffix ""] #:syntax [src-syntax #f])

  (define dirpath (resolve-margrave-filename-keyword pre-dirpath))
   
  (printf "Parsing multiple IOS configurations in directory ~a ...~n" dirpath)
  
  (for-each (lambda (fn)   
              (file-exists?/error (build-path dirpath fn) src-syntax (format "Could not find IOS configuration file: ~a" (path->string (build-path dirpath fn)))))
            config-file-name-list)
  
  
  ; !!! todo factor out duplicate code -tn 
  
  ; May be a #path or a string. IOS parser expects a string.
  (cond [(string? dirpath)
         (compile-configurations dirpath config-file-name-list #f)]
        [(path? dirpath)
         (compile-configurations (path->string dirpath) config-file-name-list #f)]
        [else (raise-user-error (format "Expected a directory path in the second argument of parse-and-load-multi-ios; given: ~a." dirpath))])
      
  (load-ios-policies dirpath prefix suffix)
  
  (string-append "Success: loaded IOS configurations in: " 
                 dirpath))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (load-ios-helper filename dirpath prefix suffix) 
  (define pol-id (string-append prefix filename suffix))  
  (m-load-policy pol-id (build-path dirpath (string-append filename ".p")))      
  (printf "."))
  

; ------------------------------------------------
; load-ios-policies: Given a directory with post-parse sub-policies, load them all.
; Remember to start the engine before calling this.
; dirpath says where to find the policies
; prefix (and suffix) are prepended (and appended) to the 
;   policy's name to avoid naming conflicts.
(define (load-ios-policies dirpath prefix suffix (verbose #f))
  (printf "Loading IOS policies in path: ~a. Adding prefix: ~a and suffix: ~a ~n" dirpath prefix suffix)
   
  ; ACLs
  (load-ios-helper "InboundACL" dirpath prefix suffix)      
  (load-ios-helper "OutboundACL" dirpath prefix suffix) 
  
  ; NATs
  (load-ios-helper "InsideNAT" dirpath prefix suffix)  
  (load-ios-helper "OutsideNAT" dirpath prefix suffix)  
  
  ; Routing and switching
  (load-ios-helper "StaticRoute" dirpath prefix suffix)  
  (load-ios-helper "PolicyRoute" dirpath prefix suffix)  
  (load-ios-helper "DefaultPolicyRoute" dirpath prefix suffix)  
  
  (load-ios-helper "LocalSwitching" dirpath prefix suffix)  
  (load-ios-helper "NetworkSwitching" dirpath prefix suffix)  
 
  ; Encryption (not currently tested)
  (load-ios-helper "Encryption" dirpath prefix suffix)  

  (printf "~n")
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ; Internal-Result
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (define inboundacl (string->symbol (string-append prefix "InboundACL" suffix)))
  (define outboundacl (string->symbol (string-append prefix "OutboundACL" suffix)))
  (define insidenat (string->symbol (string-append prefix "InsideNAT" suffix)))
  (define outsidenat (string->symbol (string-append prefix "OutsideNAT" suffix)))
  
  (define staticroute (string->symbol (string-append prefix "StaticRoute" suffix)))  
  (define policyroute (string->symbol (string-append prefix "PolicyRoute" suffix)))
  (define defaultpolicyroute (string->symbol (string-append prefix "DefaultPolicyRoute" suffix)))
  
  (define localswitching (string->symbol (string-append prefix "LocalSwitching" suffix)))  
  (define networkswitching (string->symbol (string-append prefix "NetworkSwitching" suffix)))
  (define encryption (string->symbol (string-append prefix "Encryption" suffix)))
  
    (define vardec-20 '([ahostname Hostname]
                      [entry-interface Interface]
                      [src-addr-in IPAddress]
                      [src-addr_ IPAddress]
                      [src-addr-out IPAddress]
                      [dest-addr-in IPAddress]
                      [dest-addr_ IPAddress]
                      [dest-addr-out IPAddress]
                      [protocol Protocol-any]
                      [message ICMPMessage]
                      [flags TCPFlags]
                      [src-port-in Port]
                      [src-port_ Port]
                      [src-port-out Port]
                      [dest-port-in Port]
                      [dest-port_ Port]
                      [dest-port-out Port] 
                      [length Length]
                      [next-hop IPAddress]
                      [exit-interface Interface]))
  (define vardec-16 '([ahostname Hostname]
                      [entry-interface Interface]
                      [src-addr-in IPAddress]
                      [src-addr-out IPAddress]
                      [dest-addr-in IPAddress]
                      [dest-addr-out IPAddress]
                      [protocol Protocol-any]
                      [message ICMPMessage]
                      [flags TCPFlags]
                      [src-port-in Port]
                      [src-port-out Port]
                      [dest-port-in Port]
                      [dest-port-out Port] 
                      [length Length]
                      [next-hop IPAddress]
                      [exit-interface Interface]))
  
  (define inner-vec '(ahostname entry-interface src-addr_ src-addr_
                                dest-addr_ dest-addr_ protocol message flags src-port_ src-port_
                                dest-port_ dest-port_ length next-hop exit-interface))
  
  ; ACLs are not involved in internal-result, which is concerned with routing.
  (m-let (string-append prefix "internal-result" suffix) 
         vardec-20
         '(and ([,outsidenat translate] ahostname entry-interface src-addr-in src-addr_
                                        dest-addr-in dest-addr_ protocol message flags src-port-in src-port_
                                        dest-port-in dest-port_ length next-hop exit-interface)
               ([,insidenat translate]  ahostname entry-interface src-addr_ src-addr-out
                                       dest-addr_ dest-addr-out protocol message flags src-port_ src-port-out 
                                       dest-port_ dest-port-out length next-hop exit-interface)
               
               ; NAT translation confirmed. Now see if and how the packet is routed.
               (or ([,localswitching forward] ,inner-vec)
                   (and ([,localswitching pass] ,inner-vec)
                        (or ([,policyroute forward] ,inner-vec)
                            (and ([,policyroute route] ,inner-vec)
                                 ([,networkswitching forward] ,inner-vec))
                            (and ([,policyroute pass] ,inner-vec)
                                 (or ([,staticroute forward] ,inner-vec)
                                     (and ([,staticroute route] ,inner-vec)
                                          ([,networkswitching forward] ,inner-vec))
                                     (and ([,staticroute pass] ,inner-vec)
                                          (or ([,defaultpolicyroute forward] ,inner-vec)
                                              (and ([,defaultpolicyroute forward] ,inner-vec)
                                                   ([,networkswitching forward] ,inner-vec))
                                              ; Final option: Packet is dropped.
                                              (and ([,defaultpolicyroute pass] ,inner-vec)
                                                   (= next-hop dest-addr-out)
                                                   (Interf-drop exit-interface)))))))))))
  
; (define xml-response-ir (mtext (string-append "EXPLORE "
;  (string-append prefix "OutsideNAT" suffix)
;  ":translate(ahostname, entry-interface, src-addr-in, src-addr_,
;  dest-addr-in, dest-addr_, protocol, message, flags, src-port-in, src-port_,
;  dest-port-in, dest-port_, length, next-hop, exit-interface)
;AND
;
;( "
;  (string-append prefix "LocalSwitching" suffix)
;  ":Forward(ahostname, entry-interface, src-addr_, src-addr_,
;  dest-addr_, dest-addr_, protocol, message, flags, src-port_, src-port_,
;  dest-port_, dest-port_, length, next-hop, exit-interface)
;  OR
;
;  ( "
;  (string-append prefix "LocalSwitching" suffix)
;    ":Pass(ahostname, entry-interface, src-addr_, src-addr_,
;    dest-addr_, dest-addr_, protocol, message, flags, src-port_, src-port_,
;    dest-port_, dest-port_, length, next-hop, exit-interface)
;    AND
;    ( "
;      (string-append prefix "PolicyRoute" suffix)
;      ":Forward(ahostname, entry-interface, src-addr_, src-addr_,
;      dest-addr_, dest-addr_, protocol, message, flags, src-port_, src-port_,
;      dest-port_, dest-port_, length, next-hop, exit-interface)
;      OR
;      ( "
;        (string-append prefix "PolicyRoute" suffix)
;        ":Route(ahostname, entry-interface, src-addr_, src-addr_,
;        dest-addr_, dest-addr_, protocol, message, flags, src-port_, src-port_,
;        dest-port_, dest-port_, length, next-hop, exit-interface)
;        AND "
;          (string-append prefix "NetworkSwitching" suffix)
;        ":Forward(ahostname, entry-interface, src-addr_,
;        src-addr_, dest-addr_, dest-addr_, protocol, message, flags,
;        src-port_, src-port_, dest-port_, dest-port_, length,
;        next-hop, exit-interface)
;      )
;      OR
;      ( "
;          (string-append prefix "PolicyRoute" suffix)
;        ":Pass(ahostname, entry-interface, src-addr_, src-addr_,
;        dest-addr_, dest-addr_, protocol, message, flags, src-port_,
;        src-port_, dest-port_, dest-port_, length, next-hop,
;        exit-interface)
;        AND
;        ( "
;          (string-append prefix "StaticRoute" suffix)
;          ":Forward(ahostname, entry-interface, src-addr_,
;          src-addr_, dest-addr_, dest-addr_, protocol, message, flags,
;          src-port_, src-port_, dest-port_, dest-port_, length,
;          next-hop, exit-interface)
;          OR
;          ( "
;            (string-append prefix "StaticRoute" suffix)
;            ":Route(ahostname, entry-interface, src-addr_,
;            src-addr_, dest-addr_, dest-addr_, protocol, message, flags,
;            src-port_, src-port_, dest-port_, dest-port_, length,
;            next-hop, exit-interface)
;            AND "
;            (string-append prefix "NetworkSwitching" suffix)
;            ":Forward(ahostname, entry-interface, src-addr_,
;            src-addr_, dest-addr_, dest-addr_, protocol, message, flags,
;            src-port_, src-port_, dest-port_, dest-port_, length,
;            next-hop, exit-interface)
;          )
;          OR
;          ( "
;            (string-append prefix "StaticRoute" suffix)
;            ":Pass(ahostname, entry-interface, src-addr_, src-addr_,
;            dest-addr_, dest-addr_, protocol, message, flags, src-port_,
;            src-port_, dest-port_, dest-port_, length, next-hop,
;            exit-interface)
;            AND
;            ( "
;              (string-append prefix "DefaultPolicyRoute" suffix)
;              ":Forward(ahostname, entry-interface,
;              src-addr_, src-addr_, dest-addr_, dest-addr_,
;              protocol, message, flags, src-port_, src-port_, dest-port_,
;              dest-port_, length, next-hop, exit-interface)
;              OR
;              ( "
;              (string-append prefix "DefaultPolicyRoute" suffix)
;              ":Route(ahostname, entry-interface,
;                src-addr_, src-addr_, dest-addr_, dest-addr_,
;                protocol, message, flags, src-port_, src-port_, dest-port_,
;                dest-port_, length, next-hop, exit-interface)
;                AND "
;              (string-append prefix "NetworkSwitching" suffix)
;              ":Forward(ahostname, entry-interface,
;                src-addr_, src-addr_, dest-addr_, dest-addr_,
;                protocol, message, flags, src-port_, src-port_, dest-port_,
;                dest-port_, length, next-hop, exit-interface)
;              )"
;              
;              ;final case: unrouted. DPR finds no next-hop, so the packet
;              ; has nowhere to go
;              "OR 
;               ( "
;              (string-append prefix "DefaultPolicyRoute" suffix)
;              ":Pass(ahostname, entry-interface,
;                src-addr_, src-addr_, dest-addr_, dest-addr_,
;                protocol, message, flags, src-port_, src-port_, dest-port_,
;                dest-port_, length, next-hop, exit-interface)
;                AND 
;               (next-hop = dest-addr-out) AND
;               interf-drop(exit-interface)"
;
;"             )
;            )
;          )
;        )
;      )
;    )
;  )
;)
;
;AND "
;              (string-append prefix "InsideNAT" suffix)
;              ":Translate()
;
;PUBLISH ahostname, entry-interface, 
;        src-addr-in, src-addr_, src-addr-out, 
;        dest-addr-in, dest-addr_, dest-addr-out, 
;        protocol, message, flags,
;        src-port-in, src-port_, src-port-out, 
;        dest-port-in, dest-port_, dest-port-out, 
;        length, next-hop, exit-interface
;
;TUPLING"))) 
  



  ;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ; Int-dropped: Just check for NOT interf-drop(exit-interface).
  ; Need to use an UNDER clause since there are no policies appearing.
  ; 
  ; Technically there's no reason to have this IDB take more than exit-interface.
  ; However, for now, we use the standard policy request vector, and so must
  ; use EACH variable in the query definition.
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
   (m-let (string-append prefix "int-dropped" suffix) 
         vardec-16
         '(and (Interf-drop exit-interface)
               ) #:under '((symbol->string inboundacl)))
  
  ;(define xml-response-id (mtext (string-append "EXPLORE interf-drop(exit-interface) AND
; IPAddress(src-addr-in) AND IPAddress(src-addr-out) AND IPAddress(dest-addr-in) AND IPAddress(dest-addr-out) AND 
;Port(dest-port-out) AND Port(dest-port-in) AND Port(src-port-out) AND Port(src-port-in) AND IPAddress(next-hop) AND 
;ICMPMessage(message) AND Interface(entry-interface) AND Interface(exit-interface) and Length(length) AND 
;Protocol-any(protocol) AND Hostname(ahostname) AND TCPFlags(flags)

  
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ; Firewall-Passed
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  ;; Do NOT assert internal-result in this query. Why? Because then it cannot be safely
  ;; negated to mean the packets the firewall drops (or rejects). It would then also
  ;; include all the nonsensical scenarios...
  ; Therefore, this query doesn't have the full arity of internal-result. (No src-addr_ etc.)
  
  
  (define aclinvec '(ahostname entry-interface src-addr-in src-addr-in
                               dest-addr-in dest-addr-in protocol message flags src-port-in src-port-in
                               dest-port-in dest-port-in length next-hop exit-interface))
  
  (define acloutvec '(ahostname entry-interface src-addr-out src-addr-out
                                dest-addr-out dest-addr-out protocol message flags src-port-out
                                src-port-out dest-port-out dest-port-out length next-hop
                                exit-interface))
    
  (m-let (string-append prefix "passes-firewall" suffix) 
         vardec-16
         `(and (not (Interf-drop exit-interface))
               ([,inboundacl permit] ,@aclinvec)
               ([,outboundacl permit] ,@acloutvec)))
  
  ; end
  )

  
