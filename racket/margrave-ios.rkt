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
(provide load-ios-policies
         vardec-internal
         )


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

; 14 variables exposing modification of packet and exit interface,
; hiding internal addresses used between in/out nat, next-hop.
(define vardec-internal '([ahostname Hostname]
                          [entry Interface]
                          [sa IPAddress]
                          [da IPAddress]
                          [sp Port]
                          [dp Port]
                          [protocol Protocol-any]
                          [paf PayloadAndFlags]                          
                          [exit Interface]
                          [sa2 IPAddress]
                          [da2 IPAddress]
                          [sp2 Port]
                          [dp2 Port]))

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
 
    
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ; ACLs are not involved in internal-result, which is concerned with routing, switching and NAT.
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  (m-let (string-append prefix "internal-result" suffix) 
         vardec-internal
         
         `(exists src-addr_ IPAddress
            (exists dest-addr_ IPAddress
              (exists src-port_ Port
               (exists dest-port_ Port
                 (exists next-hop IPAddress
                         
           (and ([,outsidenat translate] ahostname entry 
                                        sa da sp dp protocol
                                        src-addr_ dest-addr_ src-port_ dest-port_)
               
           
           
               ([,insidenat translate] ahostname entry 
                                       src-addr_ dest-addr_ src-port_ dest-port_ protocol
                                       sa2 da2 sp2 dp2)
                              
               ; NAT translation confirmed. Now see if and how the packet is routed.
               ; Routing can either go to a gateway (route) or an interface (forward)   
               ; Local routing just checks if the destination is a local interface.
                        
               ; First option: localswitching sends immediately, because the destination is attached.
               ; In this case next-hop isn't strictly needed, so we set it equal to destination to avoid confusion.
               
               ; Other options use networkswitching and one of the routing policies.
               ; The routing policies will fix next-hop.
               
               ; !!! TN: I am concerned about that places that (= next-hop dest-addr-in) still appears in the ios.ss file.
               ; If odd behavior occurs with switching, examine that first.
               (or (and (= next-hop dest-addr_) 
                        ([,localswitching forward] ahostname dest-addr_ exit))
                   (and ([,localswitching pass] ahostname dest-addr_ exit))
                   
                        (or ([,policyroute forward] ahostname entry src-addr_ dest-addr_ src-port_ dest-port_ protocol next-hop exit)
                            (and ([,policyroute route] ahostname entry src-addr_ dest-addr_ src-port_ dest-port_ protocol next-hop exit)
                                 ([,networkswitching forward] ahostname next-hop exit))
                            (and ([,policyroute pass] ahostname entry src-addr_ dest-addr_ src-port_ dest-port_ protocol next-hop exit)
                                 (or ([,staticroute forward] ahostname dest-addr_ next-hop exit)
                                     (and ([,staticroute route] ahostname dest-addr_ next-hop exit)
                                          ([,networkswitching forward] ahostname next-hop exit))
                                     (and ([,staticroute pass] ahostname dest-addr_ next-hop exit)
                                          (or ([,defaultpolicyroute forward] ahostname entry src-addr_ dest-addr_ src-port_ dest-port_ protocol next-hop exit)
                                              (and ([,defaultpolicyroute route] ahostname entry src-addr_ dest-addr_ src-port_ dest-port_ protocol next-hop exit)
                                                   ([,networkswitching forward] ahostname next-hop exit))
                                              ; Final option: Packet is dropped.
                                              (and ([,defaultpolicyroute pass] ahostname entry src-addr_ dest-addr_ src-port_ dest-port_ protocol next-hop exit)
                                                   (= next-hop dest-addr_)
                                                   (interf-drop exit)))))))))))))))
  
  
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ; Int-dropped: Just check for NOT interf-drop(exit-interface).
  ; Need to use an UNDER clause since there are no policies appearing.
  ; 
  ; Technically there's no reason to have this IDB take more than exit-interface.
  ; However, for now, we use the standard policy request vector, and so must
  ; use EACH variable in the query definition.
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  ; (m-let (string-append prefix "int-dropped" suffix) 
  ;       vardec-16
  ;       `(and (Interf-drop exit-interface)
  ;             ) #:under (list (symbol->string inboundacl)))
  
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
  
  ;;;;;;;;; ^^^^ REVISE THIS: is this restriction still needed?
    
  ; paf contains what were once separate: message, flags, length.
  ; next-hop is only represented inside internal-result
  
  (define aclinvec '(ahostname entry sa da sp dp protocol paf))  
  (define acloutvec '(ahostname exit sa2 da2 sp2 dp2 protocol paf)) 
      
  (m-let (string-append prefix "passes-firewall" suffix)          
         vardec-internal
         `(and (not (interf-drop exit))
               ([,inboundacl permit] ,@aclinvec )
               ([,outboundacl permit] ,@acloutvec)))  
  ; end
  )

  
