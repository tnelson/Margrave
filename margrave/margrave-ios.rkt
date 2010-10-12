; Copyright © 2009-2010 Brown University and Worcester Polytechnic Institute.
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

; Assume same dir for now (later, module path!)
(require margrave/margrave
         margrave/IOS-parser/ios-compile)

(provide load-ios-policies )

; Routed Packets query for IOS parser
; tn april 2010
; updated tn july 2010
; updated for release aug-sept 2010


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
  (let ([ polname (load-policy (build-path dirpath (string-append filename ".p")))])
    
    ;(printf ".~a~n" filename)
    (printf ".")
    
    (when (or (> (string-length prefix) 0)
              (> (string-length suffix) 0))
      (mtext (string-append "RENAME " polname " " prefix polname suffix))))) 
  

; ------------------------------------------------
; load-ios-policies: Given a directory with post-parse sub-policies, load them all.
; Remember to start the engine before calling this.
; dirpath says where to find the policies
; prefix (and suffix) are prepended (and appended) to the 
;   policy's name to avoid naming conflicts.
(define (load-ios-policies dirpath prefix suffix (verbose #f))

  (time-since-last) ; init timer
  
  (printf "Loading IOS policies in path: ~a. Adding prefix: ~a and suffix: ~a ~n" dirpath prefix suffix)
  
  ; ACLs
  (load-ios-helper "InboundACL" dirpath prefix suffix)    
  (when (equal? #t verbose)
    (printf "Time to load InboundACL: ~a ms.~n" (time-since-last))
    (display-response (mtext "INFO")))
  
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
  (when (equal? #t verbose)
    (printf "Time to load all other sub-policies: ~a ms.~n" (time-since-last))
    (display-response (mtext "INFO")))

 ; (printf "Time to load policy files + vocabularies: ~a milliseconds.~n" (time-since-last))

  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ; Internal-Result
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  (define xml-response-ir (mtext (string-append "EXPLORE "
  (string-append prefix "OutsideNAT" suffix)
  ":translate(ahostname, entry-interface, src-addr-in, src-addr_,
  dest-addr-in, dest-addr_, protocol, message, flags, src-port-in, src-port_,
  dest-port-in, dest-port_, length, next-hop, exit-interface)
AND

( "
  (string-append prefix "LocalSwitching" suffix)
  ":Forward(ahostname, entry-interface, src-addr_, src-addr_,
  dest-addr_, dest-addr_, protocol, message, flags, src-port_, src-port_,
  dest-port_, dest-port_, length, next-hop, exit-interface)
  OR

  ( "
  (string-append prefix "LocalSwitching" suffix)
    ":Pass(ahostname, entry-interface, src-addr_, src-addr_,
    dest-addr_, dest-addr_, protocol, message, flags, src-port_, src-port_,
    dest-port_, dest-port_, length, next-hop, exit-interface)
    AND
    ( "
      (string-append prefix "PolicyRoute" suffix)
      ":Forward(ahostname, entry-interface, src-addr_, src-addr_,
      dest-addr_, dest-addr_, protocol, message, flags, src-port_, src-port_,
      dest-port_, dest-port_, length, next-hop, exit-interface)
      OR
      ( "
        (string-append prefix "PolicyRoute" suffix)
        ":Route(ahostname, entry-interface, src-addr_, src-addr_,
        dest-addr_, dest-addr_, protocol, message, flags, src-port_, src-port_,
        dest-port_, dest-port_, length, next-hop, exit-interface)
        AND "
          (string-append prefix "NetworkSwitching" suffix)
        ":Forward(ahostname, entry-interface, src-addr_,
        src-addr_, dest-addr_, dest-addr_, protocol, message, flags,
        src-port_, src-port_, dest-port_, dest-port_, length,
        next-hop, exit-interface)
      )
      OR
      ( "
          (string-append prefix "PolicyRoute" suffix)
        ":Pass(ahostname, entry-interface, src-addr_, src-addr_,
        dest-addr_, dest-addr_, protocol, message, flags, src-port_,
        src-port_, dest-port_, dest-port_, length, next-hop,
        exit-interface)
        AND
        ( "
          (string-append prefix "StaticRoute" suffix)
          ":Forward(ahostname, entry-interface, src-addr_,
          src-addr_, dest-addr_, dest-addr_, protocol, message, flags,
          src-port_, src-port_, dest-port_, dest-port_, length,
          next-hop, exit-interface)
          OR
          ( "
            (string-append prefix "StaticRoute" suffix)
            ":Route(ahostname, entry-interface, src-addr_,
            src-addr_, dest-addr_, dest-addr_, protocol, message, flags,
            src-port_, src-port_, dest-port_, dest-port_, length,
            next-hop, exit-interface)
            AND "
            (string-append prefix "NetworkSwitching" suffix)
            ":Forward(ahostname, entry-interface, src-addr_,
            src-addr_, dest-addr_, dest-addr_, protocol, message, flags,
            src-port_, src-port_, dest-port_, dest-port_, length,
            next-hop, exit-interface)
          )
          OR
          ( "
            (string-append prefix "StaticRoute" suffix)
            ":Pass(ahostname, entry-interface, src-addr_, src-addr_,
            dest-addr_, dest-addr_, protocol, message, flags, src-port_,
            src-port_, dest-port_, dest-port_, length, next-hop,
            exit-interface)
            AND
            ( "
              (string-append prefix "DefaultPolicyRoute" suffix)
              ":Forward(ahostname, entry-interface,
              src-addr_, src-addr_, dest-addr_, dest-addr_,
              protocol, message, flags, src-port_, src-port_, dest-port_,
              dest-port_, length, next-hop, exit-interface)
              OR
              ( "
              (string-append prefix "DefaultPolicyRoute" suffix)
              ":Route(ahostname, entry-interface,
                src-addr_, src-addr_, dest-addr_, dest-addr_,
                protocol, message, flags, src-port_, src-port_, dest-port_,
                dest-port_, length, next-hop, exit-interface)
                AND "
              (string-append prefix "NetworkSwitching" suffix)
              ":Forward(ahostname, entry-interface,
                src-addr_, src-addr_, dest-addr_, dest-addr_,
                protocol, message, flags, src-port_, src-port_, dest-port_,
                dest-port_, length, next-hop, exit-interface)
              )"
              
              ;final case: unrouted. DPR finds no next-hop, so the packet
              ; has nowhere to go
              "OR 
               ( "
              (string-append prefix "DefaultPolicyRoute" suffix)
              ":Pass(ahostname, entry-interface,
                src-addr_, src-addr_, dest-addr_, dest-addr_,
                protocol, message, flags, src-port_, src-port_, dest-port_,
                dest-port_, length, next-hop, exit-interface)
                AND 
               (next-hop = dest-addr-out) AND
               interf-drop(exit-interface)"

"             )
            )
          )
        )
      )
    )
  )
)

AND "
              (string-append prefix "InsideNAT" suffix)
              ":Translate(ahostname, entry-interface, src-addr_, src-addr-out,
  dest-addr_, dest-addr-out, protocol, message, flags, src-port_,
  src-port-out, dest-port_, dest-port-out, length, next-hop,
  exit-interface)

PUBLISH ahostname, entry-interface, 
        src-addr-in, src-addr_, src-addr-out, 
        dest-addr-in, dest-addr_, dest-addr-out, 
        protocol, message, flags,
        src-port-in, src-port_, src-port-out, 
        dest-port-in, dest-port_, dest-port-out, 
        length, next-hop, exit-interface

TUPLING"))) 
  
  (mtext (string-append "RENAME LAST " prefix "internal-result" suffix))


  ;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ; Int-dropped: Just check for NOT interf-drop(exit-interface).
  ; Need to use an UNDER clause since there are no policies appearing.
  ; 
  ; Technically there's no reason to have this IDB take more than exit-interface.
  ; However, for now, we use the standard policy request vector, and so must
  ; use EACH variable in the query definition.
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (define xml-response-id (mtext (string-append "EXPLORE interf-drop(exit-interface) AND

IPAddress(src-addr-in) AND IPAddress(src-addr-out) AND IPAddress(dest-addr-in) AND IPAddress(dest-addr-out) AND 
Port(dest-port-out) AND Port(dest-port-in) AND Port(src-port-out) AND Port(src-port-in) AND IPAddress(next-hop) AND 
ICMPMessage(message) AND Interface(entry-interface) AND Interface(exit-interface) and Length(length) AND 
Protocol-any(protocol) AND Hostname(ahostname) AND TCPFlags(flags)

UNDER " prefix "InboundACL" suffix " 

PUBLISH ahostname, entry-interface, 
        src-addr-in,  src-addr-out, 
        dest-addr-in, dest-addr-out, 
        protocol, message, flags,
        src-port-in,  src-port-out, 
        dest-port-in,  dest-port-out, 
        length, next-hop, exit-interface

TUPLING")))
  (mtext (string-append "RENAME LAST " prefix "int-dropped" suffix))
  
  
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ; Firewall-Passed
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  ;; Do NOT assert internal-result in this query. Why? Because then it cannot be safely
  ;; negated to mean the packets the firewall drops (or rejects). It would then also
  ;; include all the nonsensical scenarios...
  ; Therefore, this query doesn't have the full arity of internal-result. (No src-addr_ etc.)
  (define xml-response-pf (mtext (string-append "EXPLORE NOT interf-drop(exit-interface) AND " 
                        prefix "InboundACL" suffix
                        ":permit(ahostname, entry-interface, src-addr-in, src-addr-in,
  dest-addr-in, dest-addr-in, protocol, message, flags, src-port-in, src-port-in,
  dest-port-in, dest-port-in, length, next-hop, exit-interface) AND " 
                        prefix "OutboundACL" suffix
                        ":Permit(ahostname, entry-interface, src-addr-out, src-addr-out,
  dest-addr-out, dest-addr-out, protocol, message, flags, src-port-out,
  src-port-out, dest-port-out, dest-port-out, length, next-hop,
  exit-interface)

PUBLISH ahostname, entry-interface, 
        src-addr-in,  src-addr-out, 
        dest-addr-in, dest-addr-out, 
        protocol, message, flags,
        src-port-in,  src-port-out, 
        dest-port-in,  dest-port-out, 
        length, next-hop, exit-interface

TUPLING")))
  (mtext (string-append "RENAME LAST " prefix "passes-firewall" suffix))

  (when (equal? #t verbose)
    (printf "Time to create and save internal-result, int-dropped, and passes-firewall: ~a milliseconds.~n" (time-since-last)))
  
) ; end of function (more clear as lone paren; we are adding more)