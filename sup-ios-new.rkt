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

#lang margrave/racket







; In progress: What do we WANT the superfluous checker program to look like?
; (Much of this syntax is not yet supported, and may not even be supported in
; the final version.)






; Originally written for SISC by tn
; Re-written for Racket by tn

; todo: too much string manipulation going on here. Is it possible to send back more structured IDB data?
;       for example, why not send a triple: (collectionname, idbname, tupling-data)?

; todo: Use sets rather than lists for efficiency (we do intersections)


; ********************************************************
; Helper functions
; ********************************************************


(define (keys-not-mapped-to-empty a-map)
  (filter (lambda (x) x) ; non-false?
          (hash-map a-map
                    (lambda (key value)
                      (if (not (empty? (hash-ref a-map key)))
                          key
                          #f)))))

;(define (makeIdbList lst)
;  (if (equal? '() lst)
;      ""
;      (let ([ idbname (first lst)])
;        (string-append idbname 
;                       "("
;                       reqVector
;                       ")"
;                       (if (equal? '() (rest lst))
;                           ""
;                           ", ")
;                       (makeIdbList (rest lst))))))

;(define (list-intersection lst1 lst2)
;  ;(printf "INTERSECT: ~a ~n ~a ~n" lst1 lst2)
;  (filter (lambda (x) (not (equal? (member x lst2) #f))) lst1))

; for -keep-collection
; replace rule-name with (string-append pol-name ":" rule-name)

(define (cleanup-idb-list-no-applies thelist)
  (map (lambda (idbname)
         (let* ([qualified-rule (first (regexp-split "_applies\\[" idbname))]
                [fragments (regexp-split ":" qualified-rule)]
                [pol-name (first fragments)]
                [rule-name (string-join (rest fragments) ":")])
           rule-name))
       thelist))

; *********************************************************************

; ********************************************************
; Detecting overlaps for shadowed rules
; ********************************************************

(define (find-overshadowed neverApplyList log-file candidates cases)

    (time-since-last) ;; reset timer
  
  (define denyOverlapPermitId 
    (xml-explore-result->id #M
                            EXPLORE true 
                            UNDER inboundacl 
                            INCLUDE |(string-append candidates ", " cases)|
                            TUPLING ))

  (define themap
    (xml-map-response->map #M                           
                           SHOW REALIZED |denyOverlapPermitId| |candidates| FOR CASES |cases|))    
    
    themap)



; ********************************************************
; Detecting superfluous rules
; ********************************************************


(define (timed-check-superfluous pFileName)
  
  ; Start the Java process
;  (start-margrave-engine (current-directory) '("-Xss2048k" "-Xmx1g") '("-log"))   ;; remove log for benchmarking
    (start-margrave-engine (current-directory) '("-Xss2048k" "-Xmx1g") '())   ;; remove log for benchmarking
      
  ; Start the timer
  (time-since-last)
  
  ; Load the policy
  (define log-file (open-output-file "sup-benchmarking.csv" #:exists 'append))
  
  (define polname #M 
    LOAD POLICY |pFileName|)
  
  (define n-load-time (time-since-last))
  (printf "Loading took: ~a milliseconds.~n" n-load-time)  
  (write-string (string-append (number->string n-load-time) ", ") log-file)
    
  ; !!!!
  ; *** At the moment, these helper funcs are implemented using mtext. Will of course need to change them when this is finalized.
  
  (define all-rules (get-rule-list polname))
  (define all-applied (make-applies-list all-rules "InboundACL")) 
  (define all-matches (make-matches-list all-rules "InboundACL"))
  (define all-permit-rules (get-rule-list polname "permit"))
  (define all-deny-rules (get-rule-list polname "deny"))
  (define all-permit-applied (make-applies-list all-permit-rules "InboundACL"))
  (define all-deny-applied (make-applies-list all-deny-rules "InboundACL"))
  (define all-permit-matches (make-matches-list all-permit-rules "InboundACL"))
  (define all-deny-matches (make-matches-list all-deny-rules "InboundACL"))
  
 ; (define idblistmatches (makeIdbList all-matches))
 ; (define idblistapplied (makeIdbList all-applied))         
  
  ; !!!!!!!!!!!!!!!!!!!
  ; *** Do we want this converter in here? Why make the user deal with this XML at all?
  ; *** Same with other converters
  
  (define neverApplyId 
    (xml-explore-result->id #M 
                            EXPLORE true 
                            UNDER inboundacl
                            INCLUDE  |all-applied| // Not right, missing vector part
                            TUPLING ))
            
  (printf "Time to make lists, strings, etc.: ~a milliseconds.~n" (time-since-last))
  
  (printf "Number of permit rules: ~a.~nNumber of deny rules: ~a.~nTotal rules: ~a~n"
          (length all-permit-rules) (length all-deny-rules) (length all-rules))
  (printf "Running superfluous-rule finder...~n")
  
  ; **********************************************************************************************************
      
  (define neverApplyList 
    (cleanup-idb-list-no-applies (xml-set-response->list #M 
                                                         SHOW UNREALIZED |neverApplyId| |idblistapplied| )))
  
  (define n-sup-time (time-since-last))
  (printf "superfluous-rule finder took: ~a milliseconds.~n" n-sup-time)
  
  (define idblistpa (makeIdbList all-permit-applied))
  (define idblistdn-sup (makeIdbList (make-matches-list (list-intersection all-deny-rules neverApplyList) "InboundACL")))
  (define idblistda (makeIdbList all-deny-applied))
  (define idblistpn-sup (makeIdbList (make-matches-list (list-intersection all-permit-rules neverApplyList) "InboundACL")))

  
  ; !!!!!!!
  ; *** Use Racket's set library for this
  
  (define n-list-time (time-since-last))      
  (printf "Creating list intersections took: ~a milliseconds.~n" n-list-time) 
    
  (write-string (string-append (number->string n-sup-time) ", ") log-file)
  (write-string (string-append (number->string n-list-time) ", ") log-file)
    
  (printf "superfluous rules count:~n~a~n" (length neverApplyList))
  
  ;(printf "~a ~n~n~n ~a ~n~n~n" idblistpn-sup idblistdn-sup)
  ;(printf "idblists: ~a~n~n~a~n~n" idblistdn-sup idblistpn-sup)
    
        
  ; Look for permits overlapping denies (and vice versa)
  (define povershadowed (keys-not-mapped-to-empty 
                         (find-overshadowed neverApplyList log-file idblistda idblistpn-sup)))
  (define n-p-runtime (time-since-last))
  (define dovershadowed (keys-not-mapped-to-empty 
                         (find-overshadowed neverApplyList log-file idblistpa idblistdn-sup)))
  (define n-d-runtime (time-since-last))  

  (write-string (string-append (number->string n-p-runtime) ", ") log-file)
  (write-string (string-append (number->string n-d-runtime) ", ") log-file)
  
  (printf "Number of never-firing permits overlapped by denies: ~a ~nTime: ~a~n" (length povershadowed) n-runtime)
  (printf "Number of never-firing denies overlapped by permits: ~a ~nTime: ~a~n" (length dovershadowed) n-runtime)
  
  (write-string "\n" log-file)
  (close-output-port log-file))))) 
; Keep the JVM open afterward


(define (benchmark num-trials file-name)
  (when (> num-trials 0)   
    (printf " ~a trials left...~n" num-trials)
    (run-timed-script file-name)    
    (benchmark (- num-trials 1) file-name)))
