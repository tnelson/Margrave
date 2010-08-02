#lang racket
(require (planet schematics/schemeunit:3:4) ;Schemeunit
         (planet schematics/schemeunit:3:4/text-ui)
         "margrave-xml.rkt" "parser-compiler.rkt"
         xml)

;*************************************************************************
;parser-compiler
(evalxml "info")

;*************************************************************************
;XML

(define test-model (read-xml (open-input-string
                           "<MARGRAVE-RESPONSE type=\"model\">
<MODEL size=\"3\">
<RELATION arity=\"1\" name=\"author\">
<TUPLE>
<ATOM>Atom0</ATOM>
</TUPLE>
</RELATION>
<RELATION arity=\"1\" name=\"paper\">
<TUPLE>
<ATOM>Atom1</ATOM>
</TUPLE>
</RELATION>
<RELATION arity=\"1\" name=\"subject\">
<TUPLE>
<ATOM>Atom0</ATOM>
</TUPLE>
</RELATION>
<RELATION arity=\"1\" name=\"resource\">
<TUPLE>
<ATOM>Atom1</ATOM>
</TUPLE>
</RELATION>
<RELATION arity=\"1\" name=\"submitreview\" />
<RELATION arity=\"1\" name=\"action\">
<TUPLE>
<ATOM>Atom2</ATOM>
</TUPLE>
</RELATION>
<RELATION arity=\"1\" name=\"reviewer\" />
<RELATION arity=\"1\" name=\"readpaper\">
<TUPLE>
<ATOM>Atom2</ATOM>
</TUPLE>
</RELATION>
<RELATION arity=\"1\" name=\"review\" />
<RELATION arity=\"1\" name=\"submitpaper\" />
<RELATION arity=\"2\" name=\"conflicted\">
<TUPLE>
<ATOM>Atom0</ATOM>
<ATOM>Atom1</ATOM>
</TUPLE>
</RELATION>
<RELATION arity=\"2\" name=\"assigned\" />
<RELATION arity=\"1\" name=\"$r\">
<TUPLE>
<ATOM>Atom1</ATOM>
</TUPLE>
</RELATION>
<RELATION arity=\"1\" name=\"$a\">
<TUPLE>
<ATOM>Atom2</ATOM>
</TUPLE>
</RELATION>
<RELATION arity=\"1\" name=\"$s\">
<TUPLE>
<ATOM>Atom0</ATOM>
</TUPLE>
</RELATION>
<ANNOTATION>firewall1:accept is true for: [ipsrc, ipdest, portsrc, portdest, pro]</ANNOTATION>
</MODEL>
<STATISTICS computed-max-size=\"1\" max-size=\"1\" result-id=\"0\" user-max-size=\"6\"/>
</MARGRAVE-RESPONSE>")))

;Test that warnings appear if:
;the user ceiling is lower than the calculated ceiling
;or if the calculated ceiling is -1 (infinitary)
(define low-user-ceiling (read-xml (open-input-string
                           "<MARGRAVE-RESPONSE type=\"model\">
<MODEL size=\"3\">
<RELATION arity=\"1\" name=\"author\">
<TUPLE>
<ATOM>Atom0</ATOM>
</TUPLE>
</RELATION>
</MODEL>
<STATISTICS computed-max-size=\"1\" max-size=\"1\" result-id=\"0\" user-max-size=\"0\"/>
</MARGRAVE-RESPONSE>")))

(define negative-ceiling (read-xml (open-input-string
                           "<MARGRAVE-RESPONSE type=\"model\">
<MODEL size=\"3\">
<RELATION arity=\"1\" name=\"author\">
<TUPLE>
<ATOM>Atom0</ATOM>
</TUPLE>
</RELATION>
</MODEL>
<STATISTICS computed-max-size=\"-1\" max-size=\"1\" result-id=\"0\" user-max-size=\"0\"/>
</MARGRAVE-RESPONSE>")))

(define test-sys-info (read-xml (open-input-string
                                 "<MARGRAVE-RESPONSE type=\"sysinfo\">
<MANAGER atoms=\"9\" conjunctions=\"46\" decls=\"0\" disjunctions=\"11\" multiplicity=\"0\" negations=\"12\" num-variables=\"5\" q-exists=\"0\" q-forall=\"0\" relations=\"10\" total-formulas=\"78\" total-reclaimed=\"0\" variable-tuples=\"4\">
<HEAP-USAGE init=\"49729280\" max=\"708378624\" units=\"bytes\" used=\"9989928\"/>
<NON-HEAP-USAGE init=\"19136512\" max=\"117440512\" units=\"bytes\" used=\"4500392\"/>
</MANAGER>
<VOCABULARIES count=\"1\"/>
<COLLECTIONS count=\"1\"/>
<CACHED-RESULTS count=\"0\"/>
</MARGRAVE-RESPONSE>")))

(define test-coll-info (read-xml (open-input-string 
                                  "<MARGRAVE-RESPONSE type=\"collection-info\">
<POLICY-LEAF name=\"conf1\" rule-combine=\"fac\">
<IDBS>
<IDB base-name=\"paperconflict_applies\">conf1:paperconflict_applies</IDB>
<IDB base-name=\"paperconflict\">conf1:paperconflict</IDB>
<IDB base-name=\"deny\">conf1:deny</IDB>
<IDB base-name=\"permit\">conf1:permit</IDB>
<IDB base-name=\"papernoconflict_applies\">conf1:papernoconflict_applies</IDB>
<IDB base-name=\"paperassigned_applies\">conf1:paperassigned_applies</IDB>
<IDB base-name=\"papernoconflict\">conf1:papernoconflict</IDB>
<IDB base-name=\"paperassigned\">conf1:paperassigned</IDB>
</IDBS>
<FREE-VARIABLES>
<VARIABLE>s</VARIABLE>
<VARIABLE>a</VARIABLE>
<VARIABLE>r</VARIABLE>
</FREE-VARIABLES>
</POLICY-LEAF>
</MARGRAVE-RESPONSE>")))

(define test-vocab-info (read-xml (open-input-string
                                   "<MARGRAVE-RESPONSE type=\"vocabulary-info\">
<VOCABULARY name=\"examplefw1\">
<SORTS>
<SORT name=\"port80\"/>
<SORT name=\"port\">
<SORT name=\"port80\"/>
<SORT name=\"port21\"/>
</SORT>
<SORT name=\"port21\"/>
<SORT name=\"protocol\">
<SORT name=\"tcp\"/>
<SORT name=\"udp\"/>
</SORT>
<SORT name=\"ftpserver\"/>
<SORT name=\"udp\"/>
<SORT name=\"blacklistaddr\"/>
<SORT name=\"tcp\"/>
<SORT name=\"webserver\"/>
<SORT name=\"ipaddress\">
<SORT name=\"webserver\"/>
<SORT name=\"ftpserver\"/>
<SORT name=\"blacklistaddr\"/>
</SORT>
</SORTS>
<REQ-VECTOR>
<VARIABLE order=\"1\">ipsrc</VARIABLE>
<VARIABLE order=\"2\">ipdest</VARIABLE>
<VARIABLE order=\"3\">portsrc</VARIABLE>
<VARIABLE order=\"4\">portdest</VARIABLE>
<VARIABLE order=\"5\">pro</VARIABLE>
</REQ-VECTOR>
<AXIOMS>

<DISJOINT>

<SORT name=\"blacklistaddr\">

<SORT name=\"ftpserver\"/>

</SORT>

<SORT name=\"webserver\">

<SORT name=\"blacklistaddr\"/>

<SORT name=\"ftpserver\"/>

</SORT>

<SORT name=\"udp\">

<SORT name=\"tcp\"/>

</SORT>

<SORT name=\"port21\">

<SORT name=\"port80\"/>

</SORT>

</DISJOINT>

</AXIOMS>
</VOCABULARY>
</MARGRAVE-RESPONSE>")))

(define test-error-response
  (read-xml (open-input-string "<MARGRAVE-RESPONSE type=\"error\">

<ERROR subtype=\"a subtype\" type=\"a type\">This is an error</ERROR>

</MARGRAVE-RESPONSE>")))

(define test-exception
  (read-xml (open-input-string "<MARGRAVE-RESPONSE type=\"exception\">

<EXCEPTION class=\"edu.wpi.margrave.MSemanticException\" stack-trace=\"[edu.wpi.margrave.MCommunicator.validateDBIdentifier(MCommunicator.java:1161), edu.wpi.margrave.MCommunicator.exploreHelper(MCommunicator.java:971), edu.wpi.margrave.MCommunicator.xmlHelper(MCommunicator.java:144), edu.wpi.margrave.MCommunicator.handleXMLCommand(MCommunicator.java:100), edu.wpi.margrave.MCommunicator.executeCommand(MCommunicator.java:1051), edu.wpi.margrave.MCommunicator.readCommands(MCommunicator.java:1026), edu.wpi.margrave.MCommunicator.main(MCommunicator.java:74)]\">

<MESSAGE>Margrave could not understand...</MESSAGE>

<LOCATION problem=\"Unknown IDB Collection: firewall1\"/>

<COMMAND/>

</EXCEPTION>

</MARGRAVE-RESPONSE>")))

(define (string-contains? str phrase)
  (cond [(< (string-length str) (string-length phrase)) false]
        [else (or (equal? (substring str 0 (string-length phrase)) phrase)
                  (string-contains? (substring str 1) phrase))]))

;To run this: (run-tests pretty-print-tests)
(define pretty-print-tests
    (test-suite
     "Pretty Printing tests"
  
     (check-true (string-contains? (pretty-print-response-xml test-model) "firewall1:accept is true for: [ipsrc, ipdest, portsrc, portdest, pro]"))
     (check-true (string-contains? (pretty-print-response-xml test-model) "Computed max size: 1"))
     (check-true (string-contains? (pretty-print-response-xml test-sys-info) "Type: sysinfo"))
     (check-true (string-contains? (pretty-print-response-xml test-sys-info) "Init: 49729280"))
     (check-true (string-contains? (pretty-print-response-xml test-coll-info) "Collection Info:"))
     (check-true (string-contains? (pretty-print-response-xml test-coll-info) "Policy Name: conf1"))
     (check-true (string-contains? (pretty-print-response-xml test-vocab-info) "Vocabulary Info:"))
     (check-true (string-contains? (pretty-print-response-xml test-vocab-info) "Vocabulary Name: examplefw1"))
     (check-true (string-contains? (pretty-print-response-xml test-vocab-info) "DISJOINT"))
     
     (check-true (string-contains? (pretty-print-response-xml test-error-response) "Error:\nType: a type\nSubtype: a subtype\nThis is an error"))
     (check-true (string-contains? (pretty-print-response-xml test-exception) "Exception:\nClass: edu.wpi.margrave.MSemanticException\nStack Trace: [edu.wpi.margrave.MCommunicator.validateDBIdentifier(MCommunicator.java:1161), edu.wpi.margrave.MCommunicator.exploreHelper(MCommunicator.java:971), edu.wpi.margrave.MCommunicator.xmlHelper(MCommunicator.java:144), edu.wpi.margrave.MCommunicator.handleXMLCommand(MCommunicator.java:100), edu.wpi.margrave.MCommunicator.executeCommand(MCommunicator.java:1051), edu.wpi.margrave.MCommunicator.readCommands(MCommunicator.java:1026), edu.wpi.margrave.MCommunicator.main(MCommunicator.java:74)]\nMessage: Margrave could not understand...\nLocation of Problem: Unknown IDB Collection: firewall1\n"))
     (check-true (string-contains? (pretty-print-response-xml low-user-ceiling) "Warning: User max ceiling"))
     (check-true (string-contains? (pretty-print-response-xml negative-ceiling) "Warning: Unable to calculate sufficient ceiling size."))))

(display "MODEL: \n")
(pretty-print-response-xml test-model)
(display "\n\n\tSYSINFO: \n")
(pretty-print-response-xml test-sys-info)
(display "\n\n\tCOLLECTION INFO: \n")
(pretty-print-response-xml test-coll-info)
(display "\n\n\tVOCAB INFO: \n")
(pretty-print-response-xml test-vocab-info)
(display "\n\n\tError: \n")
(pretty-print-response-xml test-error-response)
(display "\n\n\tException: \n")
(pretty-print-response-xml test-exception)
(display "\n\n\tLow User Ceiling:: \n")
(pretty-print-response-xml low-user-ceiling)
(display "\n\n\tNegative Ceiling: \n")
(pretty-print-response-xml negative-ceiling)
