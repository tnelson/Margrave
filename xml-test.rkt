#lang racket
(require "margrave-xml.rkt"
         xml)

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
<DISJOINT/>
</AXIOMS>
</VOCABULARY>
</MARGRAVE-RESPONSE>")))

(display "MODEL: \n")
(pretty-print-model (document-element test-model))
(display "\n\nSYSINFO: \n")
(pretty-print-info-xml (document-element test-sys-info))
(display "\n\n COLLECTION INFO: \n")
(pretty-print-info-xml (document-element test-coll-info))
(display "\n\n VOCAB INFO: \n")
(pretty-print-info-xml (document-element test-vocab-info))
