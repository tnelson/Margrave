    #lang racket
(require ;(file "./read.rkt")
         "margrave.rkt"
         "margrave-xml.rkt"
         "margrave-policy-vocab.rkt"
         xml)

(start-margrave-engine)

(load-policy (string-append "./" "generatedconfig.p"))


(mtext "rename GeneratedConfigPolicy p")(mtext "EXPLORE (NOT FS~5(c)) AND 
 (NOT FS~3(c)) AND 
 (Tomcat~1(c) IMPLIES (Ping(c) OR WWW(c))) AND 
 (Ping~1(c) IMPLIES Net(c) AND (FS~2(c) OR FS~1(c))) AND 
 (Mail~1(c) IMPLIES Net(c) AND (FS~5(c) OR FS~4(c) OR FS~3(c) OR FS~2(c))) AND 
 (WWW~1(c) IMPLIES Net(c) AND (FS~5(c) OR FS~4(c) OR FS~3(c) OR FS~1(c)) AND Net~1(c)) AND 
 (Net~1(c) IMPLIES (Mail~1(c) OR Ping~1(c) OR Connection(c)))
UNDER p
PUBLISH c
")


(mtext "COUNT")


(stop-margrave-engine)