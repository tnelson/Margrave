#lang racket

(require margrave)

(start-margrave-engine #:margrave-params '("-log"))

(m-load-policy "continue" "continuep.p")

(m-let "Q1" '([s User]
              [a Action]
              [r Object])
       '([continue permit] s a r))
(m-get "Q1")



;#lang margrave
;
;LOAD POLICY continuep.p;
;EXPLORE continuep:permit(s,a,r);
;SHOW ONE;
;
;
;// For type inference testing (doesn't work yet)
;EXPLORE authorOf(x, y) under continuep;
;is possible?;  
;// should be true, is false
;EXPLORE authorOf(x, y) under continuep ceiling 4;
;is possible?;




