; Load the API 
(newline)
(display "-> Loading Margrave library ... ... ...") (newline)
(load "margrave.scm")
(display "-> margrave.scm loaded. ") (newline)
(newline)
(newline)

(define pPolicy (load-policy (string-append my-directory "sampleconfig.p")))

(define-policy "p" pPolicy)

(m "EXPLORE WWW(c) AND Configuration(c) AND p:valid(c)")

(pause-for-user)

(m "EXPLORE Mail(c) AND p:valid(c)")

(pause-for-user)

(m "EXPLORE Tomcat(c) AND p:valid(c)")

