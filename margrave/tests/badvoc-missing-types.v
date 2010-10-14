(PolicyVocab badvoc-missing-types
             ; missing types clause
             (Decisions 
              TollFree
              Toll
              Refuse)
             (Predicates
              (GetExchange : Number Exchange))

	     (ReqVariables (src : Number)
                           (dest : Number))
             (OthVariables (e1 : Exchange) (e2 : Exchange) (e3 : Exchange))
             (Constraints

	      ; There is a unique exchange for each number.
              (total-function GetExchange)
         
              ; A number is either in or out of service, never both.
	      (disjoint-all Number)

              (abstract Exchange)
              (abstract Number)

	      ; Weed out vacuous solutions
              (nonempty Number)
              (nonempty Exchange)))
