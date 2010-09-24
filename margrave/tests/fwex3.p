; Example 3: Added the Connection state to the language.
; This means a LOT of potentially accepted packets. Beware permissive queries.
; Uses our intermediate language, but would not be difficult to translate IOS ACLs.

(Policy FWEx3 uses examplefw3
        ; No target (not a hierarchical policy.)
	(Target )

	(Rules

	; Pay attention to the firewall state table: Is there a preexisting connection?
	; Check only for the symmetric "return" traffic here: 1, 3, 5... will always be 
        ; Allowed by the ACL below.
        (RuleSt2 = (Accept ipsrc ipdest portsrc portdest pro) :- (Connection ipdest ipsrc portdest portsrc pro))

        (RuleBL = (Drop ipsrc ipdest portsrc portdest pro) :- (blacklistaddr ipsrc))

        (RuleAS1 = (Drop ipsrc ipdest portsrc portdest pro) :- (webserver ipsrc))
        (RuleAS2 = (Drop ipsrc ipdest portsrc portdest pro) :- (ftpserver ipsrc))
        (RuleAS3 = (Drop ipsrc ipdest portsrc portdest pro) :- (blacklistaddr ipdest))



        ; Permit all incoming traffic to connect to port 80 on the web server.
         (RuleWWW = (Accept ipsrc ipdest portsrc portdest pro) :- (webserver ipdest) (port80 portdest) (TCP pro))

        ; Permit all incoming traffic to connect to port 25 on the Mail server.
         (RuleMail = (Accept ipsrc ipdest portsrc portdest pro) :- (mailserver ipdest) (port25 portdest) (TCP pro))

        ; Rule to permit incoming traffic to ftp server
         (RuleFTP = (Accept ipsrc ipdest portsrc portdest pro) :- (ftpserver ipdest) (port21 portdest) (TCP pro))
         
         ; Catch-all Drop rule
         (RuleX = (Drop ipsrc ipdest portsrc portdest pro) :- true)
                )
        
        ; Tells Margrave how to combine the rules. Firewalls are First-Applicable.        
	(RComb FAC)
	(PComb FAC)

        ; Not a hierarchical policy.
	(Children ))

