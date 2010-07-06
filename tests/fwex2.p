; Example 2: A small firewall policy. No state, and traffic in only one direction.
; Additions from Example 2: *Set* of IP Addresses (dmzset, containing 3 servers and possibly more)
; Uses our intermediate language, but would not be difficult to translate IOS ACLs.

(Policy FWEx2 uses examplefw2
        ; No target (not a hierarchical policy.)
	(Target )

	(Rules

        (RuleBL = (Drop ipsrc ipdest portsrc portdest pro) :- (blacklistaddr ipsrc))

        (RuleAS1 = (Drop ipsrc ipdest portsrc portdest pro) :- (webserver ipsrc))
        (RuleAS2 = (Drop ipsrc ipdest portsrc portdest pro) :- (ftpserver ipsrc))
        (RuleAS3 = (Drop ipsrc ipdest portsrc portdest pro) :- (blacklistaddr ipdest))



        ; Permit all incoming traffic to connect to port 80 on the web server.
         (RuleWWW = (Accept ipsrc ipdest portsrc portdest pro) :- (webserver ipdest) (port80 portdest) (TCP pro))

        ; Permit all incoming traffic to connect to port 25 on the Mail server.
         (RuleMail = (Accept ipsrc ipdest portsrc portdest pro) :- (mailserver ipdest) (port25 portdest) (TCP pro))

        ; Deny external access to everything else in the dmz 
         (RuleNotDMZ = (Drop ipsrc ipdest portsrc portdest pro) :- (dmz ipdest))

        ; Rule to permit incoming traffic to ftp server: Badly positioned.
        ; This example is contrived: The point is to illustrate that MG does rule combination for you.
         (RuleFTP = (Accept ipsrc ipdest portsrc portdest pro) :- (ftpserver ipdest) (port21 portdest) (TCP pro))
         
         ; Catch-all Drop rule
         (RuleX = (Drop ipsrc ipdest portsrc portdest pro) :- true)
                )
        
        ; Tells Margrave how to combine the rules. Firewalls are First-Applicable.        
	(RComb FAC)
	(PComb FAC)

        ; Not a hierarchical policy.
	(Children ))

