; Example 1: A small firewall policy. No state, and traffic in only one direction.
; Uses our intermediate language, but would not be difficult to translate IOS ACLs.

(Policy FWEx1a uses examplefw1
        ; No target (not a hierarchical policy.)
	(Target )

	(Rules

        ; Blacklist: We have one IP Address that we want to prohibit at all times.
         (RuleBL = (Drop ipsrc ipdest portsrc portdest pro) :- (blacklistaddr ipsrc))

        ; Prohibit spoofing
        (RuleAS1 = (Drop ipsrc ipdest portsrc portdest pro) :- (webserver ipsrc))
        (RuleAS2 = (Drop ipsrc ipdest portsrc portdest pro) :- (ftpserver ipsrc))
        (RuleAS3 = (Drop ipsrc ipdest portsrc portdest pro) :- (blacklistaddr ipdest))

        ; Permit all incoming traffic to connect to port 80 on the web server.
         (RuleWWW = (Accept ipsrc ipdest portsrc portdest pro) :- (webserver ipdest) (port80 portdest) (TCP pro))

        ; Permit all incoming traffic to connect to port 21 on the FTP server.
        ; Commented out to demonstrate differencing.
        ; (RuleFTP = (Accept ipsrc ipdest portsrc portdest pro) :- (ftpserver ipdest) (port21 portdest) (TCP pro))
         
         ; Catch-all Drop rule
         (RuleX = (Drop ipsrc ipdest portsrc portdest pro) :- (IPAddress ipsrc))
                )
        
        ; Tells Margrave how to combine the rules. Firewalls are First-Applicable.        
	(RComb FAC)
	(PComb FAC)

        ; Not a hierarchical policy.
	(Children ))

