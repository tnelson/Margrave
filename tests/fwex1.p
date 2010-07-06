; Example 1: A small firewall policy. No state, and traffic in only one direction.
; (In a Cisco Firewall, there is one policy per interface per direction. This example
;  simulates the inbound interface coming into the firewall itself.)
; Uses our intermediate language, but would not be difficult to translate IOS ACLs.

(Policy FWEx1 uses examplefw1
        ; No target (not a hierarchical policy.)
	(Target )

	(Rules

        ; Blacklist: We have one IP Address that we want to prohibit at all times.
         (RuleBL = (Drop ipsrc ipdest portsrc portdest pro) :- (blacklistaddr ipsrc))

        ; These 4 rules enforce the idea that the policy is for a single traffic path only.
        ; (Some also protect against spoofing: For instance, if a packet arrives at the inbound
        ; interface claiming to be from an *internal* source, drop it.)
	(RuleOneDirection1 = (Drop ipsrc ipdest portsrc portdest pro) :- (webserver ipsrc))
        (RuleOneDirection2 = (Drop ipsrc ipdest portsrc portdest pro) :- (ftpserver ipsrc))
        (RuleOneDirection3 = (Drop ipsrc ipdest portsrc portdest pro) :- (blacklistaddr ipdest))

        ; Permit all incoming traffic to connect to port 80 on the web server.
         (RuleWWW = (Accept ipsrc ipdest portsrc portdest pro) :- (webserver ipdest) (port80 portdest) (TCP pro))

        ; Permit all incoming traffic to connect to port 21 on the FTP server.
         (RuleFTP = (Accept ipsrc ipdest portsrc portdest pro) :- (ftpserver ipdest) (port21 portdest) (TCP pro))
         
         ; Catch-all Drop rule
         (RuleX = (Drop ipsrc ipdest portsrc portdest pro) :- true)
                )
        
        ; Tells Margrave how to combine the rules. Firewalls are First-Applicable.        
	(RComb FAC)
	(PComb FAC)

        ; Not a hierarchical policy.
	(Children ))

