;ip access-list extended less-secure
;remark allow all hosts on 10 net to access web server
;permit tcp 10.1.1.0 0.0.0.255 host 192.168.1.200 eq www
;permit tcp 10.1.1.0 0.0.0.255 host 192.168.1.200 eq 443
;
;remark allow RESPONSE to secure PC.s requets for access to web, ftp control,
;data, smtp, and pop3 on 10 net
;permit tcp 10.1.1.0 0.0.0.255 eq www host 192.168.1.201
;permit tcp 10.1.1.0 0.0.0.255 eq ftp host 192.168.1.201
;permit tcp 10.1.1.0 0.0.0.255 eq ftp-data host 192.168.1.201
;permit tcp 10.1.1.0 0.0.0.255 eq smtp host 192.168.1.201
;permit tcp 10.1.1.0 0.0.0.255 eq pop3 host 192.168.1.201

(Policy HappyRouterLess uses happyrouteracl
	(Target )

	(Rules

         ; All hosts in external net can access the web server on appropriate port
         (Rule1 = (Accept ipsrc ipdest portsrc portdest pro) :- (10network ipsrc) (webserver ipdest) (http portdest) (tcp pro))
         (Rule2 = (Accept ipsrc ipdest portsrc portdest pro) :- (10network ipsrc) (webserver ipdest) (https portdest) (tcp pro))
         
         ; Allow responses to PC client from external network (from approved ports)
         (Rule3 = (Accept ipsrc ipdest portsrc portdest pro) :- (10network ipsrc) (pc ipdest) (http portsrc) (tcp pro))
         (Rule4 = (Accept ipsrc ipdest portsrc portdest pro) :- (10network ipsrc) (pc ipdest) (ftp portsrc) (tcp pro))
         (Rule5 = (Accept ipsrc ipdest portsrc portdest pro) :- (10network ipsrc) (pc ipdest) (ftpdata portsrc) (tcp pro))
         (Rule6 = (Accept ipsrc ipdest portsrc portdest pro) :- (10network ipsrc) (pc ipdest) (smtp portsrc) (tcp pro))
         (Rule7 = (Accept ipsrc ipdest portsrc portdest pro) :- (10network ipsrc) (pc ipdest) (pop3 portsrc) (tcp pro)) 

         (RuleX = (Drop ipsrc ipdest portsrc portdest pro) :- true)
                )
        
	(RComb FAC)
	(PComb FAC)

	(Children ))

