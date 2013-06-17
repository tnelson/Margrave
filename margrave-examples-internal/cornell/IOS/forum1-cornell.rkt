#lang margrave

// WITH prepends and appends to the policy names automatically.
// So the first line yields InboundACL1, LocalSwitching1, etc.

LOAD IOS "config/config.txt" WITH "" "1";
    
// The IOS parser doesn't use constants. Constants need to
// denote in every scenario, which means they are equivalent 
// to "one" constrained unary predicates. Here, we use "lone"
// constrained unary predicates to allow un-needed IPs, ports,
// etc. to just not appear (for both efficiency and clarity).

////////////////////////////////////////////////////////////
// Looking for packets that come from outside and are routed
// inside, as well as passing all necessary ACLs.
////////////////////////////////////////////////////////////

let returning[hostname : Hostname,
              entry : Interface,
              sa : IPAddress,
              da : IPAddress,
              sp : Port,  
              dp : Port, 
              protocol : Protocol-any,		      
              paf : PayloadAndFlags,  	
	      exit : Interface,
              sa2 : IPAddress,
              da2 : IPAddress,
              sp2 : Port,  
              dp2 : Port] be
// Coming from outside (with an appropriate address)
fastEthernet0(entry) AND
vlan1(exit) AND
NOT ip-192.168.2.0/255.255.255.0(sa) AND
// TCP packet with source port = 80 (i.e., returning from a webserver)
prot-TCP(protocol) AND
port-80(sp) AND
// internal-result binds exit interface, etc.
internal-result1(hostname, entry, sa, da, sp, dp, protocol, paf,
                exit, sa2, da2, sp2, dp2) AND
// NATted to <sa2, da2, sp2, dp2> and switched to interface exit.
// passes-firewall needs both pre- and post-nat since OUTGOING ACL
// (if any) uses post-nat addresses.
passes-firewall1(hostname, entry, sa, da, sp, dp, protocol, paf,
                exit, sa2, da2, sp2, dp2);

// Which concrete ports on inside machines can receive traffic through the firewall? 
SHOW REALIZED returning 
  port-80(dp),
  port-20(dp), 
  port-21(dp),
  port-23(dp),
  port-3389(dp);

//////////////////////////////////////////////////////  
// Can still refer to sub-policies. E.g.,
// "Are any of *those* packets locally switched?
let returning-locally[hostname : Hostname,
                      entry : Interface,
                      sa : IPAddress,
                      da : IPAddress,
                      sp : Port,  
                      dp : Port, 
                      protocol : Protocol-any,		      
                      paf : PayloadAndFlags,  	
		      exit : Interface,
                      sa2 : IPAddress,
                      da2 : IPAddress,
                      sp2 : Port,  
                      dp2 : Port] be
returning(hostname, entry, sa, da, sp, dp, protocol, paf,
                exit, sa2, da2, sp2, dp2) and
LocalSwitching1:forward(hostname, da2, exit);

show returning-locally;
