#lang margrave

/* Note that this check DOES NOT CONSIDER NAT!
 * As the paper says, Margrave does not support stateful
 * dynamic NAT, only models the *outgoing* packets.
 * Here we fix the ephemeral port issue, which comes up before
 * NAT has a chance to apply. */

// Load original and modified policy
// WITH prepends and appends to the policy names automatically.
// So the first line yields InboundACL1, LocalSwitching1, etc.

LOAD IOS "config/config.txt" WITH "" "1";
LOAD IOS "config-revised/config-revised.txt" WITH "" "2";
    
// The IOS parser doesn't use constants. Constants need to
// denote in every scenario, which means they are equivalent 
// to "one" constrained unary predicates. Here, we use "lone"
// constrained unary predicates to allow un-needed IPs, ports,
// etc. to just not appear (for both efficiency and clarity).

////////////////////////////////////////////////////////////
// Change impact: added entry for packets with *SRC* port 80
////////////////////////////////////////////////////////////

let returning-changes[hostname : Hostname,
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

                                               
NOT ip-192.168.2.0/255.255.255.0(src-addr-in) AND
FastEthernet0(entry-interface) AND
prot-TCP(protocol) AND
port-80(src-port-in) AND

//passes-firewall1(<policyvector>) AND
// not passes-firewall2(<policyvector>)
internal-result1(hostname, theentry, sa, da, sp, dp, protocol, paf,
                 exit, sa2, da2, sp2, dp2);                 
  
// This degree of granularity for internal-result doesn't expose the next hop, 
// or the internal NAT addresses. Perhaps we should provide another "full" version.

show returning
  include InboundACL1:permit(hostname, entry, sa, da, sp, dp, protocol),
          InboundACL2:permit(hostname, entry, sa, da, sp, dp, protocol);


  
  