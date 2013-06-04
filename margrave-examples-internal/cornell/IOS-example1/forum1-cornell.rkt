#lang margrave

/* Note that this check DOES NOT CONSIDER NAT!
 * As the paper says, Margrave does not reason about the
 * return packages in stateful dynamic NAT;
 * it only models the *outgoing* packets.
 * Here we investigate the ephemeral port issue, which 
 * comes up before NAT has a chance to apply to return packets. */

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

// The vector is 13ary to deal with NAT. Without NAT, we'd be at 9.

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

// Coming from outside (with an appropriate address)
FastEthernet0(entry) AND
NOT ip-192.168.2.0/255.255.255.0(sa) AND

// TCP packet with source port = 80 (i.e., returning from a webserver)
prot-TCP(protocol) AND
port-80(sp) AND

// NATted to <sa2, da2, sp2, dp2> and switched to interface exit.
internal-result1(hostname, entry, sa, da, sp, dp, protocol, paf,
                 exit, sa2, da2, sp2, dp2) AND

// Change-impact over the ACLs: 
passes-firewall1(hostname, entry, sa, da, sp, dp, protocol, paf,
                 exit, sa2, da2, sp2, dp2) AND
NOT passes-firewall2(hostname, entry, sa, da, sp, dp, protocol, paf,
                 exit, sa2, da2, sp2, dp2);                 

///////////////////////////////////////////////////////////
// Show change-impact scenarios, but also include info about
// the policies in the decomposition:
show returning
  include InboundACL1:permit(hostname, entry, sa, da, sp, dp, protocol),
          InboundACL2:permit(hostname, entry, sa, da, sp, dp, protocol);

          
///////////////////////////////////////////////////////////
// !!! TN note to self (remove before final version):  
// This degree of granularity for internal-result doesn't expose the next hop, 
// or the internal NAT addresses. Perhaps we should provide another "full" version.

  
  