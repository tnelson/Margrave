#lang margrave

// Two routers on the network. One does NAT and filtering.
// The other just does filtering.
LOAD POLICY aclfw1 = "inboundacl_fw1.p";
LOAD POLICY aclfw2 = "inboundacl_fw2.p";
LOAD POLICY natfw2 = "inboundnat_fw2.p";

// A first fix!
LOAD POLICY aclfw1fix = "inboundacl_fw1_new.p";

// The COMPARE command sets up a change-impact query over a SINGLE POLICY.
// Without giving a variable vector, it will name the variables v0, ...
COMPARE diff = aclfw1 aclfw1fix (interf, ipsrc, ipdest, portsrc, portdest, pro);


// todo more. link with compare

///////////////////////////////////////////////////
// *************************************************
// We changed FW1's ACL to try to fix the bug. Let's see
// how that changes the behavior of outgoing packets!
// (Note: we did not change FW2. So we only consider changes to FW1.)
// Look for unexpected consequences (outside manager's PC)
// *************************************************
let q1[ipsrc: IPAddress, ipdest: IPAddress,
       portsrc: Port, portdest: Port, pro: Protocol, 
       tempnatsrc: IPAddress] be 

// Packet isn't from manager's PC.
not $managerpc = ipsrc and

// Internal FW passes the packet and translates its source to tempnatsrc.                                                 
aclfw2.accept($fw2int, ipsrc, ipdest, portsrc, portdest, pro) and
natfw2.translate($fw2int, ipsrc, ipdest, portsrc, portdest, pro, tempnatsrc) and
  
// Gain or loss of access vs. new ACL
((not aclfw1.accept($fw1dmz, tempnatsrc, ipdest, portsrc, portdest, pro) and 
  aclfw1fix.accept($fw1dmz, tempnatsrc, ipdest, portsrc, portdest, pro)) 
  OR
 (aclfw1.accept($fw1dmz, tempnatsrc, ipdest, portsrc, portdest, pro) and 
  not aclfw1fix.accept($fw1dmz, tempnatsrc, ipdest, portsrc, portdest, pro)));

SHOW q1;
                                                                           
///////////////////////////////////////////////////
// Change-impact has possibly led us to another property. Before we just had
// "The manager must be able to access to web."
// but now (depending on organizational goals) we may have:
// "Non-managers cannot access the web."

// The second fix:
LOAD POLICY aclfw2fix = "inboundacl_fw2_new.p";

///////////////////////////////////////////////////
// Do the two changes pass both properties? 

let nonmanager[ipsrc: IPAddress, ipdest: IPAddress,
            portsrc: Port, 
            tempnatsrc: IPAddress] be 

	// someone other than the manager, accessing an external website         
	 OutsideIPs(ipdest) and                            
         not $managerpc=ipsrc and
	// is allowed through
         aclfw2fix.accept($fw2int, ipsrc, ipdest, portsrc, $port80, $tcp) and
         natfw2.translate($fw2int, ipsrc, ipdest, portsrc, $port80, $tcp, tempnatsrc) and
         aclfw1fix.accept($fw1dmz, tempnatsrc, ipdest, portsrc, $port80, $tcp);

let managerblocked[ipsrc: IPAddress, ipdest: IPAddress,
            portsrc: Port, 
            tempnatsrc: IPAddress] be 

	// the manager, accessing an external website       
	 OutsideIPs(ipdest) and                            
         $managerpc=ipsrc and
	// is blocked. either by:
   	(
          // denial at internal FW2 ACL
          aclfw2fix.deny($fw2int, ipsrc, ipdest, portsrc, $port80, $tcp)           
	or
          // denial at external FW1 ACL after NAT          
           (natfw2.translate($fw2int, ipsrc, ipdest, portsrc, $port80, $tcp, tempnatsrc) and
           aclfw1fix.deny($fw1dmz, tempnatsrc, ipdest, portsrc, $port80, $tcp))
	);

