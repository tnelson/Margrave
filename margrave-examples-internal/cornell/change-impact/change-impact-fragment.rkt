#lang margrave

// Two routers on the network. One does NAT and filtering.
// The other just does filtering.
LOAD POLICY aclfw1 = "inboundacl_fw1.p";
LOAD POLICY aclfw2 = "inboundacl_fw2.p";
LOAD POLICY natfw2 = "inboundnat_fw2.p";

// A first fix!
LOAD POLICY aclfw1fix = "inboundacl_fw1_new.p";

// The COMPARE command sets up a change-impact query between two single policies
// Without giving a variable vector, it will name the variables v0, ...
COMPARE diff = aclfw1 aclfw1fix (interf, sa, da, sp, dp, pro);

// the query "diff" now describes scenarios where the old and new acl for FW1 disagree:

show diff;


// But this doesn't take NAT into account! Moreover, the scenario involves the 
// manager's PC. Let's get a bit more involved:

///////////////////////////////////////////////////
// *************************************************
// We changed FW1's ACL to try to fix the bug. Let's see
// how that changes the behavior of outgoing packets!
// (Note: we did not change FW2. So we only consider changes to FW1.)
// *************************************************
let fullci[sa: IPAddress, da: IPAddress,
           sp: Port, dp: Port, pro: Protocol, 
           tempnatsrc: IPAddress] be 

  // topology: packet passes filter and NAT translates sa to tempnatsrc. 
  aclfw2:permit($fw2int, sa, da, sp, dp, pro) and
  natfw2:translate($fw2int, sa, da, sp, dp, pro, tempnatsrc) and
  // look for differences between original and fixed versions of FW1
  diff($fw1dmz, tempnatsrc, da, sp, dp, pro) and
  // limit to internal source addresses and external destinations
  InternalIPs(sa) and OutsideIPs(da);
  
SHOW fullci;
        
// previous version doesn't indicate the decision change across
//  policies.  Use an include to show the permit/deny terms
SHOW fullci 
  INCLUDE aclfw1:permit($fw1dmz, tempnatsrc, da, sp, dp, pro),
          aclfw1:deny($fw1dmz, tempnatsrc, da, sp, dp, pro),
          aclfw1fix:permit($fw1dmz, tempnatsrc, da, sp, dp, pro),
          aclfw1fix:deny($fw1dmz, tempnatsrc, da, sp, dp, pro);

          
          
///////////////////////////////////////////////////
// Change-impact has possibly led us to another property. Before we just had
// "The manager must be able to access to web."
// but now (depending on organizational goals) we may have:
// "Non-managers cannot access the web."

// The second fix:
LOAD POLICY aclfw2fix = "inboundacl_fw2_new.p";

///////////////////////////////////////////////////
// Did the changes to fw2 affect the manager's access?
// ideally, the change should not

COMPARE diff2 = aclfw2 aclfw2fix (interf, sa, da, sp, dp, pro);

let fw2ci[sa: IPAddress, da: IPAddress,
          sp: Port, dp: Port, pro: Protocol] be
  diff($fw2int, sa, da, sp, dp, pro) and
  $managerpc=sa;
                                            
poss? fw2ci;

///////////////////////////////////////////////////
// Do the two changes pass both properties? 
// One query for each property:

let nonmanager[sa: IPAddress, da: IPAddress,
               sp: Port, tempnatsrc: IPAddress] be 

  // someone other than the manager, accessing an external website         
  OutsideIPs(da) and                            
  not $managerpc=sa and
  // is allowed through
  aclfw2fix:permit($fw2int, sa, da, sp, $port80, $tcp) and
  natfw2:translate($fw2int, sa, da, sp, $port80, $tcp, tempnatsrc) and
  aclfw1fix:permit($fw1dmz, tempnatsrc, da, sp, $port80, $tcp);

let managerblocked[sa: IPAddress, da: IPAddress,
                   sp: Port, tempnatsrc: IPAddress] be 

  // the manager, accessing an external website       
  OutsideIPs(da) and                            
  $managerpc=sa and
  // is blocked. either by:
  (
   // denial at internal FW2 ACL
   aclfw2fix:deny($fw2int, sa, da, sp, $port80, $tcp)           
   or
   // denial at external FW1 ACL after NAT          
   (natfw2:translate($fw2int, sa, da, sp, $port80, $tcp, tempnatsrc) and
    aclfw1fix:deny($fw1dmz, tempnatsrc, da, sp, $port80, $tcp))
  );

poss? nonmanager;
poss? managerblocked;

         
