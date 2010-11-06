#lang margrave

// Full request vector plus the intermediate NAT vars 
// that are bound by internal-result.
DEFVEC <reqvector> ahostname, entry-interface, 
        src-addr-in, src-addr_, src-addr-out, 
        dest-addr-in, dest-addr_, dest-addr-out, 
        protocol, message, flags,
        src-port-in, src-port_, src-port-out, 
        dest-port-in, dest-port_, dest-port-out, 
        length, next-hop, exit-interface;

// Policy request vector (no intermediate NAT vars)
DEFVEC <policyvector> ahostname, entry-interface, 
        src-addr-in,  src-addr-out, 
        dest-addr-in,  dest-addr-out, 
        protocol, message, flags,
        src-port-in,  src-port-out, 
        dest-port-in, dest-port-out, 
        length, next-hop, exit-interface;

// Src-addr <---> dest-addr
// Src-port <---> dest-port
// entry-interface <---> exit-interface
// Used for reflexive ACL axiom

// Note that this check DOES NOT CONSIDER NAT!
// As the paper says, Margrave does not support stateful
// dynamic NAT, only models the *outgoing* packets.
// Here we fix the ephemeral port issue, which comes up before
// NAT has a chance to apply.

DEFVEC <reversepolicyvector> ahostname, exit-interface, 
        dest-addr-in,  dest-addr-out,         
        src-addr-in,  src-addr-out,  
        protocol, message, flags,
        dest-port-in, dest-port-out,         
        src-port-in,  src-port-out,         
        length, next-hop, entry-interface;

 
// Load all the policies 
// InboundACL -> InboundACL1, InboundACL2, InboundACL3 respectively.
LOAD IOS "config-ack/config.txt" WITH "" "ack";
LOAD IOS "config/config.txt" WITH "" "1";
LOAD IOS "config-revised/config-revised.txt" WITH "" "2";
LOAD IOS "config-reflexive/config.txt" WITH "" "3";
  
  //;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  // Version 1
  //;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
 
  
EXPLORE
NOT src-addr-in IN 192.168.2.0/255.255.255.0 AND
FastEthernet0 = entry-interface AND
prot-TCP = protocol AND
port-80 = src-port-in AND
passes-firewall1(<policyvector>) AND
internal-result1(<reqvector>)                 
TUPLING;
  
IS POSSIBLE?;
GET ONE;
  
SHOW REALIZED port-80 = dest-port-in,
port-20=dest-port-in, 
port-21=dest-port-in,
port-23 = dest-port-in,
port-3389=dest-port-in;
  
//^^^ Expected 80, 21, 20, 23 above.
  
  
  
EXPLORE
NOT src-addr-in IN 192.168.2.0/255.255.255.0 AND
FastEthernet0=entry-interface AND
prot-TCP=protocol AND
port-80=src-port-in AND
passes-firewall1(<policyvector>) AND
internal-result1(<reqvector>) AND
NOT port-80=dest-port-in AND
NOT port-20=dest-port-in AND
NOT port-21=dest-port-in AND
NOT port-23=dest-port-in AND
NOT port-3389=dest-port-in
TUPLING;
  
IS POSSIBLE?;
  
// ^^^ Expected false above.
 
  
  
  //;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  // Version 2
  //;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  
EXPLORE
NOT src-addr-in IN 192.168.2.0/255.255.255.0 AND
FastEthernet0=entry-interface AND
prot-TCP=protocol AND
port-80=src-port-in AND
passes-firewall2(<policyvector>) AND
internal-result2(<reqvector>) AND
 NOT port-80=dest-port-in AND
 NOT port-20=dest-port-in AND
 NOT port-21=dest-port-in AND
 NOT port-23=dest-port-in AND
 NOT port-3389=dest-port-in
TUPLING;
  
IS POSSIBLE?;
  
// ^^^ Expected true above.
  
  //;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  // Version 3
  //;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
EXPLORE
NOT src-addr-in IN 192.168.2.0/255.255.255.0 AND
FastEthernet0=entry-interface AND
prot-TCP=protocol AND
port-80=src-port-in AND
passes-firewall3(<policyvector>) AND
internal-result3(<reqvector>) AND
( Connection-returntcp(src-addr-in, src-port-in, protocol, dest-addr-in, dest-port-in) 
  IMPLIES
  InboundACL3:Router-Vlan1-line29_applies(<reversepolicyvector>))
                                                                    
AND NOT port-80=dest-port-in AND
NOT port-20=dest-port-in AND
NOT port-21=dest-port-in AND
NOT port-23=dest-port-in AND
NOT port-3389=dest-port-in                                                                 
TUPLING;
  
IS POSSIBLE?;
  
// ^^^ Expected true above.
  
GET ONE;

// Look for changes between original version and reflexive ACL version.
// Did any NOT involve the Connection predicate?

EXPLORE
// Reflexive ACL axiom (Vlan1-line29 is the "reflect returntcp" rule)
( Connection-returntcp(src-addr-in, src-port-in, protocol, dest-addr-in, dest-port-in) 
  IMPLIES
  InboundACL3:Router-Vlan1-line29_applies(<reversepolicyvector>)) 
AND 

// Change 
((InboundACL1:Permit(<policyvector>) AND NOT InboundACL3:Permit(<policyvector>)) 
OR
(InboundACL3:Permit(<policyvector>) AND NOT InboundACL1:Permit(<policyvector>)))

// but not because of the reflexive acl
AND NOT Connection-returntcp(src-addr-in, src-port-in, protocol, dest-addr-in, dest-port-in)                                                                                                                                      
TUPLING
INCLUDE InboundACL1:Permit(<policyvector>), InboundACL3:Permit(<policyvector>);

IS POSSIBLE?;

// There are still scenarios because in the original, there was no ACL attached to vlan1
// so everything was allowed. But in the reflexive ACL version, the permit-all rule
// applies to TCP *only*. Confirm:

EXPLORE
( Connection-returntcp(src-addr-in, src-port-in, protocol, dest-addr-in, dest-port-in) 
  IMPLIES
  InboundACL3:Router-Vlan1-line29_applies(<reversepolicyvector>)) 
AND 
((InboundACL1:Permit(<policyvector>) AND NOT InboundACL3:Permit(<policyvector>)) 
OR
(InboundACL3:Permit(<policyvector>) AND NOT InboundACL1:Permit(<policyvector>)))

// but not because of the reflexive acl
AND NOT Connection-returntcp(src-addr-in, src-port-in, protocol, dest-addr-in, dest-port-in)                                                                                                                                      
AND protocol=prot-tcp
TUPLING;

IS POSSIBLE?;

// Confirmed. 
// Can fix this by adding a catch-all permit ip after the reflexive tcp rule.