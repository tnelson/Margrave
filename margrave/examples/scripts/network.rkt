#lang margrave

// Vectors for the queries in this file

DEFVEC <reqfull-1> fw1, fw1-entry-interface, 
        fw1-src-addr-in, fw1-src-addr_, fw1-src-addr-out, 
        fw1-dest-addr-in, fw1-dest-addr_, fw1-dest-addr-out, 
        protocol, message, flags,
        fw1-src-port-in, fw1-src-port_, fw1-src-port-out, 
        fw1-dest-port-in, fw1-dest-port_, fw1-dest-port-out, 
        length, fw1-next-hop, fw1-exit-interface;

DEFVEC <reqfull-2> fw2, fw2-entry-interface, 
        fw1-src-addr-out, fw2-src-addr_, fw2-src-addr-out, 
        fw1-dest-addr-out, fw2-dest-addr_, fw2-dest-addr-out, 
        protocol, message, flags,
        fw1-src-port-out, fw2-src-port_, fw2-src-port-out, 
        fw1-dest-port-out, fw2-dest-port_, fw2-dest-port-out, 
        length, fw2-next-hop, fw2-exit-interface;

DEFVEC <reqpol-1> fw1, fw1-entry-interface, 
        fw1-src-addr-in, fw1-src-addr-out, 
        fw1-dest-addr-in, fw1-dest-addr-out, 
        protocol, message, flags,
        fw1-src-port-in, fw1-src-port-out, 
        fw1-dest-port-in, fw1-dest-port-out, 
        length, fw1-next-hop, fw1-exit-interface;

DEFVEC <reqpol-2> fw2, fw2-entry-interface, 
        fw1-src-addr-out, fw2-src-addr-out, 
        fw1-dest-addr-out, fw2-dest-addr-out, 
        protocol, message, flags,
        fw1-src-port-out, fw2-src-port-out, 
        fw1-dest-port-out, fw2-dest-port-out, 
        length, fw2-next-hop, fw2-exit-interface;


// for testing

DEFVEC <nat1> fw1, fw1-entry-interface, 
        fw1-src-addr-in, fw1-src-addr-out, 
        fw1-dest-addr-in, fw1-dest-addr-out, 
        protocol, message, flags,
        fw1-src-port-in, fw1-src-port-out, 
        fw1-dest-port-in, fw1-dest-port-out, 
        length, fw1-next-hop, fw1-exit-interface;

DEFVEC <nat2> fw1, fw1-entry-interface, 
        fw1-src-addr-out, fw1-src-addr-out, 
        fw1-dest-addr-out, fw1-dest-addr-out, 
        protocol, message, flags,
        fw1-src-port-out, fw1-src-port-out, 
        fw1-dest-port-out, fw1-dest-port-out, 
        length, fw1-next-hop, fw1-exit-interface;

DEFVEC <route1> fw1, fw1-entry-interface, 
        fw1-src-addr-out, fw1-src-addr-out, 
        fw1-dest-addr-out, fw1-dest-addr-out, 
        protocol, message, flags,
        fw1-src-port-out, fw1-src-port-out, 
        fw1-dest-port-out, fw1-dest-port-out, 
        length, fw1-next-hop, fw1-exit-interface;

LOAD IOS "intern.txt", "extern.txt" IN "*margrave*/examples/policies/network";
  
// Remember: AND binds tighter than OR, so wrap the OR in parens.
// 10.200.0.0/255.255.0.0 is "the internet" for this example: something outside the extern router
  
EXPLORE prot-TCP = protocol AND
192.168.1.2 = fw1-src-addr-in  AND
in_lan = fw1-entry-interface AND
out_dmz = fw2-entry-interface AND
hostname-int = fw1 AND
hostname-ext = fw2 AND

fw1-dest-addr-in IN 10.200.0.0/255.255.0.0 AND
NOT 10.200.200.200 = fw1-dest-addr-in AND
port-80 = fw1-dest-port-in AND

internal-result(<reqfull-1>) AND
( NOT passes-firewall(<reqpol-1>) OR

internal-result(<reqfull-2>) AND
NOT passes-firewall(<reqpol-2>))

UNDER InboundACL
INCLUDE
InboundACL:int-in_lan-line12_applies(<reqpol-1>),
InboundACL:int-in_lan-line15_applies(<reqpol-1>),
InboundACL:ext-out_dmz-line17_applies(<reqpol-2>),
InboundACL:ext-out_dmz-line18_applies(<reqpol-2>),
InboundACL:ext-out_dmz-line20_applies(<reqpol-2>)
TUPLING;

  
  
GET ONE;
  
SHOW REALIZED 
InboundACL:int-in_lan-line12_applies(<reqpol-1>),
InboundACL:int-in_lan-line15_applies(<reqpol-1>),
InboundACL:ext-out_dmz-line17_applies(<reqpol-2>),
InboundACL:ext-out_dmz-line18_applies(<reqpol-2>),
InboundACL:ext-out_dmz-line20_applies(<reqpol-2>);
  
 