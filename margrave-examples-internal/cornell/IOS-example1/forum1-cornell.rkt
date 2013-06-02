#lang margrave

// Full request vector plus the intermediate NAT vars 
// that are bound by internal-result.
//DEFVEC <reqvector> ahostname, entry-interface, 
  //      src-addr-in, src-addr_, src-addr-out, 
    //    dest-addr-in, dest-addr_, dest-addr-out, 
      //  protocol, message, flags,
      //  src-port-in, src-port_, src-port-out, 
       // dest-port-in, dest-port_, dest-port-out, 
        //length, next-hop, exit-interface;

// Policy request vector (no intermediate NAT vars)
//DEFVEC <policyvector> ahostname, entry-interface, 
//        src-addr-in,  src-addr-out, 
//        dest-addr-in,  dest-addr-out, 
 //       protocol, message, flags,
  //      src-port-in,  src-port-out, 
  //      dest-port-in, dest-port-out, 
    //    length, next-hop, exit-interface;

// Src-addr <---> dest-addr
// Src-port <---> dest-port
// entry-interface <---> exit-interface
// Used for reflexive ACL axiom

// Note that this check DOES NOT CONSIDER NAT!
// As the paper says, Margrave does not support stateful
// dynamic NAT, only models the *outgoing* packets.
// Here we fix the ephemeral port issue, which comes up before
// NAT has a chance to apply.

//DEFVEC <reversepolicyvector> ahostname, exit-interface, 
//       dest-addr-in,  dest-addr-out,         
 //       src-addr-in,  src-addr-out,  
  //      protocol, message, flags,
   //     dest-port-in, dest-port-out,         
    //    src-port-in,  src-port-out,         
     //   length, next-hop, entry-interface;

 
// Load all the policies 
// InboundACL -> InboundACL1, InboundACL2, InboundACL3 respectively.
LOAD IOS "config-ack/config.txt" WITH "" "ack";
LOAD IOS "config/config.txt" WITH "" "1";
LOAD IOS "config-revised/config-revised.txt" WITH "" "2";
LOAD IOS "config-reflexive/config.txt" WITH "" "3";
  
  //;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  // Version 1
  //;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
 
let returning[hostname : Hostname,
              entry-interface : Interface,
              src-addr-in : IPAddress,
              dest-addr-in : IPAddress,
              src-port-in : Port,  
              dest-port-in : Port, 
              protocol : Protocol-any] be

NOT ip-192.168.2.0/255.255.255.0(src-addr-in) AND
FastEthernet0(entry-interface) AND
prot-TCP(protocol) AND
port-80(src-port-in) AND
InboundACL1:permit(hostname,    entry-interface,    src-addr-in,    dest-addr-in,    src-port-in,    dest-port-in,    protocol);

//passes-firewall1(<policyvector>) AND
//internal-result1(<reqvector>)                 
  
show returning;


//SHOW REALIZED port-80 = dest-port-in,
//port-20=dest-port-in, 
//port-21=dest-port-in,
//port-23 = dest-port-in,
//port-3389=dest-port-in;
  
//^^^ Expected 80, 21, 20, 23 above.
  
  
  