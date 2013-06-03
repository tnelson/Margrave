#lang margrave

// Note that this check DOES NOT CONSIDER NAT!
// As the paper says, Margrave does not support stateful
// dynamic NAT, only models the *outgoing* packets.
// Here we fix the ephemeral port issue, which comes up before
// NAT has a chance to apply.

// Load original and modified policy
LOAD IOS "config/config.txt" WITH "" "1";
LOAD IOS "config-revised/config-revised.txt" WITH "" "2";
  
//;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
// Version 1
//;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   
let returning-changes[hostname : Hostname,
                      entry-interface : Interface,
                      src-addr-in : IPAddress,
                      dest-addr-in : IPAddress,
                      src-port-in : Port,  
                      dest-port-in : Port, 
                      protocol : Protocol-any,
		      payload : Payload,
                      flags : Flags,
		// what of next-hop? might want to reveal? hmm.
		      exit-interface : Interface
                      src-addr-out : IPAddress,
                      dest-addr-out : IPAddress,
                      src-port-out : Port,  
                      dest-port-out : Port] be

                                               
NOT ip-192.168.2.0/255.255.255.0(src-addr-in) AND
FastEthernet0(entry-interface) AND
prot-TCP(protocol) AND
port-80(src-port-in) AND
InboundACL1:permit(hostname,    entry-interface,    src-addr-in,    dest-addr-in,    src-port-in,    dest-port-in,    protocol);

//passes-firewall1(<policyvector>) AND
// not passes-firewall2(<policyvector>)
//internal-result1(<reqvector>)                 
  
show returning;

  
  