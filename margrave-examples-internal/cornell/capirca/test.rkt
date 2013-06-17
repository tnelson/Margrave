#lang margrave

load policy cisco = "sample_cisco_lab.p";
load policy jun = "sample_juniper_loopback.p";
load policy multi = "sample_multitarget.p";
load policy filter = "sample_packetfilter.p";
load policy speedway = "sample_speedway.p";
load policy srx = "sample_srx.p";

// A cross-policy diff:

COMPARE diff = cisco jun (sa, sp, da, dp, pro, opt);

show diff include cisco:accept(sa, sp, da, dp, pro, opt), cisco:deny(sa, sp, da, dp, pro, opt),
                  jun:accept(sa, sp, da, dp, pro, opt), jun:deny(sa, sp, da, dp, pro, opt),

                  // All the cisco rules. (I don't have sugar for "include all rules" at the moment.)
                  cisco:accept-to-honestdns_applies(sa, sp, da, dp, pro, opt),
                  cisco:accept-tcp-replies_applies(sa, sp, da, dp, pro, opt),
                  cisco:deny-to-internal_applies(sa, sp, da, dp, pro, opt),
                  cisco:deny-to-specific_hosts_applies(sa, sp, da, dp, pro, opt),
                  cisco:default-permit_applies(sa, sp, da, dp, pro, opt);
                  
// aggregate: which cisco rules ever apply in a change-impact scenario?
// aggregation over large sets of rules will speed up when i get back to a real keyboard...
show realized diff 
cisco:accept-to-honestdns_applies(sa, sp, da, dp, pro, opt),
cisco:accept-tcp-replies_applies(sa, sp, da, dp, pro, opt),
cisco:deny-to-internal_applies(sa, sp, da, dp, pro, opt),
cisco:deny-to-specific_hosts_applies(sa, sp, da, dp, pro, opt),
cisco:default-permit_applies(sa, sp, da, dp, pro, opt);
                  
//////////////////////////////////////////////////////////////////
// capirca adds logging to terms as a side-effect. margrave breaks out logging
// as a duplicate rule, and gives a separate "log" decision, which can be used like this:

// can SRX ever accept a udp packet without logging?
let logcheck[sa: IPAddress, sp: IPAddress, da: Port, dp: Port, pro: Protocol, opt: Options] be 
  srx:accept(sa, sp, da, dp, pro, opt) and not srx:log(sa, sp, da, dp, pro, opt) and
  udp(pro);

poss? logcheck;