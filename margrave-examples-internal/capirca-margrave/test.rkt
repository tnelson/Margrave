#lang margrave

// This is a Margrave script to demonstrate a few (not all!) ways that it can
// analyze capirca-produced ACLs. 

// Note the below policy file references are relative to the current dir.
load policy cisco = "capirca-r242-MODIFIED/filters/sample_cisco_lab.p";
load policy jun = "capirca-r242-MODIFIED/filters/sample_juniper_loopback.p";
load policy multi = "capirca-r242-MODIFIED/filters/sample_multitarget.p";
load policy filter = "capirca-r242-MODIFIED/filters/sample_packetfilter.p";
load policy speedway = "capirca-r242-MODIFIED/filters/sample_speedway.p";
load policy srx = "capirca-r242-MODIFIED/filters/sample_srx.p";

//////////////////////////////////////////
// Margrave can take the semantic diff of policies. 
// Here is an example:

COMPARE diff = cisco jun (sa, sp, da, dp, pro, opt);

// We expect there to be scenarios where they differ, since they are plainly different examples!
// Show a scenario for the query named diff:

show diff include cisco:accept(sa, sp, da, dp, pro, opt), cisco:deny(sa, sp, da, dp, pro, opt),
                  jun:accept(sa, sp, da, dp, pro, opt), jun:deny(sa, sp, da, dp, pro, opt),
                  
                  // Say WHY the policies differed (i.e., which of the rules applied?)
                  // We also have keywords for matching but being over-ruled (not shown).
                  // All the cisco rules. (I don't have sugar for "include all rules" at the moment.)
                  cisco:accept-to-honestdns_applies(sa, sp, da, dp, pro, opt),
                  cisco:accept-tcp-replies_applies(sa, sp, da, dp, pro, opt),
                  cisco:deny-to-internal_applies(sa, sp, da, dp, pro, opt),
                  cisco:deny-to-specific_hosts_applies(sa, sp, da, dp, pro, opt),
                  cisco:default-permit_applies(sa, sp, da, dp, pro, opt);

////////////////////////////////
// aggregate the differences: 
// which cisco rules ever apply in a change-impact scenario?
// same-ish syntax as INCLUDE
// (Yeah, we need syntactic sugar for "check all rules' applicability" ;-)) 
show realized diff 
cisco:accept-to-honestdns_applies(sa, sp, da, dp, pro, opt),
cisco:accept-tcp-replies_applies(sa, sp, da, dp, pro, opt),
cisco:deny-to-internal_applies(sa, sp, da, dp, pro, opt),
cisco:deny-to-specific_hosts_applies(sa, sp, da, dp, pro, opt),
cisco:default-permit_applies(sa, sp, da, dp, pro, opt);
                  
//////////////////////////////////////////////////////////////////
// Lots more can be done, but here is a second example showing *decisions*.
// capirca adds logging to terms as a side-effect. the capirca->margrave parser
// breaks out logging as a duplicate rule, and gives a separate "log" decision,
// which can be used like this:

// can SRX ever accept a udp packet without logging?
let logcheck[sa: IPAddress, sp: IPAddress, da: Port, dp: Port, pro: Protocol, opt: Options] be 
  srx:accept(sa, sp, da, dp, pro, opt) and not srx:log(sa, sp, da, dp, pro, opt) and
  udp(pro);

// false expected
poss? logcheck;