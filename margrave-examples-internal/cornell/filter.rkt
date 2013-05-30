#lang margrave

LOAD POLICY filter = "filter.p";

///////////////////////////////////////////////////
// What WWW traffic is passed by the filter?
let q1[sa: IPAddress, da: IPAddress, sp: Port, dp: Port] be 
  //not ip10-1-1-1(sa) and ip10-1-1-x(sa)
  //under filter;
  
  filter.permit(sa, sp, da, dp) and port80(dp);
  
show q1;
show q1;


///////////////////////////////////////////////////
// Is any of that traffic allowed by an unexpected rule?
let q2[sa: IPAddress, da: IPAddress, sp: Port, dp: Port] be 
  q1(sa, da, sp, dp) and 
  not ( filter.rule3_applies(sa, sp, da, dp) or 
        filter.rule4_applies(sa, sp, da, dp));
                                             
show q2;                                             
 
  
///////////////////////////////////////////////////
// What addresses can initiate web requests?
// (could do this with rule names, too ala filter.rule1_applies(sa, sp, da, dp))
show realized q1 ip10-1-1-1(sa), ip10-1-1-2(sa), ip10-1-20-20(sa);

///////////////////////////////////////////////////
// What rules NEVER fire? (Indicate bugs, bad design, or good engineering!)
// careful to not limit to just permit:

let q3[sa: IPAddress, da: IPAddress, sp: Port, dp: Port] be 
  true
  under filter;

show unrealized q3
  filter.rule1_applies(sa, sp, da, dp),
  filter.rule2_applies(sa, sp, da, dp),
  filter.rule3_applies(sa, sp, da, dp),
  filter.rule4_applies(sa, sp, da, dp),
  filter.rule7_applies(sa, sp, da, dp);

///////////////////////////////////////////////////
// Why does this rule never fire? 
// (that is, what rules contribute to it being overshadowed?)
let q4[sa: IPAddress, da: IPAddress, sp: Port, dp: Port] be 
  filter.rule7_matches(sa, sp, da, dp);

show realized q4 
  filter.rule1_applies(sa, sp, da, dp),
  filter.rule2_applies(sa, sp, da, dp),
  filter.rule3_applies(sa, sp, da, dp),
  filter.rule4_applies(sa, sp, da, dp);


