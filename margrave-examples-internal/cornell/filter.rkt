#lang margrave

LOAD POLICY filter = "filter.p";

// What WWW traffic is passed by the filter?
let q1[sa: IPAddress, da: IPAddress, sp: Port, dp: Port] be 
  //not ip10-1-1-1(sa) and ip10-1-1-x(sa)
  //under filter;
  
  filter.permit(sa, sp, da, dp) and port80(dp);
  
show q1;
show q1;

// which of these rules can ever apply?
show realized q1 
  filter.rule1_applies(sa, sp, da, dp), 
  filter.rule2_applies(sa, sp, da, dp),
  filter.rule3_applies(sa, sp, da, dp),
  filter.rule4_applies(sa, sp, da, dp),
  filter.rule7_applies(sa, sp, da, dp);



show realized q1 ip10-1-1-1(sa), ip10-1-1-2(sa), ip10-1-20-20(sa);