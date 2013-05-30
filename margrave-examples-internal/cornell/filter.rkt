#lang margrave

LOAD POLICY filter = "filter.p";

// Test that the policy loaded:

INFO filter;

let q1[sa: IPAddress, da: IPAddress, sp: Port, dp: Port] be 
  //not ip10-1-1-1(sa) and ip10-1-1-x(sa)
  //under filter;
  
  filter.permit(sa, sp, da, dp);
  
show q1;