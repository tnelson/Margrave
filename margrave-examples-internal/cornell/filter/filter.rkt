#lang margrave

LOAD POLICY filter = "filter.p";

///////////////////////////////////////////////////
// Basic query: What WWW traffic is passed by the filter?
let q1[sa: IPAddress, da: IPAddress, sp: Port, dp: Port] be 
  filter:permit(sa, sp, da, dp) and dp=$port80;

// Get a scenario 
show q1;

// Revise q1 to eliminate cases in which sa=da
let q1v2[sa: IPAddress, da: IPAddress, sp: Port, dp: Port] be 
  filter:permit(sa, sp, da, dp) and dp=$port80
  and not sa = da; // ignore silly examples

// Can see WHY permitted:
show q1v2 
  include filter:rule2_applies(sa, sp, da, dp),
          filter:rule4_applies(sa, sp, da, dp);


///////////////////////////////////////////////////
// Is any of that traffic allowed by an unexpected rule?
// (That is: a rule other than the two permit rules.)
let q2[sa: IPAddress, da: IPAddress, sp: Port, dp: Port] be 
  q1(sa, da, sp, dp) and 
  not ( filter:rule2_applies(sa, sp, da, dp) or 
        filter:rule4_applies(sa, sp, da, dp));
                                             
poss? q2;                                           

///////////////////////////////////////////////////
// What rules NEVER fire? (Indicate bugs, bad design, or good engineering!)
// careful to not limit to just permit: create a new query that's always true

let q3[sa: IPAddress, da: IPAddress, sp: Port, dp: Port] be 
  true
  under filter; // gives context when no policy references in body

show unrealized q3
  filter:rule1_applies(sa, sp, da, dp),
  filter:rule2_applies(sa, sp, da, dp),
  filter:rule3_applies(sa, sp, da, dp),
  filter:rule4_applies(sa, sp, da, dp),
  filter:rule5_applies(sa, sp, da, dp);

///////////////////////////////////////////////////
// Why does this rule never fire? 
// (that is, what rules contribute to it being overshadowed?)
let q4[sa: IPAddress, da: IPAddress, sp: Port, dp: Port] be 
  filter:rule4_matches(sa, sp, da, dp);

show realized q4 
  filter:rule1_applies(sa, sp, da, dp),
  filter:rule2_applies(sa, sp, da, dp),
  filter:rule3_applies(sa, sp, da, dp),
  filter:rule5_applies(sa, sp, da, dp);


