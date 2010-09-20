
#lang margrave

// Load all the policies.
// These are three different versions of the same configuration. The policies are loaded 
// with the suffixes 1,2, and 3 respectively.

// aha. how to get margrave-home-path in there?
// use margrave-home-path ALWAYS as relative path?
// that would make sense

LOAD IOS "../policies/ios-demo/initial/demo.txt" WITH "" "1";
LOAD IOS "../policies/ios-demo/change1/change1.txt" WITH "" "2";
LOAD IOS "../policies/ios-demo/change2/change2.txt" WITH "" "3";

//////////////////////////////////////////////
// example: which-packets
//////////////////////////////////////////////

EXPLORE InboundACL1:Permit(<InboundACL1:req>) TUPLING;
// The TUPLING keyword activates the tupling optimization, which is very useful for firewalls.

SHOW ONE;

// (pause-for-user)

//////////////////////////////////////////////
// example: verification
//////////////////////////////////////////////

EXPLORE InboundACL1:Permit(<InboundACL1:req>)
        AND src-addr-in = 10.1.1.2 
        AND fe0 = entry-interface
     TUPLING;
     
IS POSSIBLE?;

//(pause-for-user)

//////////////////////////////////////////////
// example: rule responsibility
//////////////////////////////////////////////

EXPLORE InboundACL1:Deny(<InboundACL1:req>)
AND 10.1.1.2 = src-addr-in
AND fe0 = entry-interface
                         
INCLUDE InboundACL1:Router-fe0-line9_applies(<InboundACL1:req>),
        InboundACL1:Router-fe0-line12_applies(<InboundACL1:req>)
TUPLING;

SHOW REALIZED InboundACL1:Router-fe0-line9_applies(<InboundACL1:req>),
              InboundACL1:Router-fe0-line12_applies(<InboundACL1:req>);

// (pause-for-user)

//////////////////////////////////////////////
// example: change-impact
//////////////////////////////////////////////

// vs change 1 
// (Differencing versions 1 and 2)

EXPLORE 
(InboundACL1:Permit(<InboundACL1:req>)
 AND NOT InboundACL2:Permit(<InboundACL1:req>))
OR
(InboundACL2:Permit(<InboundACL1:req>)
 AND NOT InboundACL1:Permit(<InboundACL1:req>))
TUPLING;
     
IS POSSIBLE?;

//Expect to see false

//(pause-for-user)

// Vs. change 2
//(Differencing versions 1 and 3)

EXPLORE 
(InboundACL1:Permit(<InboundACL1:req>)
 AND NOT InboundACL3:Permit(<InboundACL1:req>))
OR
(InboundACL3:Permit(<InboundACL1:req>)
 AND NOT InboundACL1:Permit(<InboundACL1:req>))
TUPLING;

IS POSSIBLE?;

// Expect to see true

// (pause-for-user)

//////////////////////////////////////////////
// example: Rule relationships
//////////////////////////////////////////////

// (printf "~n~nRule relationships:~n")

// This involves rules in the first change (version 2)
// line 12 wants to apply: what prevents it from doing so?

EXPLORE InboundACL2:Router-fe0-line12_matches(<InboundACL1:req>)
INCLUDE InboundACL2:Router-fe0-line9_applies(<InboundACL1:req>),
InboundACL2:Router-fe0-line10_applies(<InboundACL1:req>),
InboundACL2:Router-fe0-line11_applies(<InboundACL1:req>)
TUPLING;

SHOW REALIZED InboundACL2:Router-fe0-line9_applies(<InboundACL1:req>),
InboundACL2:Router-fe0-line10_applies(<InboundACL1:req>),
InboundACL2:Router-fe0-line11_applies(<InboundACL1:req>);


// Expect to see that line9 applies.



//////////////////////////////////////////////
