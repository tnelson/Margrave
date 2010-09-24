#lang margrave

// Tests of Margrave commands.

// TODO
// The Margrave script language doesn't allow testing the _results_
// So once inlining in Racket is finished, move these tests inline
// and confirm they return the correct results

// TODO
// invoke Java tests (needs to be from Racket)

// ***************************************
// Initial basic tests for syntax coverage
// ***************************************

info;

// LOAD POLICY
load policy "<MARGRAVE>/tests/conference1.p";

// RENAME
rename conferencepolicy1 conf1;

// EXPLORE + UNDER + AND + NOT
explore assigned(s, r) AND NOT assigned(s, r) UNDER conf1;

// IS POSSIBLE? + SHOW ONE + SHOW ALL
// expect: unsat
IS POSSIBLE?;
SHOW ONE;
SHOW ALL;

// IMPLIES
explore conf1:permit(s, a, r) AND
(subject(s) IMPLIES NOT subject(s));
// expect: unsat
IS POSSIBLE?;

// IFF
explore conf1:permit(s, a, r) AND
(subject(s) IFF NOT subject(s));
// expect: unsat
IS POSSIBLE?;

// basic request vector sugar
explore conf1:permit(<conf1:req>);

// publish and vector in publish
explore conf1:permit(<conf1:req>) PUBLISH <conf1:req>;

// last ID, info saved query
info last;

// Result accessor commands with and without explicit ID
IS POSSIBLE? 0;
SHOW ONE 0;
SHOW NEXT 0;
GET ONE 0;
GET NEXT 0;
GET ALL 0;
COUNT 0;

SHOW ONE;
SHOW NEXT;
GET ONE;
GET NEXT;
GET ALL;
COUNT;

// Accessing a policy's rule set
GET RULES IN conf1;
GET QUALIFIED RULES IN conf1;
GET RULES IN conf1 WITH DECISION permit;
GET QUALIFIED RULES IN conf1 WITH DECISION permit;



// TODO: realized (later in doc?)

// TODO: rest of syntax


// ***************************************
// Tests from old SISC suite
// ***************************************

load policy "<MARGRAVE>/tests/emptyconference.p";
load policy "<MARGRAVE>/tests/extconference.p";
load policy "<MARGRAVE>/tests/conference1.p";
load policy "<MARGRAVE>/tests/hospitaldenypayrollmedrecsfa.p";
rename hospitaldenypayrollmedrecs hospitalfa;
load policy "<MARGRAVE>/tests/hospitaldenypayrollmedrecsdo.p";
rename hospitaldenypayrollmedrecs hospitaldo;
load policy "<MARGRAVE>/tests/awfw.p";
load policy "<MARGRAVE>/tests/bigfw.p";
load policy "<MARGRAVE>/tests/phone1.p";

EXPLORE BigFW:Accept(ipsrc, ipdest, portsrc, portdest);
IS POSSIBLE?;
// expect: true

// Empty policy never permits
EXPLORE EmptyConference:Permit(s, a, r);
IS POSSIBLE?;
// expect: false


// TODO: import the rest of the old SISC tests


// ***************************************



//create vocabulary myvoc;
//add to myvoc sort xsort;
//add to myvoc subsort xsort s1;
//add to myvoc subsort xsort s2;
//add to myvoc decision permit;
//add to myvoc decision deny;
//add to myvoc requestvar x xsort;
//add to myvoc requestvar y xsort;

//create policy leaf mypol myvoc;
//add rule to mypol rule1 permit (s1 x) (s2 y);
//add rule to mypol rule2 deny (s2 x) (s1 y);
//prepare mypol;

// test without tupling

//explore xsort(x)
//UNDER mypol
//include mypol:rule1, mypol:rule2, mypol:rule1_applies, mypol:rule2_applies

//show realized 0 mypol:rule1, mypol:rule2 for cases mypol:rule1_applies, mypol:rule2_applies;


// test with tupling

//explore xsort(x)
//UNDER mypol
//include mypol:rule1(x), mypol:rule2(x), mypol:rule1_applies(x), mypol:rule2_applies(x)
//tupling;

//show realized 0 mypol:rule1(x), mypol:rule2(x) for cases mypol:rule1_applies(x), mypol:rule2_applies(x);





//  check >1 vector size


//create vocabulary myvoc;
//add to myvoc sort xsort;
//add to myvoc subsort xsort s1;
//add to myvoc subsort xsort s2;
//add to myvoc decision permit;
//add to myvoc decision deny;
//add to myvoc requestvar x xsort;
//add to myvoc requestvar y xsort;

//create policy leaf mypol myvoc;
//add rule to mypol rule1 permit (s1 x) (s2 y);
//add rule to mypol rule2 deny (s2 x) (s1 y);
//prepare mypol;


// *****************************

load policy "<MARGRAVE>/tests/happyroutermore.p";

// TUPLING, INCLUDE, SHOW REALIZED
// Need a policy with q-free IDBs to TUPLE (so can't use the phone policy)

explore ipaddress(ipsrc) and ipaddress(ipdest)
UNDER happyroutermore
include happyroutermore:rule1_matches(<happyroutermore:req>),
        happyroutermore:rule2_matches(<happyroutermore:req>),
        happyroutermore:rule3_matches(<happyroutermore:req>),
        happyroutermore:ruleX_matches(<happyroutermore:req>),
        happyroutermore:rule1_applies(<happyroutermore:req>),
        happyroutermore:rule2_applies(<happyroutermore:req>),
        happyroutermore:rule3_applies(<happyroutermore:req>),
        happyroutermore:ruleX_applies(<happyroutermore:req>)
TUPLING;


// Don't enable this: error when user forgets qualifier in INCLUDE
// But check for exn:fail:user, not null ptr
//explore ipaddress(ipsrc) and ipaddress(ipdest)
//UNDER happyroutermore
//include rule1_matches(<happyroutermore:req>),
//        rule2_matches(<happyroutermore:req>),
//        rule3_matches(<happyroutermore:req>),
//        ruleX_matches(<happyroutermore:req>)
//TUPLING;

SHOW REALIZED happyroutermore:rule1_matches(<happyroutermore:req>),
        happyroutermore:rule2_matches(<happyroutermore:req>),
        happyroutermore:rule3_matches(<happyroutermore:req>),
        happyroutermore:ruleX_matches(<happyroutermore:req>);

SHOW UNREALIZED happyroutermore:rule1_matches(<happyroutermore:req>),
        happyroutermore:rule2_matches(<happyroutermore:req>),
        happyroutermore:rule3_matches(<happyroutermore:req>),
        happyroutermore:ruleX_matches(<happyroutermore:req>);                
        
SHOW REALIZED happyroutermore:rule1_matches(<happyroutermore:req>),
        happyroutermore:rule2_matches(<happyroutermore:req>),
        happyroutermore:rule3_matches(<happyroutermore:req>),
        happyroutermore:ruleX_matches(<happyroutermore:req>)
FOR CASES happyroutermore:rule1_applies(<happyroutermore:req>),
        happyroutermore:rule2_applies(<happyroutermore:req>),
        happyroutermore:rule3_applies(<happyroutermore:req>),
        happyroutermore:ruleX_applies(<happyroutermore:req>);       
       
SHOW UNREALIZED happyroutermore:rule1_matches(<happyroutermore:req>),
        happyroutermore:rule2_matches(<happyroutermore:req>),
        happyroutermore:rule3_matches(<happyroutermore:req>),
        happyroutermore:ruleX_matches(<happyroutermore:req>)
FOR CASES happyroutermore:rule1_applies(<happyroutermore:req>),
        happyroutermore:rule2_applies(<happyroutermore:req>),
        happyroutermore:rule3_applies(<happyroutermore:req>),
        happyroutermore:ruleX_applies(<happyroutermore:req>);       

        
// Test: EDB in tupled INCLUDE ---> forces the EDB to be included if it doesn't appear in the query
EXPLORE happyroutermore:rulex_applies(<happyroutermore:req>) DEBUG 3
INCLUDE happyroutermore:rulex_applies(<happyroutermore:req>), udp(pro)
TUPLING;
SHOW ONE;



// Test SHOW REALIZED with non-standard variable ordering:
// matches reverse packet when normal pkt is accepted?
// rules 1, 2, 3 apply: dest 10net --> src 10net. only X can match.
// rule X applies: there is still some room available to have 10network(ipdest), so 1,2,3 can all match
explore ipaddress(ipsrc) and ipaddress(ipdest)
UNDER happyroutermore
include happyroutermore:rule1_matches(ipdest, ipsrc, portdest, portsrc, pro),
        happyroutermore:rule2_matches(ipdest, ipsrc, portdest, portsrc, pro),
        happyroutermore:rule3_matches(ipdest, ipsrc, portdest, portsrc, pro),
        happyroutermore:ruleX_matches(ipdest, ipsrc, portdest, portsrc, pro),
        happyroutermore:rule1_applies(<happyroutermore:req>),
        happyroutermore:rule2_applies(<happyroutermore:req>),
        happyroutermore:rule3_applies(<happyroutermore:req>),
        happyroutermore:ruleX_applies(<happyroutermore:req>)
TUPLING;

SHOW REALIZED happyroutermore:rule1_matches(ipdest, ipsrc, portdest, portsrc, pro),
        happyroutermore:rule2_matches(ipdest, ipsrc, portdest, portsrc, pro),
        happyroutermore:rule3_matches(ipdest, ipsrc, portdest, portsrc, pro),
        happyroutermore:ruleX_matches(ipdest, ipsrc, portdest, portsrc, pro)
FOR CASES happyroutermore:rule1_applies(<happyroutermore:req>),
        happyroutermore:rule2_applies(<happyroutermore:req>),
        happyroutermore:rule3_applies(<happyroutermore:req>),
        happyroutermore:ruleX_applies(<happyroutermore:req>);   
        
        
// TODO: test show realized WITHOUT tupling        