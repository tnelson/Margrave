#lang margrave

// Tests of Margrave commands.

// TODO
// The Margrave script language doesn't allow testing the _results_
// So once inlining in Racket is finished, move these tests inline
// and confirm they return the correct results

// TODO
// invoke Java tests (needs to be from Racket)

// TODO: import the rest of the old SISC tests



// ***************************************
// Initial basic tests for syntax coverage
// ***************************************

info;

//info doesntexist;

// LOAD POLICY (no-path, *MARGRAVE*, relative path. with and without quotes)
load policy "conference1.p";
LOAD POLICY "*MARGRAVE*/tests/conference2.p";
load policy ./emptyconference.p;


// RENAME
rename conferencepolicy1 conf1;

// EXPLORE + UNDER + AND + NOT
explore assigned(s, r) AND NOT assigned(s, r) UNDER conf1;

// more than one UNDER
explore assigned(s, r) AND NOT assigned(s, r) 
UNDER conf1, conferencepolicy2;

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

load policy "*MARGRAVE*/tests/extconference.p";
load policy "*MARGRAVE*/tests/conference1.p";
load policy "*MARGRAVE*/tests/hospitaldenypayrollmedrecsfa.p";
rename hospitaldenypayrollmedrecs hospitalfa;
load policy "*MARGRAVE*/tests/hospitaldenypayrollmedrecsdo.p";
rename hospitaldenypayrollmedrecs hospitaldo;
load policy "*MARGRAVE*/tests/awfw.p";
load policy "*MARGRAVE*/tests/bigfw.p";
load policy "*MARGRAVE*/tests/phone1.p";

EXPLORE BigFW:Accept(ipsrc, ipdest, portsrc, portdest);
IS POSSIBLE?;
// expect: true

// Empty policy never permits
EXPLORE EmptyConference:Permit(s, a, r);
IS POSSIBLE?;
// expect: false

// *******************************************************************
// *******************************************************************

// Use the happy router "more" secure side policy for realized testing
load policy "*MARGRAVE*/tests/happyroutermore.p";


// Tests for REALIZED without tupling
// Much slower without tupling
// IMPORTANT: Note below rulex_applies -> rule1_matches, which seems
// nonsensical. But without tupling, realized can be read as populated.
// other packets exist populating rule1_matches...

// DISABLED 10/11/10 by TN
//  since we are changing the syntax of SHOW REALIZED w/o tupling
//  to match the tupled case


//explore happyroutermore:accept(<happyroutermorex:req>)

//explore happyroutermore:accept(<happyroutermore:req>) AND 
//  tcp=pro AND ipsrc=webserver and portdest=portsrc and portdest=http
//include happyroutermore:rule1_matches,
//        happyroutermore:ruleX_matches,
//        happyroutermore:rule1_applies,
//        happyroutermore:ruleX_applies;

//SHOW ONE;        
        
//SHOW REALIZED happyroutermore:rule1_matches,        
//        happyroutermore:ruleX_matches;

//SHOW UNREALIZED happyroutermore:rule1_matches,
//        happyroutermore:ruleX_matches; 
        
//SHOW REALIZED happyroutermore:rule1_matches,
//        happyroutermore:ruleX_matches
//FOR CASES happyroutermore:rule1_applies,
//        happyroutermore:ruleX_applies;                     
        
//SHOW UNREALIZED happyroutermore:rule1_matches,
//        happyroutermore:ruleX_matches
//FOR CASES happyroutermore:rule1_applies,
//        happyroutermore:ruleX_applies;       



// *******************************************************************

// Tests for REALIZED with tupling


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


// *** TUPLING ONLY
// Test: EDB in tupled INCLUDE ---> forces the EDB to be included if it doesn't appear in the query
EXPLORE happyroutermore:rulex_applies(<happyroutermore:req>) DEBUG 3
INCLUDE happyroutermore:rulex_applies(<happyroutermore:req>), udp(pro)
TUPLING;
SHOW ONE;


// *** TUPLING ONLY
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
        
// *******************************************************************
// *******************************************************************

// Test COMPARE keyword
// and comparing >2-decision policies

load policy "*MARGRAVE*/tests/phone2.p";

COMPARE phone1 phone2;
COMPARE phone1 phone2 CEILING 9;
INFO LAST; // should be the compare
IS POSSIBLE?;
EXPLORE last(src, dest) and outofservice(dest) or number(extravar) 
PUBLISH src, dest, extravar CEILING 9;
INFO LAST;
IS POSSIBLE?; // true. and should see ceiling 11, not ceiling 9 (extravar and its exchange)
SHOW ONE;

// duplicate what compare does
EXPLORE 
(Phone1:TollFree(ncaller, nreceive)
 AND NOT Phone2:TollFree(ncaller, nreceive)) 
OR
(Phone2:TollFree(ncaller, nreceive)
 AND NOT Phone1:TollFree(ncaller, nreceive)) 
OR
(Phone1:Toll(ncaller, nreceive)
 AND NOT Phone2:Toll(ncaller, nreceive)) 
OR
(Phone2:Toll(ncaller, nreceive)
 AND NOT Phone1:Toll(ncaller, nreceive)) 
OR
(Phone1:Refuse(ncaller, nreceive)
 AND NOT Phone2:Refuse(ncaller, nreceive)) 
OR
(Phone2:Refuse(ncaller, nreceive)
 AND NOT Phone1:Refuse(ncaller, nreceive)) CEILING 9;        
IS POSSIBLE?;

// IOS loading

LOAD IOS "../examples/policies/ios-demo/initial/demo.txt" WITH "" "1";
LOAD IOS "*MARGRAVE*/examples/policies/ios-demo/change1/change1.txt" WITH "" "2";
// identical to demo.txt above, but in the local directory to test lack of path in the filename
LOAD IOS ios-local-file-load.txt WITH "" "3";