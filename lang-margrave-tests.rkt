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
load policy "tests/conference1.p";

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


// TODO: rest of syntax


// ***************************************
// Tests from old SISC suite
// ***************************************

LOAD POLICY "tests/emptyconference.p";
LOAD POLICY "tests/extconference.p";
LOAD POLICY "tests/conference1.p";
LOAD POLICY "tests/hospitaldenypayrollmedrecsfa.p";
rename hospitaldenypayrollmedrecs hospitalfa;
LOAD POLICY "tests/hospitaldenypayrollmedrecsdo.p";
rename hospitaldenypayrollmedrecs hospitaldo;
LOAD POLICY "tests/awfw.p";
LOAD POLICY "tests/bigfw.p";

EXPLORE BigFW:Accept(ipsrc, ipdest, portsrc, portdest);
IS POSSIBLE?;
// expect: true

// Empty policy never permits
EXPLORE EmptyConference:Permit(s, a, r);
IS POSSIBLE?;
// expect: false






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

//show populated 0 mypol:rule1, mypol:rule2 for cases mypol:rule1_applies, mypol:rule2_applies;


// test with tupling

//explore xsort(x)
//UNDER mypol
//include mypol:rule1(x), mypol:rule2(x), mypol:rule1_applies(x), mypol:rule2_applies(x)
//tupling;

//show populated 0 mypol:rule1(x), mypol:rule2(x) for cases mypol:rule1_applies(x), mypol:rule2_applies(x);





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


//explore xsort(x) and xsort(y)
//UNDER mypol
//include mypol:rule1(x, y), mypol:rule2(x, y), mypol:rule1_applies(x, y), mypol:rule2_applies(x, y)
//tupling;

//show populated 0 mypol:rule1(x, y), mypol:rule2(x, y) for cases mypol:rule1_applies(x, y), mypol:rule2_applies(x, y);

//show populated 0 mypol:rule1(y, x), mypol:rule2(x, y) for cases mypol:rule1_applies(x, y), mypol:rule2_applies(x, y);
//show populated 0 mypol:rule1(x, y), mypol:rule2(x, y) for cases mypol:rule1_applies(y, x), mypol:rule2_applies(x, y);

//show unpopulated 0 mypol:rule1(x, y), mypol:rule2(x, y) for cases mypol:rule1_applies(x, y), mypol:rule2_applies(x, y);

//show unpopulated 0 mypol:rule1(y, x), mypol:rule2(x, y) for cases mypol:rule1_applies(x, y), mypol:rule2_applies(x, y);
//show unpopulated 0 mypol:rule1(x, y), mypol:rule2(x, y) for cases mypol:rule1_applies(y, x), mypol:rule2_applies(x, y);


