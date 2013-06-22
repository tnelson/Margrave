#lang margrave

// Tests of Margrave commands.

// TODO
// invoke Java tests (needs to be from Racket)

// ***************************************
// Initial basic tests for syntax coverage
// ***************************************

// Remember that Margrave is case-sensitive!

info;

//info doesntexist;

// LOAD POLICY (no-path, *MARGRAVE*, relative path. with and without quotes)
#load policy conf1 = "conference1.p";
#LOAD POLICY conf2 = "*MARGRAVE*/tests/conference2.p";
//#load policy emptyconf = "./emptyconference.p";

let qtrue[s: Subject, a: Action, r: Resource] be true
UNDER conf1;

let qfalse[s: Subject, a: Action, r: Resource] be false
UNDER conf1;

// EXPLORE + UNDER + AND + NOT + constant
let q1[s: Subject, a: Action, r: Resource] be assigned(s, r) AND NOT assigned(s, r) 
OR NOT ($margravepaper = $margravepaper)
UNDER conf1;

info conf1;
info q1;

// more than one UNDER
let q2[s: Subject, a: Action, r: Resource] be assigned(s, r) AND NOT assigned(s, r) 
UNDER conf1, conf2;

// IS POSSIBLE? + SHOW ONE + SHOW ALL
// expect: unsat
POSS? q1;
SHOW q1;
SHOW ALL q1;

// IMPLIES
// and ISA 
// and ISA sugar
let q3[s: Subject, a: Action, r: Resource] be 
  conf1:permit(s, a, r) AND
  ((ISA s: Subject (true) AND ISA s: Subject ) IMPLIES NOT Subject(s));

// expect: unsat
POSS? q3;

// IFF
let q4[s: Subject, a: Action, r: Resource] be 
conf1:permit(s, a, r) IFF NOT conf1:permit(s, a, r);

// expect: unsat
POSS? q4;
INFO q4;

// basic request vector sugar
//explore conf1:permit(<conf1:req>);
// publish and vector in publish
//explore conf1:permit(<conf1:req>) PUBLISH <conf1:req>;
// last ID, info saved query
//info last;

COUNT q4;


/////////////////////////////////////////////////////////
// Testing show realized: create a query that matches everything:
// (also test include, but remember it's a SHOW option now!
let q5[s: Subject, a: Action, r: Resource] be Paper(r) and ReadPaper(a)
under conf1;

SHOW q5 include conf1:permit(s,a,r), conf1:deny(s,a,r), 
            conf1:papernoconflict_applies(s,a,r),
            conf1:paperassigned_applies(s,a,r),
            conf1:paperconflict_applies(s,a,r);

SHOW REALIZED q5 conf1:permit(s, a, r);
SHOW REALIZED q5 conf1:permit(s, a, r), conf1:deny(s, a, r);

SHOW REALIZED q5 conf1:permit(s, a, r), conf1:deny(s, a, r)
  FOR CASES conf1:papernoconflict_applies(s,a,r),
            conf1:paperassigned_applies(s,a,r),
            conf1:paperconflict_applies(s,a,r);


// Testing show unrealized.
// Also testing that substitution of variables is occuring for Show realized.
let q6[x: Subject, y: Action, z: Resource] be conf1:permit(x,y,z) ;


SHOW UNREALIZED q6 conf1:permit(x,y,z), conf1:deny(x,y,z);



//// vvv check this well.
// yes, UNREALIZED on case=deny, where the query requires permit, 
// has deny as an unrealized.

SHOW UNREALIZED q6
              conf1:permit(x,y,z), 
              conf1:deny(x,y,z), 
              conf1:papernoconflict_applies(x,y,z),
              conf1:paperassigned_applies(x,y,z),
              conf1:paperconflict_applies(x,y,z)
            FOR CASES conf1:permit(x,y,z), 
                      conf1:deny(x,y,z);
                      
SHOW REALIZED q5
              conf1:deny(s, a, r),
              conf1:permit(s, a, r),               
              conf1:paperassigned_applies(s,a,r)
              FOR CASES conf1:permit(s, a, r), conf1:paperassigned_applies(s,a,r),
              conf1:deny(s,a,r), conf1:paperconflict_applies(s,a,r), conf1:papernoconflict_applies(s,a,r);
                                  

// Show realized for constant=variable
SHOW REALIZED q5 $margravepaper=r;

// show realized for edb predicate
// latter has a type mismatch, so doesn't appear in result
SHOW REALIZED q6 assigned(x, z), conflicted(x, y);

/////////////////////////////////////////////////////////

// Test p4p policies in #lang margrave

#load policy p4p1 = "p4p-policy.p";


/////////////////////////////////////////////////////////

// Test that Margrave implicitly binds:
let q7[] be conf1:permit(s,a,r);

let q8[] be q7() and not q7();

// And that there are no variables shown for s/a/r.
show q7;

show q8;

let q9[] be conflicted(s,r) under conf1;

let q10[x: Subject] be conf1:deny(x, y, z) and conflicted(x,z);

//let q9[] be conf1:permit(s,a,r) and conf1:permit(r,a,s);

// needs testing in interactions, too.
count q9; count q10;


/////////////////////
// error cases: TODO test cases!
/////////////////////
// let qfa[sa: Subject] be false under conf1;
// let foox[s:] be conflicted(s,r) under conf1;
// let all[] be conf1:permit(s,a,r);


