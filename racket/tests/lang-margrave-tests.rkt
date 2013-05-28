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

// EXPLORE + UNDER + AND + NOT
let Q1[s: Subject, a: Action, r: Resource] be assigned(s, r) AND NOT assigned(s, r) 
UNDER conf1;

info conf1;
info Q1;

// more than one UNDER
let Q2[s: Subject, a: Action, r: Resource] be assigned(s, r) AND NOT assigned(s, r) 
UNDER conf1, conf2;

// IS POSSIBLE? + SHOW ONE + SHOW ALL
// expect: unsat
IS POSS? Q1;
SHOW Q1;
SHOW ALL Q1;

// IMPLIES
// and ISA 
// and ISA sugar
let Q3[s: Subject, a: Action, r: Resource] be 
  conf1.permit(s, a, r) AND
  ((ISA s: Subject (true) AND ISA s: Subject ) IMPLIES NOT Subject(s));

// expect: unsat
IS POSS? Q3;

// IFF
let Q4[s: Subject, a: Action, r: Resource] be 
conf1.permit(s, a, r) IFF NOT conf1.permit(s, a, r);

// expect: unsat
IS POSS? Q4;

INFO Q4;

// basic request vector sugar
//explore conf1:permit(<conf1:req>);
// publish and vector in publish
//explore conf1:permit(<conf1:req>) PUBLISH <conf1:req>;
// last ID, info saved query
//info last;

COUNT Q4;


/////////////////////////////////////////////////////////
// Testing show realized: create a query that matches everything:
let Q5[s: Subject, a: Action, r: Resource] be 
true
under conf1;

SHOW Q5;

SHOW REALIZED Q5 conf1.permit(s, a, r);
SHOW REALIZED Q5 conf1.permit(s, a, r), conf1.deny(s, a, r);

SHOW REALIZED Q5 conf1.permit(s, a, r), conf1.deny(s, a, r)
  FOR CASES conf1.papernoconflict_applies(s,a,r),
            conf1.paperassigned_applies(s,a,r),
            conf1.paperconflict_applies(s,a,r);

// FOR CASES


// Testing show unrealized: 
let Q6[s: Subject, a: Action, r: Resource] be conf1.permit(s,a,r);

// TODO: not yet connected in implementation. is just complement of realized?
// do the sugar in java.

SHOW UNREALIZED Q6 conf1.permit(s, a, r), conf1.deny(s, a, r);

/////////////////////////////////////////////////////////
