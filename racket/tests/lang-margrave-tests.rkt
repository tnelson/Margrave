#lang margrave

// Tests of Margrave commands.

// TODO
// invoke Java tests (needs to be from Racket)

// ***************************************
// Initial basic tests for syntax coverage
// ***************************************

info;

//info doesntexist;

// LOAD POLICY (no-path, *MARGRAVE*, relative path. with and without quotes)
#load policy conf1 = "conference1.p";
#LOAD POLICY conf2 = "*MARGRAVE*/tests/conference2.p";
//#load policy emptyconf = "./emptyconference.p";

// EXPLORE + UNDER + AND + NOT
let Q1[s: Subject, a: Action, r: Resource] be assigned(s, r) AND NOT assigned(s, r) 
UNDER conf1;

// more than one UNDER
let Q2[s: Subject, a: Action, r: Resource] be assigned(s, r) AND NOT assigned(s, r) 
UNDER conf1, conf2;

// IS POSSIBLE? + SHOW ONE + SHOW ALL
// expect: unsat
IS POSS? Q1;
SHOW Q1;
SHOW ALL Q1;

// IMPLIES
let Q3[s: Subject, a: Action, r: Resource] be conf1.permit(s, a, r) AND
(subject(s) IMPLIES NOT subject(s));

// expect: unsat
IS POSS? Q3;

// IFF
let Q4[s: Subject, a: Action, r: Resource] be conf1.permit(s, a, r) AND
(subject(s) IFF NOT subject(s));

// expect: unsat
IS POSS? Q4;

INFO LAST;

// basic request vector sugar
//explore conf1:permit(<conf1:req>);
// publish and vector in publish
//explore conf1:permit(<conf1:req>) PUBLISH <conf1:req>;
// last ID, info saved query
//info last;

COUNT Q4;




// TODO: realized (later in doc?)

// TODO: rest of syntax

