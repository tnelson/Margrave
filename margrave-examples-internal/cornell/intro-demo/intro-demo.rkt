#lang margrave

// this example starts with the first two rules from the notes, which
// offer conflicting decisions on certain packets. Queries show the conflict.
// Then add first-applicable rule combinator (tworulesFA, further down in this file)
// and show that the conflict disappears

LOAD POLICY tworules = "tworules.p";

// compute denied packets from 10.1.1.2 to 10.1.20.20
let basicdeny[sp: Port, dp: Port] be 
  tworules:deny($10.1.1.2, sp, $10.1.20.20, dp);

// compute permitted packets from 10.1.1.2 to 10.1.20.20
let basicpermit[sp: Port, dp: Port] be 
  tworules:permit($10.1.1.2, sp, $10.1.20.20, dp);

// Any denied packets from 10.1.1.2 to 10.1.20.20? (yes)
poss? basicdeny;

// Any permitted packets from 10.1.1.2 to 10.1.20.20? (yes)
poss? basicpermit;

// if want to get into scenarios this early, use "show" instead of "poss?"

LOAD POLICY tworulesFA = "tworulesFA.p";

// compute denied packets from 10.1.1.2 to 10.1.20.20
let denyUnderFA[sp: Port, dp: Port] be 
  tworulesFA:deny($10.1.1.2, sp, $10.1.20.20, dp);

// compute permitted packets from 10.1.1.2 to 10.1.20.20
let permitUnderFA[sp: Port, dp: Port] be 
  tworulesFA:permit($10.1.1.2, sp, $10.1.20.20, dp);
  
poss? denyUnderFA;  // yes
poss? permitUnderFA; // no
