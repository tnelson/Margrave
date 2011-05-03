#lang margrave

#load policy WPI = "*margrave*/examples/TM/WPI.p";

#load policy Charlie = "*margrave*/examples/TM/charlie.p";
#load policy Bob = "*margrave*/examples/TM/bob.p";
#load policy Alice = "*margrave*/examples/TM/alice.p";

#info Alice;

let QCharlie[p:Person, d:Door] be 
  Charlie.canOpen(p, d);

let QBob[p:Person, d:Door] be 
  Bob.canOpen(p, d);

let QAlice[p:Person, d:Door] be 
  Alice.canOpen(p, d)
  CEILING 7;

COUNT QCharlie;
COUNT QBob;

// QAlice has a very large size ceiling.
SHOW QAlice;
IS POSS? QAlice;

//Count QAlice;

// Who can open the grad center door?
let QA2[p: Person, d: Door] be QAlice(p, d) and d:GradCtrDoor;
// Who can open ALL doors?
let QA3[p: Person] be forall d : Door (QAlice(p, d));

let QA4[p: Person, d: Door] be QAlice(p, d) and Alice.AliceTrustsBob_matches(p, d)
  DEBUG 3;
