#lang margrave

#load policy Charlie = "*margrave*/examples/TM/charlie.p";
#load policy Bob = "*margrave*/examples/TM/bob.p";
#load policy Alice = "*margrave*/examples/TM/alice.p";
#info Alice;

let QCharlie[p:Person, d:Door] be 
  Charlie.canOpen(p, d);

let QBob[p:Person, d:Door] be 
  Bob.canOpen(p, d);

let QAlice[p:Person, d:Door] be 
  Alice.canOpen(p, d);
