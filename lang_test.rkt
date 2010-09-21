#lang margrave

// This is a comment

INFO;

LOAD POLICY ./tests/fwex1.p; // this is also a comment

GET RULES IN fwex1;

//explore fWeX1:ACCepT(ipsrc, ipdest, portsrc, portdest, pro ) TUPLING; 

explore fWeX1:ACCepT(ipsrc, ipdest, portsrc, portdest, pro ) and ipaddress(ipsrc) and not ipaddress(ipsrc); 
shoW one;
count;
shoW AlL;
get all;
//EXPLORE fwex1:accept(<foo:bar>);

// AND binds tighter than IMPLIES, so don't forget the parens:
EXPLORE fwex1:accept(<fwex1:req>) and (ipaddress(ipsrc) implies ipaddress(ipsrc));
show one;

//This is yet another comment

info fwex1;
info last;

//LOAD IOS ./examples/policies/ios-demo/change1/change1.txt
// oh no this comment is in the middle of a statement. i sure hope that
// the lexer ignores them
//WITH "pre" "suf";
// gotta do pre/suf first or else the id will be used
//LOAD IOS ./examples/policies/ios-demo/initial/demo.txt;
//info

// trailing whitespace (and comment, which can't be trimmed!) after the next command, make sure it doesn't clog the parser
//quit;             //comment