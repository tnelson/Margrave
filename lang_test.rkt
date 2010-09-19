#lang margrave

// This is a comment
LOAD POLICY ./tests/fwex1.p; // this is also a comment
//explore fWeX1:ACCepT(ipsrc, ipdest, portsrc, portdest, pro ) TUPLING; 
explore fWeX1:ACCepT(ipsrc, ipdest, portsrc, portdest, pro ) and ipaddress(ipsrc) and not ipaddress(ipsrc); 
//shoW one;
//count;
shoW AlL;
get all;
//EXPLORE fwex1:accept(<foo:bar>);
//This is yet another comment