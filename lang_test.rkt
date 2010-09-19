#lang margrave

// This is a comment
LOAD POLICY ./tests/fwex1.p; // this is also a comment
//explore fWeX1:ACCepT(ipsrc, ipdest, portsrc, portdest, pro ) TUPLING; 
//explore fWeX1:ACCepT(ipsrc, ipdest, portsrc, portdest, pro ) and ipaddress(ipsrc) and not ipaddress(ipsrc); 
//shoW one;
//count;
//shoW AlL;
//get all;
//EXPLORE fwex1:accept(<foo:bar>);
EXPLORE fwex1:accept(<fwex1:req>) and ipaddress(ipsrc) implies ipaddress(ipsrc);
show one;
//This is yet another comment
info;;