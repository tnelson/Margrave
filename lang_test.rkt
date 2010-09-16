#lang margrave

LOAD POLICY ./tests/fwex1.p; 
explore fwex1:accept(ipsrc, ipdest, portsrc, portdest, pro );
show one;
count;
get all;