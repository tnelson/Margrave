#lang reader "margrave-lang-read.rkt"

load policy ./tests/fwex1.p; 
explore fwex1:accept(ipsrc, ipdest, portsrc, portdest, pro );
show one;
count;
get all;