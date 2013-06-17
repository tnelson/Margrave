#lang margrave
LOAD POLICY two = "tworules.p";
LOAD POLICY twoFA = "tworulesFA.p";

LET no[sa : IPAddress, da : IPAddress] BE
 two:deny(sa, sp, da, dp);
LET yes[sa : IPAddress, da : IPAddress] BE
 two:permit(sa, sp, da, dp);

LET noFA[sa : IPAddress, da : IPAddress] BE
 twoFA:deny(sa, sp, da, dp);
LET yesFA[sa : IPAddress, da : IPAddress] BE
 twoFA:permit(sa, sp, da, dp);

count yes;

count no;
count yesFA;
count noFA;

LET noFA2[sa : IPAddress, da : IPAddress] BE
 twoFA:deny(sa, sp, da, dp) debug 3;