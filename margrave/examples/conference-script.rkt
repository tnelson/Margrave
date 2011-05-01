#lang margrave

#load policy Mypol = "*margrave*/examples/conference1.p";
let Q[s : Subject ,a : Action,r : Resource] be Mypol.permit(s,a,r);
count Q;

let Q2[s : Subject, a: Action, r : Resource] be Q(s, a, r);
let Q3[s : Subject, a: Action, r : Resource] be Q(s, a, f(r));