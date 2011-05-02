#lang margrave

#load policy Mypol = "*margrave*/examples/conference1.p";
let Q[s : Subject ,a : Action,r : Resource] be Mypol.permit(s,a,r);
count Q;

let Q2[s : Subject, a: Action, r : Resource] be Q(s, a, r);
let Q3[s : Subject, a: Action, r : Resource] be Q(s, a, f(r));

let Q4[s : Subject, r : Resource] be forall a2 : Action (Q(s, a2, f(r)));
// !!! TODO : unbound leaf exception on show Q4. s/b better error. due to no sort A.

let Q5[s : Subject, r : Resource] be forall a2 : Action (Q(s, a2, f(r)));
count Q5;
show Q5;
is poss? Q5;

// But that will have a lot of vacuous solutions. If no actions...

let Q6[s : Subject, r : Resource] be forall a2 : Action (Q(s, a2, f(r))) and exists anAction : Action (anAction = anAction);
count Q6;
let Q7[s : Subject, r : Resource] be forall a2 : Action (Q(s, a2, f(r)))  and exists anAction : Action (anAction = anAction) CEILING 4;
count Q7;

let Q8[s : Subject ,a : Action,r : Resource] be Mypol.permit(s,a,r) and Mypol.deny(s,a,r);
is poss? Q8;

let Q9[s : Subject ,a : Action,r : Resource] be Mypol.permit(s,a,r) and s : Resource;
