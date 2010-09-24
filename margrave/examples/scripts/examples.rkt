#lang margrave

// This file contains non-IOS Margrave examples. 

// Define the policies used in this script.
// Each policy references a vocabulary, which is loaded automatically.
// The vocabulary defines (among other things) what shape a policy request takes.

LOAD POLICY "../../tests/conference1.p";
LOAD POLICY "../../tests/conference2.p";
LOAD POLICY "../../tests/phone1.p";
LOAD POLICY "../../tests/fwex1.p";
LOAD POLICY "../../tests/fwex1a.p";
LOAD POLICY "../../tests/fwex2.p";
LOAD POLICY "../../tests/happyrouterless.p";
LOAD POLICY "../../tests/happyroutermore.p";

// Policies are loaded under the name given in their policy file;
// we'd like to change that and use some better names in these examples.
rename conferencepolicy1 conf1;
rename conferencepolicy2 conf2;
rename fwex1 firewall1;
rename fwex1a firewall1a;
rename fwex2 firewall2;
rename happyrouterless HRless;
rename happyroutermore HRmore;

// ********************************************
// General examples
// ********************************************

// In the basic conference policy: When can someone read a paper?  
EXPLORE readpaper(a) and paper(r) and conf1:permit(s,a,r);
// The EXPLORE statement defines a query. Now we need to get its results.

// First, are there ANY scenarios that satisfy the query?
IS POSSIBLE?;

// Yes there are. But how many?
COUNT;

// 7 solutions. What are they?
SHOW ALL;

// (Margrave's result order isn't fixed, so these may be out of order.)
// * Both author and reviewer, and assigned to the paper
// * Both author and reviewer, both conflicted and assigned
// * Just a reviewer, both conflicted and assigned
// * Just a reviewer, neither conflicted nor assigned
// * Just a reviewer, only assigned
// * Just an author, neither conflicted nor assigned
// * Both author and reviewer, neither conflicted nor assigned

// ********************************************


// Want to continue to refine? Restate the same query, using the PUBLISH clause.
// The PUBLISH clause gives an ordering on the variables so you can use previous
// queries in new ones.

EXPLORE readpaper(a) and paper(r) and conf1:permit(s,a,r) PUBLISH s,a,r;

// Now we can refine that query.
// The LAST identifier means the last query created via EXPLORE.
EXPLORE reviewer(s) and last(s,a,r);

// How many of the solutions involve a reviewer?
COUNT;

// Now there are 6 solutions. We ruled out this one:
// * Just an author, neither conflicted nor assigned

// ********************************************



// Ok, so when is reading a paper NOT permitted?
EXPLORE readpaper(a) and paper(r) and subject(s) and not conf1:permit(s, a, r);

// Let's just ask for the first solution:
SHOW ONE;

SHOW NEXT;

// When no solutions remain, SHOW NEXT will give a 
// "no more solutions" message.

// ********************************************




// We've seen how to ask the most basic of questions:
// "When does this policy render that decision?"

// How about something more complex:
// "When can a reviewer read ALL papers?" 

// (Note for logicians: Like in Datalog,
//   we get universals by defining subqueries and negating them.)

// First query: There exists a reviewer who cannot read some paper.
EXPLORE reviewer(s) AND readpaper(a) AND paper(r)
            AND NOT conf1:Permit(s, a, r)
    PUBLISH s, a;

// The PUBLISH clause lets us dictate which variables are available for
// later use, and what order to use them in. Un-PUBLISHED variables are
// bound within the query and can be negated as seen below.

// You can always see what parameters a saved query takes, and in what order:
INFO last;

// And rename "last" to something more meaningful
RENAME LAST notallowed;

// Query: There is a reviewer to whom the last query does not apply."
EXPLORE reviewer(s) AND readpaper(a) AND NOT notallowed(s, a);

// Uncomment this line to see all the solutions
//SHOW ALL;
COUNT;

// That's odd: there are two solutions where there seem to be no papers
// at all! The policy's vocabulary says that all scenarios involve some 
// *Resource*, but we never said there had to be a *Paper*,
// and any scenario with no papers satisfies this query vacuously.

// Let's remove those confusing solutions:
EXPLORE reviewer(s) AND readpaper(a) AND NOT notallowed(s, a) AND paper(p);
COUNT;

// That query forces some paper to exist, which
// weeds out the two vacuous solutions from before.

// ********************************************

// A major feature of Margrave is change-impact analysis: Discovering all
// situations in which 2 policies will render different decisions.
// Suppose the two policies are Conference1 and Conference2: Conference2
// has a rule that Conference1 lacks.

// You can create your own change-impact query like so:

EXPLORE (conf1:Permit(sub, act, res) AND NOT conf2:Permit(sub, act, res)) OR
        (conf2:Permit(sub, act, res) AND NOT conf1:Permit(sub, act, res)) OR
        (conf1:Deny(sub, act, res) AND NOT conf2:Deny(sub, act, res)) OR
        (conf2:Deny(sub, act, res) AND NOT conf1:Deny(sub, act, res))
         PUBLISH sub, act, res;
         
COUNT;
SHOW ALL;

// the ordering used by LAST is now the standard ordering given by the policys' vocabulary.
// To confirm:
INFO last;

// And of course, change-impact queries can always be re-used via the LAST keyword.
EXPLORE author(sub) AND last(sub, act, res);
COUNT;
SHOW ALL;

// A word of caution: 
// be sure to provide a PUBLISH keyword, or the ordering may not be standard!




// ***********************************************************************************************

// You aren't restricted to simple "Permit" and "Deny" decisions. This
// example phone company policy has 3 different decisions: TollFree,
// Toll, and Refuse.

// When can someone make a toll-free call?
EXPLORE Phone1:TollFree(ncaller, nreceive);
COUNT;
SHOW ONE;

// **********************************************************************************************
// Example 1: Simple firewall policy, no state
// **********************************************************************************************


// What sorts of incoming packets do we allow through?
EXPLORE Firewall1:Accept(ipsrc, ipdest, portsrc, portdest, pro);
COUNT;
SHOW ONE;



// That is a lot of packet types (18), and so not very useful. Let's narrow the scope a bit.
// What packets are allowed through using TCP to port 21? (Should be 9.)
EXPLORE Firewall1:Accept(ipsrc, ipdest, portsrc, portdest, pro)
            AND TCP(pro) AND port21(portdest);
COUNT;
SHOW ONE;



// *********************************************************************************************
// Example 2: Simple stateless firewall policy, with ip ranges
// **********************************************************************************************

// What packets are allowed through using TCP to port 21?
EXPLORE Firewall2:Accept(ipsrc, ipdest, portsrc, portdest, pro) and tcp(pro) and port21(portdest);
COUNT;
SHOW ONE;

// Zero solutions.
// Nothing can connect to port 21 because of a misordering of rules.




// *********************************************************************************************
// Example 3: Sample ACL config from HappyRouter.com's Cisco ACL tutorial
// http://happyrouter.com/free-video-harden-your-cisco-router-with-ios-aclS

// When can a host on the less secure network have a 2-way TCP conversation with port 80 on the PC?
EXPLORE HRLess:Accept(ipoutside, ipinside, portoutside, portinside, pro)
    and HRMore:Accept(ipinside, ipoutside, portinside, portoutside, pro)
    and tcp(pro) and http(portinside) and 10network(ipoutside) and pc(ipinside);
COUNT;
SHOW ONE;


// For large firewalls, it may help to turn on the TUPLING optimization:
EXPLORE HRLess:Accept(ipoutside, ipinside, portoutside, portinside, pro)
    and HRMore:Accept(ipinside, ipoutside, portinside, portoutside, pro)
    and tcp(pro) and http(portinside) and 10network(ipoutside) and pc(ipinside)
    TUPLING;
COUNT;
SHOW ONE;

// Tupling often results in fewer solutions, but those solutions still
// characterize all possible satisfying scenarios for the query.

