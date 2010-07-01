/*
  	Copyright 2009 Brown University and Worcester Polytechnic Institute.
    
    This file is part of Margrave.

    Margrave is free software: you can redistribute it and/or modify
    it under the terms of the GNU Lesser General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    Margrave is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU Lesser General Public License for more details.

    You should have received a copy of the GNU Lesser General Public License
    along with Margrave.  If not, see <http://www.gnu.org/licenses/>.
*/

// tn

package edu.wpi.margrave;

import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.OutputStream;
import java.io.PrintStream;
import java.lang.management.ManagementFactory;
import java.lang.management.ThreadMXBean;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;


import com.sun.xacml.ParsingException;
import com.sun.xacml.UnknownIdentifierException;

public class MJavaTests
{
	
	static void countTest(String testname, MQuery qry, int expected_size, int expected_sols, int expected_hbu)
	throws MGEUnsortedVariable, MGEUnknownIdentifier, MGEArityMismatch, MGEBadQueryString, MGEManagerException, MGEBadIdentifierName
	{		
	
		if(qry.runTestCase(expected_size, expected_sols, expected_hbu))
			//System.out.println("Test: "+testname+" passed.");
			// too many cases, getting spammy
			;
		else
		{
			System.out.println("vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv");
			System.out.println("Test: "+testname+" failed!");
			System.out.println("^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^");
		}
			
	}
	
	static void do_test_1() 
	throws MGEBadIdentifierName, MGEUnknownIdentifier, MGEArityMismatch, MGEBadCombinator, MGECombineVocabs,
	MGEUnsortedVariable, MGEBadQueryString, MGEManagerException
	{
		MVocab env = new MVocab("Test Vocab");
		
		MPolicyLeaf pol = new MPolicyLeaf("TestPol1", env);
		MPolicyLeaf pol2 = new MPolicyLeaf("TestPol2", env);

		env.addDecision("Permit");
		env.addDecision("Deny");
		
		env.addSort("Subject");
		env.addSort("Action");
		env.addSort("Resource");

		env.addRequestVar("s", "Subject");
		env.addRequestVar("a", "Action");
		env.addRequestVar("r", "Resource");
		
		env.addSubSort("Subject", "Author");
		env.addSubSort("Subject", "Reviewer");
		env.addSubSort("Action", "SubmitPaper");
		env.addSubSort("Action", "ReadPaper");
		env.addSubSort("Action", "SubmitReview");
		env.addSubSort("Resource", "Paper");
		env.addSubSort("Resource", "Review"); // yes, a Review is a resource
		
		// Test: Don't allow cyclic type tree
		boolean fine = true;
		try
		{
			env.addSubSort("Author", "Subject");
		}
		catch(MGEBadIdentifierName e)
		{
			//System.out.println("Test: no cycles in types passed.");
			//System.out.println(e);
			fine = false;
		}
		if(fine)
		{
			System.out.println("vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv");
			System.out.println("Test: no cycles in types FAILED.");
			System.out.println("^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^");
			
		}
				
		env.axioms.addConstraintDisjointAll("Resource");
		env.axioms.addConstraintDisjointAll("Action");
		env.axioms.addConstraintDisjointAll("Subject");		
		
		// Look for interesting cases -- not the models with no subjects or resources!
		// (Note this is Nonempty, NOT nonemptyall (which would mean resources and subjects of ALL types m/b present.)
		env.axioms.addConstraintNonempty("Subject");
		env.axioms.addConstraintNonempty("Resource");
		
		// Note that this is NOT one action per request!
		// This is one atom per action TYPE (as we would expect)
		
		//env.addConstraintSingletonAll("Action");
		// 12/12/08 added new constraint type -- no longer forces atoms PER action, just some action exists (below)
		env.axioms.addConstraintAtMostOneAll("Action");
		env.axioms.addConstraintNonempty("Action");
		
		// Means that there is a single action per request. This is not the intuitive meaning
		// but it makes the output easier ;) 
		
		// Note the "all" here demonstrates the danger of using SB.
		// This may be a problem!
		// Poss. fix: Add existentials for things constrained (not just universally?)
		
		// Be careful. This problem may occur anywhere we use .one or .some in the formula
		// (even if not constrained)
		// *************************************
		//env.addConstraintSingleton("Action");
				
		env.addPredicate("Conflicted", "Reviewer Paper"); // these relations are both of type (Reviewer X Paper)
		env.addPredicate("Assigned", "Reviewer Paper");
				
		ArrayList<String> templist = new ArrayList<String>();
		templist.clear();		
		templist.add("!Conflicted s r");
		templist.add("ReadPaper a");
		templist.add("Paper r");
		pol.addRule("PaperNoConflict", "Permit", templist);
		pol2.addRule("PaperNoConflict", "Permit", templist);
		
		templist.clear();
		templist.add("Assigned s r");
		templist.add("ReadPaper a");
		templist.add("Paper r");
		pol.addRule("PaperAssigned", "Permit", templist);
		pol2.addRule("PaperAssigned", "Permit", templist);
		
		templist.clear();
		templist.add("Conflicted s r");
		templist.add("ReadPaper a");
		templist.add("Paper r");
		//pol.addRule("PaperConflict", "Deny", templist);
		pol2.addRule("PaperConflict", "Deny", templist);
		
		// Pol 2 has extra rule: PaperConflict
		
		//System.out.println("Deny overrides:");
		pol.rCombine = "O Deny Permit"; 
		pol.initIDBs();
		//pol.prettyPrintIDBs();				
		
		pol2.rCombine = "O Deny Permit";
		pol2.initIDBs();
		
		MQuery qry;
		ArrayList<MIDBCollection> pollist = new ArrayList<MIDBCollection>();		
				
		qry = pol.compareWithPolicy(pol2);
		//qry.debug_verbosity = 3;
		countTest("Diff Policies (Basic)", qry, 3, 2, 3);			
		qry.addIDBOutputs(pol.getQualifiedIDBNameList());
		qry.addIDBOutputs(pol2.getQualifiedIDBNameList());
		System.out.println("Diff Policies (Basic) output [IDBs shown!]: ");
		//qry.prettyPrintSolutions();
		
		//qry.debug_verbosity = 3;
		
		/*if(7 != qry.getPossiblyNonemptyRelations(qry.idb_names_to_output).size())
		{
			System.out.println("vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv");
			System.out.println("Test: Possibly empty relation listing FAILED!");
			System.out.println("^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^");
		}
		else
		{
			System.out.println("Test: Possibly empty relation listing Passed.");
		}
			*/
		//System.out.println(qry.getPossiblyNonemptyRelations(qry.idb_names_to_output));
		
		// TODO
		qry.prettyPrintOneSolution();
		
		//System.exit(2);
		
		
		// CANNOT just tack on (Assigned s r) ... need the following:
		
		
		pollist.clear();
		pollist.add(pol);
		pollist.add(pol2);
		qry = MQuery.queryThesePolicies("(forSome s Subject (forSome a Action (forSome r Resource " +
				"(and (Assigned s r) " +
				     "(or (and (TestPol1:Permit s a r) (not (TestPol2:Permit s a r))) " +
				         "(and (TestPol1:Deny s a r) (not (TestPol2:Deny s a r))) "+
				         "(and (TestPol2:Permit s a r) (not (TestPol1:Permit s a r))) " +
				         "(and (TestPol2:Deny s a r) (not (TestPol1:Deny s a r)))"+ 
				    ")))))", pollist);		
		countTest("Solutions to above with (Assigned s r) only", qry, 3, 1, 3);
			
		// Make sure constraint existentials are being generated: should add nonempty_constraint_resource ext.
		// reviewer+assigned+!conf, reviewer+assigned+conf, reviewier+!assigned+!conf, author+!assigned+!conf 
		// and !conf+!assigned+subject(other)
		qry = pol.queryPolicy("(forSome s Subject (forSome a Action (forAll r Resource (TestPol1:Permit s a r))))");
		
		countTest("Nonempty constraints functioning with for-all (1)", qry, 3, 5, 3);
		
		
		// shouldn't include the conflicted one
		qry = pol2.queryPolicy("(forSome s Subject (forSome a Action (forAll r Resource (TestPol2:Permit s a r))))");
		countTest("Nonempty constraints functioning with for-all (2)", qry, 3, 4, 3);		
		
		// Now test first-applicable. The below (same) rules will have different results
		
		MPolicyLeaf polfax = new MPolicyLeaf("faxp", env);
		MPolicyLeaf polfac = new MPolicyLeaf("facp", env);
		MPolicyLeaf polnone = new MPolicyLeaf("nonep", env);
		
		templist.clear();		
		templist.add("!Conflicted s r");
		templist.add("ReadPaper a");
		templist.add("Paper r");
		polfax.addRule("PaperNoConflict", "Permit", templist);
		polfac.addRule("PaperNoConflict", "Permit", templist);
		polnone.addRule("PaperNoConflict", "Permit", templist);
		
		templist.clear();
		templist.add("Assigned s r");
		templist.add("ReadPaper a");
		templist.add("Paper r");
		polfax.addRule("PaperAssigned", "Permit", templist);
		polfac.addRule("PaperAssigned", "Permit", templist);
		polnone.addRule("PaperAssigned", "Permit", templist);
				
		
		templist.clear();
		templist.add("Conflicted s r");
		templist.add("ReadPaper a");
		templist.add("Paper r");
		polfax.addRule("PaperConflict", "Deny", templist);
		polfac.addRule("PaperConflict", "Deny", templist);
		polnone.addRule("PaperConflict", "Deny", templist);
		
		polfax.rCombine = "FAX";
		polfac.rCombine = "FAC";
		polnone.rCombine = "NONE";
		polfax.initIDBs();
		polfac.initIDBs();
		polnone.initIDBs();
		
		
		// Both should give 1
		System.out.println(polfac.ruleIDBsWithHigherPriorityThan("PaperAssigned"));
		System.out.println(pol2.ruleIDBsWithHigherPriorityThan("PaperAssigned"));
		System.out.println(polfac.ruleIDBsWithHigherPriorityThan("facp:PaperAssigned"));
		System.out.println(pol2.ruleIDBsWithHigherPriorityThan("TestPol2:PaperAssigned"));
		
		System.out.println(pol2.getDecisionForRuleIDBName("PaperAssigned"));
		System.out.println(pol2.getDecisionForRuleIDBName("TestPol2:PaperAssigned"));
		
		// FAC permit
		// 1) Reviewer reading paper, neither assigned nor conflicted (!Conflict triggers first rule)
		// 2) Reviewer reading paper, BOTH conflicted and assigned (FA's effect here)
		// 3) An Author reading a paper, not conflicted nor assigned (same as #1, but diff. sort)
		// 4) Reviewer assigned to read paper
		// Note still no instance where conf and !assigned due to final rule's effect
	
		// FAX permit 
		// 1 Reviewer reading paper, neither conf nor ass
		// 2 Reviewer reading paper, assigned only
		// 3 Author reading paper, neither
		// 4 And finally, a subject that is not any of the given subtypes, reading paper.
		// (Rule 2 cannot ever take effect, nor can rule 3, due to XACML 2.0 FA semantics)
		
		// Note re: partials, 3&4 share a don't-care: Author($s).
		
		MQuery faqry = polfax.queryPolicy("(forSome s Subject (forSome a Action (forSome r Resource (faxp:Permit s a r))))");
		countTest("Test of first-applicable (FAX) #1", faqry, 3, 4, 3);		
			
		
		
//		polfax.prettyPrintIDBs();		
//		faqry.prettyPrintSolutions();

		
		
		// no deny for reviewer reading a conflicted paper (since target of prior rule overrode, gave N/a)
		faqry = polfax.queryPolicy("(forSome s Subject (forSome a Action (forSome r Resource (faxp:Deny s a r))))");
		countTest("Test of first-applicable (FAX) #2", faqry, 3, 0, 3);
		
		/*
		The space of requests w/ a=readpaper:
			Subject (only), Author, Reviewer
			Resource (only), Paper, Review

			// these apply because all 3 rules have target "paper r"
			// and not a paper! just another resource
			// all 3 subject possibilities
			*** SOLUTION: Size = 3.
			$s: reviewer 
			$r: resource 
			$a: readpaper 
			conflicted = {}
			assigned = {}
			*** SOLUTION: Size = 3.
			$s: author 
			$r: resource 
			$a: readpaper 
			conflicted = {}
			assigned = {}
			*** SOLUTION: Size = 3.
			$s: subject 
			$r: resource 
			$a: readpaper 
			conflicted = {}
			assigned = {}

			// similarly, these all involve reviews, not papers
			// all 3 subject possibilities
			*** SOLUTION: Size = 3.
			$s: reviewer 
			$r: review 
			$a: readpaper 
			conflicted = {}
			assigned = {}
			*** SOLUTION: Size = 3.
			$s: subject 
			$r: review 
			$a: readpaper 
			conflicted = {}
			assigned = {}
			*** SOLUTION: Size = 3.
			$s: author 
			$r: review 
			$a: readpaper 
			conflicted = {}
			assigned = {}

			// Finally, if conflicted rule 1's target is met but not its condition.
			// XACML FA dictates this is N/a.
			*** SOLUTION: Size = 3.
			$s: reviewer 
			$r: paper 
			$a: readpaper 
			conflicted = {[$s, $r]}
			assigned = {[$s, $r]}

			*** SOLUTION: Size = 3.
			$s: reviewer 
			$r: paper 
			$a: readpaper 
			conflicted = {[$s, $r]}
			assigned = {}
			*/
		faqry = polfax.queryPolicy("(forSome s Subject (forSome a Action (forSome r Resource (and (not (faxp:Permit s a r)) (not (faxp:Deny s a r)) (ReadPaper a)))))");		
		countTest("Restricted N/a (FAX)", faqry, 3, 8, 3);		
		
		// test FAC
		
		MQuery faqry2 = polfac.queryPolicy("(forSome s Subject (forSome a Action (forSome r Resource (facp:Permit s a r))))");
		countTest("Test of first-applicable (FAC) #1", faqry2, 3, 5, 3);				
				
		faqry2 = polfac.queryPolicy("(forSome s Subject (forSome a Action (forSome r Resource (facp:Deny s a r))))");
		countTest("Test of first-applicable (FAC) #2", faqry2, 3, 1, 3); // reviewer reading, conflicted and not assigned

		
		faqry2 = polfac.queryPolicy("(forSome s Subject (forSome a Action (forSome r Resource (and (not (facp:Permit s a r)) (not (facp:Deny s a r)) (ReadPaper a)))))");		
		countTest("Restricted N/a (FAC)", faqry2, 3, 6, 3); // one fewer ReadPaper N/a with FAC (since it denies one) + 4 "plain subject" or "plain resource"

		// Test NONE (breaks function properties!)
		
		MQuery nqry = polnone.queryPolicy("(forSome s Subject (forSome a Action (forSome r Resource (and (nonep:Permit s a r) (nonep:Deny s a r)))))");		
		countTest("Test of NONE rule combinator", nqry, 3, 1, 3); // only one solution: assigned + conflicted 

		
		// Make certain that the user can narrow scope of variables in quantifiers.
		// H.U. ceiling should count subsorts as covering their supersorts.
		faqry2 = polfac.queryPolicy("(forSome s Subject (forSome a ReadPaper (forSome r Paper (facp:Permit s a r))))");		
		countTest("Can narrow scope in quantifier", faqry2, 3, 5, 3); 

				
		// Test to make sure newlines are dealt with properly by the parser
		// Scheme input is often formatted in a human friendly manner.
		List<MIDBCollection> pollist2 = new ArrayList<MIDBCollection>();
		pollist2.add(polfac);
		pollist2.add(polfax);
		faqry2 = MQuery.queryThesePolicies("(forsome sub Subject \n        (forsome act Action\n                 (forsome res Resource (or (and (facp:Permit sub act res)\n                                            (not (faxp:Permit sub act res)))\n                                           (and (facp:Deny sub act res)\n                                                (not (faxp:Deny sub act res)))\n                                           (and (faxp:Permit sub act res)                                                (not (facp:Permit sub act res)))                                           (and (faxp:Deny sub act res)                                                (not (facp:Deny sub act res)))))))", pollist2);				
		countTest("Parser works on newlines and whitespace", faqry2, 3, 2, 3); 		
		// (disagree with conflicted, regardless of assigned.)
		
		faqry2 = polfac.queryPolicy("(forSome s Subject (forSome a Action (forSome r Resource (facp:Permit s a r)) ))");		
		countTest("Parser works on whitespace in final parens", faqry2, 3, 5, 3); 		
				
		
		// Test _RuleName predicates in parser
		MQuery rulequery = polfax.queryPolicy("(forSome as Subject (forsome aa Action (forsome ar Resource (and (faxp:permit as aa ar) (faxp:PaperNoConflict_Applies as aa ar)))))");
		countTest("Rule predicates Part 1", rulequery, 3, 4, 3);
		//rulequery.prettyPrintSolutions();
	
		
		rulequery = polfax.queryPolicy("(forSome as Subject (forsome aa Action (forsome ar Resource (and (faxp:permit as aa ar) (faxp:PaperAssigned_Applies as aa ar) (not (faxp:PaperNoConflict_Applies as aa ar))))))"); 
		countTest("Rule predicates Part 2", rulequery, 3, 0, 3);
		// If noconflict holds, under fax semantics paperassigned can never hold.


	}
	
	static void do_test_2() 
	throws MGEBadIdentifierName, MGEUnknownIdentifier, MGEArityMismatch, MGEBadCombinator, 
	MGEBadIdentifierName, MGEBadQueryString, MGEUnsortedVariable, MGECombineVocabs, MGEManagerException
	{
		MVocab env = new MVocab("Bigger Test Vocab");
		MPolicyLeaf pol = new MPolicyLeaf("ipeqtest", env);
		boolean passed = false;
		
		env.addDecision("Permit");
		env.addDecision("Deny");
		env.addDecision("CallPolice");		
		
		try
		{
			env.addDecision("Foo ");			
		}
		catch(MGEBadIdentifierName e)
		{
			System.out.println("Test: no spaces in decision names passed.");
			passed = true;
		}
		if(!passed)
		{
			System.out.println("vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv");
			System.out.println("Test: no spaces in decision names FAILED.");
			System.out.println("^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^");
		}
				
				
		env.addSort("IpAddress");
		env.addSort("Bit");
		
		env.addSubSort("Bit", "0");
		env.addSubSort("Bit", "1");
		
		env.axioms.addConstraintAbstract("Bit");
		
		env.addRequestVar("ipin", "IpAddress");
		env.addRequestVar("ipout", "IpAddress");
		
		//env.addOtherVar("a", "IpAddress");
		//env.addOtherVar("b", "IpAddress");
		
		env.addOtherVar("b1", "Bit");
		env.addOtherVar("b2", "Bit");
		
		env.axioms.addConstraintDisjointAll("IpAddress");
		
		env.axioms.addConstraintDisjointAll("Bit");
		env.axioms.addConstraintAtMostOneAll("Bit"); // 0 and 1 are atomic

		env.axioms.addConstraintNonempty("IpAddress"); // some IP Address **exists**
		
		env.addPredicate("FirstBit", "IpAddress Bit"); // func from IP Address to Bit
		
		ArrayList<String> templist = new ArrayList<String>();
		templist.clear();		
		
		// "dumb" query formulation to test HU ceiling
		templist.add("= b1 b2");
		templist.add("FirstBit ipin b1");
		templist.add("FirstBit ipout b2");
		pol.addRule("In and Out same first bit", "Permit", templist);		
		
		env.axioms.addConstraintTotalFunction("FirstBit");
		//env.addConstraintPartialFunction("FirstBit");
		
		// disabled for now
		//env.addConstraintDisjointVars("ipin", "ipout");
		
		pol.rCombine = "O CallPolice Deny Permit";
		pol.initIDBs();
		
		MQuery qry;
				
		// TODO 4 bit atoms is silly given that bit is abstract and both its subsorts are LONE.
		qry = pol.queryPolicy("(forSome ipin IpAddress (forSome ipout IpAddress " +
				"(and (ipeqtest:Permit ipin ipout) (not (= ipin ipout)))))");
		//qry.prettyPrintSolutions();
		//qry.debug_show_all_formula = true;
		countTest("First bit of IPs equal?", qry, 4, 4, 6); // 4 at size 4 

		countTest("First bit of IPs equal? (higher size)", qry, 5, 6, 6);
		// all 4 -> 0, all 4 -> 1
		// all 3 -> 0 (1 unused), all 3 -> 1 (0 unused)
		// spare ip -> 1 (in/out = 0), spare ip -> 0 (in/out 1)
		
		// Redo this with a smarter query formulation
		pol.rules.clear();
		
		templist.clear();
		templist.add("FirstBit ipin b1");
		templist.add("FirstBit ipout b1");
		pol.addRule("In and Out same first bit", "Permit", templist);	
		
		pol.rCombine = "O CallPolice Deny Permit";
		pol.initIDBs();		

		// Same! But should change max universe size (since one less existential) 
		
		qry = pol.queryPolicy("(forSome ipin IpAddress (ForSome ipout IpAddress " +
				"(and (ipeqtest:Permit ipin ipout) (not (= ipin ipout)))))");
		countTest("SMART First bit of IPs equal?", qry, 4, 4, 5); 
		countTest("SMART First bit of IPs equal? (higher size)", qry, 5, 6, 5); 
		
		
		
		
		// Make sure this cycle is detected: the T.F. says there's a func from IP -> Bit. This induces one from Bit -> IP. Have one nonempty.
		qry = pol.queryPolicy("(forAll bitin Bit (forSome ipto IpAddress (FirstBit ipto bitin)))");
		//qry.debug_show_all_formula = true;
		//qry.prettyPrintSolutions();
		countTest("cycle detected across total function constraint", qry, 1, 0, -1); // dont care about solutions, detect the cycle though.		
	}
	
	static void do_test_3() 
	throws MGEBadIdentifierName, MGEUnknownIdentifier, MGEArityMismatch, MGEBadCombinator, 
	MGEBadIdentifierName, MGEBadQueryString, MGEUnsortedVariable, MGECombineVocabs, MGEManagerException
	{
		MVocab env = new MVocab("Vocab for closure tests");
		MPolicyLeaf pol = new MPolicyLeaf("closuretest", env);
		
		env.addDecision("Permit");
		env.addDecision("Deny");
		env.addDecision("CallPolice");
		
						
		env.addSort("Subject");
		
		env.addSubSort("Subject", "Admin");
		env.addSubSort("Subject", "User");
		
		env.addRequestVar("s", "Subject");
		
		env.addOtherVar("s1", "Subject");
		
		env.axioms.addConstraintDisjointAll("Subject");		
		env.axioms.addConstraintNonempty("Subject");
		
		env.addPredicate("Delegate", "Subject Subject"); 
		
		ArrayList<String> templist = new ArrayList<String>();
		templist.clear();		
		templist.add("Admin s1");
		templist.add("Delegate* s1 s");
		pol.addRule("ChainOfDelegation", "Permit", templist);
		
		
		pol.rCombine = "O CallPolice Deny Permit";
		pol.initIDBs();
		
		
		MQuery qry = pol.queryPolicy("(forSome s Subject (closuretest:Permit s))");
		qry.sizeCeiling = 2;
		//qry.debug_show_all_formula = true;		
		
		// far too many solutions even at low model sizes...
		// (50 @ 2, 2 @ 1)
		//qry.prettyPrintSolutions();
		
		// 2 solns (One atom only which is an admin, whether delegated or not!)
		// (If s is an admin, delegate can be empty; the REFLEXIVE closure
		//  will contain ($s, $s) and satisfy the policy.
		countTest("Basic Closure Test 1", qry, 1, 2, -1);		
		
		
		
		
		
		
		MQuery qry2 = qry.refineQuery("(forAll potential Subject (not (Delegate potential potential)))");
		qry2.sizeCeiling = 1;
		
		// 1 soln (only one of the last test)
		//qry2.prettyPrintSolutions(); // 14 @ 2
		countTest("Basic Closure Test 2", qry2, 1, 1, -1);
		
		// Now, what if we require that s is not an admin?
		// then 2 solns: One admin, one user (disjoint), where admin delegates to user, whether or not
		// another 2 where $s: subject (non-user)
		MQuery qry3 = pol.queryPolicy("(and (forAll potential Subject (not (Delegate potential potential))) (forSome s Subject (and (not (Admin s)) (closuretest:Permit s))))");
		qry3.sizeCeiling = 2;
		
		//qry3.prettyPrintSolutions();
		countTest("Basic Closure Test 3", qry3, 2, 4, -1);
		
		// Test override
		
		env.addOtherVar("s2", "Subject");

		templist.clear();
		templist.add("Admin s");
		pol.addRule("Deny if s is itself an admin!", "Deny", templist);
		
		templist.clear();
		templist.add("Delegate s2 s2");
		pol.addRule("Don't allow someone to delegate to themselves.", "CallPolice", templist);
				
		pol.initIDBs();
		
		
		// Deny and CallPolice should override Permit by the above combinator, ruling out  atom solutions from before!
		// Note that this also tests the "no self-delegation"
		// Since CallPolice :- (Delegate s2 s2),
		// we Permit :- (some stuff) and !(exists s2 s.t. (Delegate s2 s2))
		// Permit :- (some stuff) and forall s2, (!Deleg s2 s2)
		// + another 2 (subject, not user)
		qry = pol.queryPolicy("(forSome s Subject (closuretest:Permit s))");
		qry.sizeCeiling = 2;
		countTest("Refined Closure Test 1", qry, 2, 4, -1);
			
		
		//qry.prettyPrintSolutions();
		
		qry = pol.queryPolicy("(forSome s Subject (closuretest:Deny s))");		
		countTest("Refined Closure Test 2", qry, 1, 1, 1);

		
		qry = pol.queryPolicy("(forSome s Subject (closuretest:CallPolice s))");
		//qry.debug_show_all_formula = true; // user, subject, admin
		countTest("Refined Closure Test 3", qry, 1, 3, 2); // no closure used here or in #2
	}
	
	static void do_test_4() 
	throws MGEBadIdentifierName, MGEUnknownIdentifier, MGEArityMismatch, MGEBadCombinator, 
	MGEBadIdentifierName, MGEBadQueryString, MGEUnsortedVariable, MGECombineVocabs, MGEManagerException
	{

		// try different types of policy networks (need to implement this stuff!)
								
		// create policy and call setTarget on it
		// then init IDBs... 
		
		
		// test for first-applicable policy combination
		MVocab env = new MVocab("Vocab for policy combo tests");		
		
		env.addDecision("Permit");
		env.addDecision("Deny");
		env.addDecision("AlertAdmin");
		
		env.addSort("IPAddress");
		
		env.addSubSort("IPAddress", "Local");
		env.addSubSort("IPAddress", "External");

		env.addSubSort("IPAddress", "rangexy");

		env.axioms.addConstraintDisjoint("Local", "External");		
		env.axioms.addConstraintNonempty("IPAddress");
		env.axioms.addConstraintNonempty("Local");
		env.axioms.addConstraintNonempty("External");
		
		env.addRequestVar("ipfrom", "IPAddress");
		env.addRequestVar("ipto", "IPAddress");	
		
		// note no solutions at size 2...
		
		
		env.addPredicate("Conversation", "IPAddress IPAddress"); 
		env.addPredicate("HackerList", "IPAddress");
		
		MPolicyLeaf polchild1 = new MPolicyLeaf("child1", env);
		MPolicyLeaf polchild2 = new MPolicyLeaf("child2", env);
		MPolicySet polparent = new MPolicySet("parent", env);

		/*
		 * Child 1
		 * Permits if the two ips are in a conversation
		 */
		ArrayList<String> templist = new ArrayList<String>();
		templist.clear();		
		templist.add("Conversation ipfrom ipto");
		polchild1.addRule("If conversation in progress between addrs", "Permit", templist);				
		polchild1.rCombine = "O AlertAdmin Deny Permit";	

		
		/*
		 * Child 2
		 * Alerts Admin if the two ips are both External or Local 
		 */		
		templist.clear();		
		templist.add("Local ipfrom");
		templist.add("Local ipto");		
		polchild2.addRule("Both local", "AlertAdmin", templist);				
		templist.clear();		
		templist.add("External ipfrom");
		templist.add("External ipto");		
		polchild2.addRule("Both external", "AlertAdmin", templist);							
		polchild2.rCombine = "O AlertAdmin Deny Permit";
		
		/*
		 * Parent
		 */
		polparent.addChild(polchild1);
		polparent.addChild(polchild2);						
		polparent.pCombine = "O AlertAdmin Deny Permit";
		
		polparent.initIDBs();		
					
		
/*		polparent.prettyPrintIDBs();
		polchild1.prettyPrintIDBs();
		polchild2.prettyPrintIDBs();
		
		
		System.out.println(polparent.getExistentialRequestPrefix());
		System.out.println(polparent.getRequestPrefixClosing());
		System.out.println(polparent.getRequestVarVector());
		System.out.println(polparent.getIDBNameList());
		
		System.exit(1);
		*/
		
		
		// Test that simplifier visitor is working
		// Commented out: Can no longer use string comparison since we're using NaryFormulas now.
		// sets will have arbitrary order.
/*		if(!"(((ipfrom -> ipto) in conversation) && !(((ipfrom in local) && (ipto in local)) || ((ipfrom in external) && (ipto in external))))".toLowerCase().equals(polparent.idbs.get("permit").toString()))
			System.out.println("-->>>>>>>>>>>>>> BASIC SIMPLIFICATION TEST CASE FAILED!");
		else
			System.out.println("Basic simplificiation test case passed.");
				*/
		
		//"(forSome ipfrom IPAddress (forSome ipto IPAddress (child2:AlertAdmin ipfrom ipto)))" has...
		// Too many solutions at model size 3. (1 in Local, 2 in external, ipfrom and ipto are the 2 in external)
		
		MQuery tempqry = polchild2.queryPolicy("(forSome ipfrom IPAddress (forSome ipto IPAddress (and (child2:AlertAdmin ipfrom ipto) (child2:Permit ipfrom ipto))))");
		countTest("Contradiction has 0 solutions", tempqry, 4, 0, 4);

		// DO NOT UNCOMMENT: Disjoint Vars constraints have been disabled!
		// test no alertadmins have ipfrom = ipto (respecting constraint)
		//MGQuery tempqry2 = polchild2.queryPolicy("(forSome ipfrom IPAddress (forSome ipto IPAddress (= ipfrom ipto)))");
		//countTest("IPFROM != IPTO constraint respected", tempqry2, 5, 0, 5);
				
		
		ArrayList<MIDBCollection> pollist = new ArrayList<MIDBCollection>();
		pollist.add(polchild1);
		pollist.add(polchild2);
		pollist.add(polparent);
		MQuery tempqry3 = MQuery.queryThesePolicies("(forSome ipfrom IPAddress (forSome ipto IPAddress (and (parent:AlertAdmin ipfrom ipto) (not (child2:AlertAdmin ipfrom ipto)))))", pollist);
		countTest("child2's alertadmin contains parent's alertadmin", tempqry3, 4, 0, 4);

		MQuery tempqry4 = MQuery.queryThesePolicies("(forSome ipfrom IPAddress (forSome ipto IPAddress (and (child2:AlertAdmin ipfrom ipto) (not (parent:AlertAdmin ipfrom ipto)))))", pollist);
		countTest("parent's alertadmin contains child2's alertadmin", tempqry4, 4, 0, 4);
		
		MQuery tempqry5 = MQuery.queryThesePolicies("(forSome ipfrom IPAddress (forSome ipto IPAddress (and (parent:Permit ipfrom ipto) (not (child1:Permit ipfrom ipto)))))", pollist);
		countTest("parent's permit contains child1's permit", tempqry5, 4, 0, 4);
		
		MQuery tempqry6 = MQuery.queryThesePolicies("(forSome ipfrom IPAddress (forSome ipto IPAddress (and (not (parent:Permit ipfrom ipto)) (child1:Permit ipfrom ipto) (not (child2:AlertAdmin ipfrom ipto)))))", pollist);
		countTest("child1 permit not echoed by parent only when child2 alertadmins", tempqry6, 4, 0, 4);	
	
		// ******************************************************************
		// Custom IDB Testing
		// ******************************************************************
		
		// (Use uppercase to make sure it's safely lowered by the constructor)
		List<String> varorder = new ArrayList<String>();
		varorder.add("x");

		// UNARY predicate, no dependencies
		MCustomIDB cust1 = new MCustomIDB(polparent.vocab, "Custom1", new ArrayList<MIDBCollection>(), varorder, "(and (rangexy x) (not (IPAddress x)))");
		
		ArrayList<MIDBCollection> idbCollections = new ArrayList<MIDBCollection>();
		idbCollections.add(cust1);
		tempqry = MQuery.queryThesePolicies("(forsome ip IPAddress (Custom1:view ip))", idbCollections);
		// 3 due to non-emptiness constraints in vocab. 0 due to subset contradiction
		countTest("Custom IDB 1", tempqry, 3, 0, 3); 
		
		ArrayList<MIDBCollection> others = new ArrayList<MIDBCollection>();
		others.add(polparent);
		varorder.add("y");
		
		// BINARY predicate, one policy dependency 
		MCustomIDB cust2 = new MCustomIDB(polparent.vocab, "Custom2", others, varorder, "(and (parent:alertadmin x y) (local y) (forall z IPAddress (not (rangexy z))))");

		idbCollections.clear();
		idbCollections.add(cust2);
		idbCollections.add(polparent); // so the query knows the policy constraints and such
		tempqry = MQuery.queryThesePolicies("(forsome ipin IPAddress (forsome ipout IPAddress (and (Custom2:view ipin ipout) (EMPTY Hackerlist) (EMPTY Conversation))))", idbCollections);
		countTest("Custom IDB 2", tempqry, -1, 14, 4); // 9 total (including superfluous size=4) + 5 using IPAddress generally
		
		
		
		// BINARY predicate, dependency on policy and another view.
		// overlapping variable names; quantification within IDB
		MCustomIDB cust3 = new MCustomIDB(polparent.vocab, "Custom3", idbCollections, varorder, "(forsome z ipaddress (and (parent:alertadmin y z) (Custom2:view z y)))");
		idbCollections.add(cust3);		
		
		// Just make sure the query can be created. 
		MQuery.queryThesePolicies("(forsome ipin IPAddress (forsome ipout IPAddress (and (Custom3:view ipin ipout) (EMPTY Hackerlist) (EMPTY Conversation))))", idbCollections);
		
		//query3.prettyPrintSolutions();
		
		// Tests to make sure predicates are being taken from BOTH vocabs when combining
		// Create a 2nd vocabulary that is missing one of the above predicates, but has a third.
		// Then run a change-impact on policies over both... should see all 3 predicates in new vocab.
		MVocab env2 = new MVocab("Vocab with different preds");		
		
		env2.addDecision("Permit");
		env2.addDecision("Deny");
		env2.addDecision("AlertAdmin");
		
		env2.addSort("IPAddress");
		
		env2.addSubSort("IPAddress", "Local");
		env2.addSubSort("IPAddress", "External");

		env2.addSubSort("IPAddress", "rangexy");

		env2.axioms.addConstraintDisjoint("Local", "External");		
		env2.axioms.addConstraintNonempty("IPAddress");
		env2.axioms.addConstraintNonempty("Local");
		env2.axioms.addConstraintNonempty("External");
		
		env2.addRequestVar("ipfrom", "IPAddress");
		env2.addRequestVar("ipto", "IPAddress");		
				
		env2.addPredicate("WhiteList", "IPAddress"); 		
		env2.addPredicate("Conversation", "IPAddress IPAddress");
		
		MPolicyLeaf poldifferent = new MPolicyLeaf("diff-parent", env2);
				
		templist.clear();		
		templist.add("Conversation ipfrom ipto");
		poldifferent.addRule("If conversation in progress between addrs", "Permit", templist);				
		templist.clear();		
		templist.add("Local ipfrom");
		templist.add("Local ipto");		
		poldifferent.addRule("Both local", "AlertAdmin", templist);				
		templist.clear();		
		templist.add("External ipfrom");
		templist.add("External ipto");		
		poldifferent.addRule("Both external", "AlertAdmin", templist);							
		templist.clear();		
		templist.add("Whitelist ipfrom");		
		poldifferent.addRule("Whitelist", "Permit", templist);							
		
		poldifferent.rCombine = "O AlertAdmin Deny Permit";	
		poldifferent.initIDBs();		
		
		// Will try to combine. 
		MQuery diffQuery1 = poldifferent.compareWithPolicy(polparent);
		MQuery diffQuery2 = polparent.compareWithPolicy(poldifferent);
		
		// Was combination successful? this will throw an exception if not.
		diffQuery1.isQuerySatisfiable();
		diffQuery2.isQuerySatisfiable();
		
		
	}
	
	
	static void do_test_5() 
	throws MGEBadIdentifierName, MGEUnknownIdentifier, MGEArityMismatch, MGEBadCombinator, 
	MGEBadIdentifierName, MGEBadQueryString, MGEUnsortedVariable, MGECombineVocabs, MGEManagerException
	{

		
		
		// test for first-applicable policy combination
		MVocab env = new MVocab("Vocab");		
		
		env.addDecision("Accept");
		env.addDecision("Drop");
		
		env.addSort("IPAddress");
		
		env.addSubSort("IPAddress", "Local");
		env.addSubSort("IPAddress", "External");

		env.addSubSort("IPAddress", "rangexy");

		env.axioms.addConstraintDisjoint("Local", "External");
		env.axioms.addConstraintDisjoint("Local", "rangexy");	
		env.axioms.addConstraintNonempty("IPAddress");			
			
		env.addRequestVar("ipfrom", "IPAddress");
		env.addRequestVar("ipto", "IPAddress");	
			
		env.addPredicate("Connection", "IPAddress IPAddress"); 
		
		MPolicyLeaf pol = new MPolicyLeaf("pol", env);

		ArrayList<String> templist = new ArrayList<String>();
		templist.clear();		
		templist.add("Local ipfrom");
		pol.addRule("Permit local src address", "Accept", templist);				
		pol.rCombine = "O Drop Accept";	

		pol.assumptions.addOtherConstraint("(forAll ipone IPAddress (forAll iptwo IPAddress (or (not (Connection ipone iptwo)) (pol:Accept ipone iptwo))))");

		pol.initIDBs(); // must init after changing assumptions!
		//pol.prettyPrintIDBs();
		MQuery tempqry = pol.queryPolicy("(forSome ipfrom IPAddress (forSome ipto IPAddress (and (pol:Accept ipfrom ipto) (not (= ipfrom ipto)))))");	
		countTest("Custom policy-level constraint", tempqry, 2, 32, 2);	
		
		tempqry = pol.queryPolicy("(forSome ipfrom IPAddress (forSome ipto IPAddress (and (pol:Accept ipfrom ipto) (not (= ipfrom ipto)) (ONE Connection))))");
		countTest("Custom policy-level constraint and ONE on state predicate", tempqry, 2, 12, 2);
		
		tempqry = pol.queryPolicy("(forSome ipfrom IPAddress (forSome ipto IPAddress (and (pol:Accept ipfrom ipto) (not (= ipfrom ipto)) (EMPTY Connection))))");
		countTest("Custom policy-level constraint and EMPTY on state predicate", tempqry, 2, 5, 2); // 4 +1 other ip
			
	}
	
	public static void runTests()
	  throws MGEUnknownIdentifier, MGEArityMismatch,
	         MGEBadCombinator, MGECombineVocabs, MGEUnsortedVariable, MGEBadQueryString, MGEBadIdentifierName, MGEManagerException
	{		
		
		RelationAndVariableReplacementV.unitTests();
		
		MQuery.unitTest();
			

		System.out.println("");
		System.out.println("ENTERING TEST BLOCK: do_test_1()");
		do_test_1();
		
		/*System.gc();
		MGFormulaManager.ping();
		MGFormulaManager.printStatistics();
		System.exit(1);*/
		
		System.out.println("");
		System.out.println("ENTERING TEST BLOCK: do_test_2()");
		do_test_2();	
		
		System.out.println("");
		System.out.println("ENTERING TEST BLOCK: do_test_3()");
		do_test_3();	
		
		// policy combination
		System.out.println("");
		System.out.println("ENTERING TEST BLOCK: do_test_4()");
		do_test_4(); 
		
		// policy-level constraints
		System.out.println("");
		System.out.println("ENTERING TEST BLOCK: do_test_5()");
		do_test_5();
		
	
	
		// Tupling tests
		do_test_tupling_1();

	/*	
		Set<Formula> a = new HashSet<Formula>();
		Set<Formula> b = new HashSet<Formula>();
		
		a.add(Formula.TRUE);
		b.add(Formula.TRUE);
		System.out.println(a.equals(b));
		System.out.println(a ==b);
		
		Variable v = Variable.unary("v");
		Relation r = Relation.unary("R");
		Relation p = Relation.unary("P");
		System.out.println(v.in(r).equals(v.in(r))); // so kk uses reference equality
		// likely because computing isomorphism is expensive
		
		HashMap<Set<Formula>, Formula> hm = new HashMap<Set<Formula>, Formula>();
		hm.put(a, Formula.TRUE);
		System.out.println(hm.containsKey(b)); // yay!
		
		ArrayList<Formula> foo = new ArrayList<Formula>();
		ArrayList<Formula> bar = new ArrayList<Formula>();
		foo.add(Formula.TRUE);
		foo.add(Formula.FALSE);
		bar.add(Formula.TRUE);
		bar.add(Formula.FALSE);
		
		System.out.println(foo.equals(bar)); // YAY!
		// Lists are equal if they have the same (equal) elements in the same order
		
		*/
		
		
		

	}
	
	/*static void cuddStyleOutput(BDD bdd)
	{
		for(int iMarker = 0; iMarker < bdd.getFactory().varNum(); iMarker++)
			System.out.print(iMarker % 10);
		System.out.print("\n");
		
		for(byte[] x : (List<byte[]>)bdd.allsat())
		{	
			// Bug in interface to native BuDDY: List size is same as # vars?
			if(x == null)
			{
				System.out.print("?");
				continue;
			}
			
		
			for(byte b : x)
			{
				if(b == -1)
					System.out.print("-");
				else if(b > -1)
					System.out.print(b);
				else
					System.out.print("?");
			}
			System.out.print("\n");
		}
		
	}
	
	static BDD BDD_Not(BDD b)
	{
		// The test case used to be C code.
		return b.not();
	}
	
	static BDD addToConjunction(BDD x, BDD y)
	{
		return x.and(y);
	}
	
	static BDD makeConjunctionFromLeaves(BDD x, BDD y)
	{
		return x.and(y);
	}
	
	static void testbdd()
	{
		// what are good parameters?
		
		
		// 1 bug in buddy interface
		// 2 reordering -- is it happening? test
		
		
		// allsat seems to work ok...
		BDDFactory factory = JFactory.init(3000, 10000);
		
		// allsat is not supported :(
		//BDDFactory factory = CUDDFactory.init(1000, 1000);
		
		// bug in allsat -- wrong num of solns in list and giving nulls!
		//BDDFactory factory = BuDDyFactory.init(1000, 1000);
		factory.setVarNum(39);
		
		// Create variables (defined by the vocab and model size, essentially.)
		BDD p0isS = factory.ithVar(0);
		BDD p0isA = factory.ithVar(1);
		BDD p0isR = factory.ithVar(2);
        BDD p1isS = factory.ithVar(3);
        BDD p1isA = factory.ithVar(4);
        BDD p1isR = factory.ithVar(5);
        BDD p2isS = factory.ithVar(6);
        BDD p2isA = factory.ithVar(7);
        BDD p2isR = factory.ithVar(8);

		BDD p0isAuthor = factory.ithVar(9); 
	    BDD p0isReviewer = factory.ithVar(10);
	    BDD p0isPaper = factory.ithVar(11);
	    BDD p0isReview = factory.ithVar(12);
	    BDD p0isSubmitReview = factory.ithVar(13);
	    BDD p0isReadPaper = factory.ithVar(14);

	        BDD p1isAuthor = factory.ithVar(15);
	        BDD p1isReviewer = factory.ithVar(16);
	        BDD p1isPaper = factory.ithVar(17);
	        BDD p1isReview = factory.ithVar(18);
	        BDD p1isSubmitReview = factory.ithVar(19);
	        BDD p1isReadPaper = factory.ithVar(20);

	        BDD p2isAuthor = factory.ithVar(21);
	        BDD p2isReviewer = factory.ithVar(22);
	        BDD p2isPaper = factory.ithVar(23);
	        BDD p2isReview = factory.ithVar(24);
	        BDD p2isSubmitReview = factory.ithVar(25);
	        BDD p2isReadPaper = factory.ithVar(26);

	        BDD p00isConflicted = factory.ithVar(27);
	        BDD p01isConflicted = factory.ithVar(28);
	        BDD p02isConflicted = factory.ithVar(29);
	        BDD p11isConflicted = factory.ithVar(30);
	        BDD p12isConflicted = factory.ithVar(31);
	        BDD p22isConflicted = factory.ithVar(32);

	        BDD p00isAssigned = factory.ithVar(33);
	        BDD p01isAssigned = factory.ithVar(34);
	        BDD p02isAssigned = factory.ithVar(35);
	        BDD p11isAssigned = factory.ithVar(36);
	        BDD p12isAssigned = factory.ithVar(37);
	        BDD p22isAssigned = factory.ithVar(38);

	        BDD sol1 = makeConjunctionFromLeaves(BDD_Not(p0isR), p0isS);
	    	sol1 = addToConjunction(sol1, BDD_Not(p0isA));
	    	sol1 = addToConjunction(sol1, BDD_Not(p1isS));
	    	sol1 = addToConjunction(sol1, p1isR);
	    	sol1 = addToConjunction(sol1, BDD_Not(p1isA));
	    	sol1 = addToConjunction(sol1, BDD_Not(p2isS));
	    	sol1 = addToConjunction(sol1, p2isA);
	    	sol1 = addToConjunction(sol1, BDD_Not(p2isR));
	    	sol1 = addToConjunction(sol1, p0isAuthor);
	    	sol1 = addToConjunction(sol1, p0isReviewer); 
	    	sol1 = addToConjunction(sol1, BDD_Not(p0isPaper));
	    	sol1 = addToConjunction(sol1, BDD_Not(p0isSubmitReview));
	    	sol1 = addToConjunction(sol1, BDD_Not(p0isReadPaper)); 
	    	sol1 = addToConjunction(sol1, BDD_Not(p0isReview));
	    	sol1 = addToConjunction(sol1, BDD_Not(p1isAuthor));
	    	sol1 = addToConjunction(sol1, BDD_Not(p1isReviewer));
	    	sol1 = addToConjunction(sol1, p1isPaper);
	    	sol1 = addToConjunction(sol1, BDD_Not(p1isSubmitReview));
	    	sol1 = addToConjunction(sol1, BDD_Not(p1isReadPaper));
	    	sol1 = addToConjunction(sol1, BDD_Not(p1isReview));
	    	sol1 = addToConjunction(sol1, BDD_Not(p2isAuthor)); 
	    	sol1 = addToConjunction(sol1, BDD_Not(p2isReviewer)); 
	    	sol1 = addToConjunction(sol1, BDD_Not(p2isPaper));
	    	sol1 = addToConjunction(sol1, BDD_Not(p2isSubmitReview));
	    	sol1 = addToConjunction(sol1, p2isReadPaper);
	    	sol1 = addToConjunction(sol1, BDD_Not(p2isReview));
	    	sol1 = addToConjunction(sol1, BDD_Not(p00isConflicted)); 
	    	sol1 = addToConjunction(sol1, p01isConflicted);
	    	sol1 = addToConjunction(sol1, BDD_Not(p02isConflicted));
	    	sol1 = addToConjunction(sol1, BDD_Not(p11isConflicted));
	    	sol1 = addToConjunction(sol1, BDD_Not(p12isConflicted));
	    	sol1 = addToConjunction(sol1, BDD_Not(p22isConflicted));
	    	sol1 = addToConjunction(sol1, BDD_Not(p00isAssigned));
	    	sol1 = addToConjunction(sol1, p01isAssigned);
	    	sol1 = addToConjunction(sol1, BDD_Not(p02isAssigned));
	    	sol1 = addToConjunction(sol1, BDD_Not(p11isAssigned));
	    	sol1 = addToConjunction(sol1, BDD_Not(p12isAssigned));
	    	sol1 = addToConjunction(sol1, BDD_Not(p22isAssigned));
	    	


	    	BDD sol2 = makeConjunctionFromLeaves(BDD_Not(p0isR), p0isS);
	    	sol2 = addToConjunction(sol2, BDD_Not(p0isA));
	    	sol2 = addToConjunction(sol2, BDD_Not(p1isS));
	    	sol2 = addToConjunction(sol2, p1isR);
	    	sol2 = addToConjunction(sol2, BDD_Not(p1isA));
	    	sol2 = addToConjunction(sol2, BDD_Not(p2isS));
	    	sol2 = addToConjunction(sol2, p2isA);
	    	sol2 = addToConjunction(sol2, BDD_Not(p2isR));
	    	sol2 = addToConjunction(sol2, BDD_Not(p0isAuthor));
	    	sol2 = addToConjunction(sol2, p0isReviewer);
	    	sol2 = addToConjunction(sol2, BDD_Not(p0isPaper));
	    	sol2 = addToConjunction(sol2, BDD_Not(p0isSubmitReview));
	    	sol2 = addToConjunction(sol2, BDD_Not(p0isReadPaper));
	    	sol2 = addToConjunction(sol2, BDD_Not(p0isReview));
	    	sol2 = addToConjunction(sol2, BDD_Not(p1isAuthor));
	    	sol2 = addToConjunction(sol2, BDD_Not(p1isReviewer));
	    	sol2 = addToConjunction(sol2, p1isPaper);
	    	sol2 = addToConjunction(sol2, BDD_Not(p1isSubmitReview));
	    	sol2 = addToConjunction(sol2, BDD_Not(p1isReadPaper));
	    	sol2 = addToConjunction(sol2, BDD_Not(p1isReview));
	    	sol2 = addToConjunction(sol2, BDD_Not(p2isAuthor));
	    	sol2 = addToConjunction(sol2, BDD_Not(p2isReviewer));
	    	sol2 = addToConjunction(sol2, BDD_Not(p2isPaper));
	    	sol2 = addToConjunction(sol2, BDD_Not(p2isSubmitReview));
	    	sol2 = addToConjunction(sol2, p2isReadPaper);
	    	sol2 = addToConjunction(sol2, BDD_Not(p2isReview));
	    	sol2 = addToConjunction(sol2, BDD_Not(p00isConflicted));
	    	sol2 = addToConjunction(sol2, p01isConflicted);
	    	sol2 = addToConjunction(sol2, BDD_Not(p02isConflicted));
	    	sol2 = addToConjunction(sol2, BDD_Not(p11isConflicted));
	    	sol2 = addToConjunction(sol2, BDD_Not(p12isConflicted));
	    	sol2 = addToConjunction(sol2, BDD_Not(p22isConflicted));
	    	sol2 = addToConjunction(sol2, BDD_Not(p00isAssigned));
	    	sol2 = addToConjunction(sol2, p01isAssigned);
	    	sol2 = addToConjunction(sol2, BDD_Not(p02isAssigned));
	    	sol2 = addToConjunction(sol2, BDD_Not(p11isAssigned));
	    	sol2 = addToConjunction(sol2, BDD_Not(p12isAssigned));
	    	sol2 = addToConjunction(sol2, BDD_Not(p22isAssigned));



	            BDD sol3 = makeConjunctionFromLeaves(BDD_Not(p0isR), p0isS);
	    	sol3 = addToConjunction(sol3, BDD_Not(p0isA));
	    	sol3 = addToConjunction(sol3, BDD_Not(p1isS));
	    	sol3 = addToConjunction(sol3, p1isR);
	    	sol3 = addToConjunction(sol3, BDD_Not(p1isA));
	    	sol3 = addToConjunction(sol3, BDD_Not(p2isS));
	    	sol3 = addToConjunction(sol3, p2isA);
	    	sol3 = addToConjunction(sol3, BDD_Not(p2isR));
	    	sol3 = addToConjunction(sol3, p0isAuthor);
	    	sol3 = addToConjunction(sol3, BDD_Not(p0isReviewer));
	    	sol3 = addToConjunction(sol3, BDD_Not(p0isPaper));
	    	sol3 = addToConjunction(sol3, BDD_Not(p0isSubmitReview));
	    	sol3 = addToConjunction(sol3, BDD_Not(p0isReadPaper));
	    	sol3 = addToConjunction(sol3, BDD_Not(p0isReview));
	    	sol3 = addToConjunction(sol3, BDD_Not(p1isAuthor));
	    	sol3 = addToConjunction(sol3, BDD_Not(p1isReviewer));
	    	sol3 = addToConjunction(sol3, p1isPaper);
	    	sol3 = addToConjunction(sol3, BDD_Not(p1isSubmitReview));
	    	sol3 = addToConjunction(sol3, BDD_Not(p1isReadPaper));
	    	sol3 = addToConjunction(sol3, BDD_Not(p1isReview));
	    	sol3 = addToConjunction(sol3, BDD_Not(p2isAuthor));
	    	sol3 = addToConjunction(sol3, BDD_Not(p2isReviewer));
	    	sol3 = addToConjunction(sol3, BDD_Not(p2isPaper));
	    	sol3 = addToConjunction(sol3, BDD_Not(p2isSubmitReview));
	    	sol3 = addToConjunction(sol3, p2isReadPaper);
	    	sol3 = addToConjunction(sol3, BDD_Not(p2isReview));
	    	sol3 = addToConjunction(sol3, BDD_Not(p00isConflicted));
	    	sol3 = addToConjunction(sol3, BDD_Not(p01isConflicted));
	    	sol3 = addToConjunction(sol3, BDD_Not(p02isConflicted));
	    	sol3 = addToConjunction(sol3, BDD_Not(p11isConflicted));
	    	sol3 = addToConjunction(sol3, BDD_Not(p12isConflicted));
	    	sol3 = addToConjunction(sol3, BDD_Not(p22isConflicted));
	    	sol3 = addToConjunction(sol3, BDD_Not(p00isAssigned));
	    	sol3 = addToConjunction(sol3, BDD_Not(p01isAssigned));
	    	sol3 = addToConjunction(sol3, BDD_Not(p02isAssigned));
	    	sol3 = addToConjunction(sol3, BDD_Not(p11isAssigned));
	    	sol3 = addToConjunction(sol3, BDD_Not(p12isAssigned));
	    	sol3 = addToConjunction(sol3, BDD_Not(p22isAssigned));

	            BDD sol4 = makeConjunctionFromLeaves(BDD_Not(p0isR), p0isS);
	            sol4 = addToConjunction(sol4, BDD_Not(p0isA));
	            sol4 = addToConjunction(sol4, BDD_Not(p1isS));
	            sol4 = addToConjunction(sol4, p1isR);
	            sol4 = addToConjunction(sol4, BDD_Not(p1isA));
	            sol4 = addToConjunction(sol4, BDD_Not(p2isS));
	            sol4 = addToConjunction(sol4, p2isA);
	            sol4 = addToConjunction(sol4, BDD_Not(p2isR));
	            sol4 = addToConjunction(sol4, p0isAuthor);
	            sol4 = addToConjunction(sol4, p0isReviewer);
	            sol4 = addToConjunction(sol4, BDD_Not(p0isPaper));
	            sol4 = addToConjunction(sol4, BDD_Not(p0isSubmitReview));
	            sol4 = addToConjunction(sol4, BDD_Not(p0isReadPaper));
	            sol4 = addToConjunction(sol4, BDD_Not(p0isReview));
	            sol4 = addToConjunction(sol4, BDD_Not(p1isAuthor));
	            sol4 = addToConjunction(sol4, BDD_Not(p1isReviewer));
	            sol4 = addToConjunction(sol4, p1isPaper);
	            sol4 = addToConjunction(sol4, BDD_Not(p1isSubmitReview));
	            sol4 = addToConjunction(sol4, BDD_Not(p1isReadPaper));
	            sol4 = addToConjunction(sol4, BDD_Not(p1isReview));
	            sol4 = addToConjunction(sol4, BDD_Not(p2isAuthor));
	            sol4 = addToConjunction(sol4, BDD_Not(p2isReviewer));
	            sol4 = addToConjunction(sol4, BDD_Not(p2isPaper));
	            sol4 = addToConjunction(sol4, BDD_Not(p2isSubmitReview));
	            sol4 = addToConjunction(sol4, p2isReadPaper);
	            sol4 = addToConjunction(sol4, BDD_Not(p2isReview));
	            sol4 = addToConjunction(sol4, BDD_Not(p00isConflicted));
	            sol4 = addToConjunction(sol4, BDD_Not(p01isConflicted));
	            sol4 = addToConjunction(sol4, BDD_Not(p02isConflicted));
	            sol4 = addToConjunction(sol4, BDD_Not(p11isConflicted));
	            sol4 = addToConjunction(sol4, BDD_Not(p12isConflicted));
	            sol4 = addToConjunction(sol4, BDD_Not(p22isConflicted));
	            sol4 = addToConjunction(sol4, BDD_Not(p00isAssigned));
	            sol4 = addToConjunction(sol4, p01isAssigned);
	            sol4 = addToConjunction(sol4, BDD_Not(p02isAssigned));
	            sol4 = addToConjunction(sol4, BDD_Not(p11isAssigned));
	            sol4 = addToConjunction(sol4, BDD_Not(p12isAssigned));
	            sol4 = addToConjunction(sol4, BDD_Not(p22isAssigned));

	            BDD sol5 = makeConjunctionFromLeaves(BDD_Not(p0isR), p0isS);
	            sol5 = addToConjunction(sol5, BDD_Not(p0isA));
	            sol5 = addToConjunction(sol5, BDD_Not(p1isS));
	            sol5 = addToConjunction(sol5, p1isR);
	            sol5 = addToConjunction(sol5, BDD_Not(p1isA));
	            sol5 = addToConjunction(sol5, BDD_Not(p2isS));
	            sol5 = addToConjunction(sol5, p2isA);
	            sol5 = addToConjunction(sol5, BDD_Not(p2isR));
	            sol5 = addToConjunction(sol5, BDD_Not(p0isAuthor));
	            sol5 = addToConjunction(sol5, p0isReviewer);
	            sol5 = addToConjunction(sol5, BDD_Not(p0isPaper));
	            sol5 = addToConjunction(sol5, BDD_Not(p0isSubmitReview));
	            sol5 = addToConjunction(sol5, BDD_Not(p0isReadPaper));
	            sol5 = addToConjunction(sol5, BDD_Not(p0isReview));
	            sol5 = addToConjunction(sol5, BDD_Not(p1isAuthor));
	            sol5 = addToConjunction(sol5, BDD_Not(p1isReviewer));
	            sol5 = addToConjunction(sol5, p1isPaper);
	            sol5 = addToConjunction(sol5, BDD_Not(p1isSubmitReview));
	            sol5 = addToConjunction(sol5, BDD_Not(p1isReadPaper));
	            sol5 = addToConjunction(sol5, BDD_Not(p1isReview));
	            sol5 = addToConjunction(sol5, BDD_Not(p2isAuthor));
	            sol5 = addToConjunction(sol5, BDD_Not(p2isReviewer));
	            sol5 = addToConjunction(sol5, BDD_Not(p2isPaper));
	            sol5 = addToConjunction(sol5, BDD_Not(p2isSubmitReview));
	            sol5 = addToConjunction(sol5, p2isReadPaper);
	            sol5 = addToConjunction(sol5, BDD_Not(p2isReview));
	            sol5 = addToConjunction(sol5, BDD_Not(p00isConflicted));
	            sol5 = addToConjunction(sol5, BDD_Not(p01isConflicted));
	            sol5 = addToConjunction(sol5, BDD_Not(p02isConflicted));
	            sol5 = addToConjunction(sol5, BDD_Not(p11isConflicted));
	            sol5 = addToConjunction(sol5, BDD_Not(p12isConflicted));
	            sol5 = addToConjunction(sol5, BDD_Not(p22isConflicted));
	            sol5 = addToConjunction(sol5, BDD_Not(p00isAssigned));
	            sol5 = addToConjunction(sol5, p01isAssigned);
	            sol5 = addToConjunction(sol5, BDD_Not(p02isAssigned));
	            sol5 = addToConjunction(sol5, BDD_Not(p11isAssigned));
	            sol5 = addToConjunction(sol5, BDD_Not(p12isAssigned));
	            sol5 = addToConjunction(sol5, BDD_Not(p22isAssigned));


	            BDD sol6 = makeConjunctionFromLeaves(BDD_Not(p0isR), p0isS);
	            sol6 = addToConjunction(sol6, BDD_Not(p0isA));
	            sol6 = addToConjunction(sol6, BDD_Not(p1isS));
	            sol6 = addToConjunction(sol6, p1isR);
	            sol6 = addToConjunction(sol6, BDD_Not(p1isA));
	            sol6 = addToConjunction(sol6, BDD_Not(p2isS));
	            sol6 = addToConjunction(sol6, p2isA);
	            sol6 = addToConjunction(sol6, BDD_Not(p2isR));
	            sol6 = addToConjunction(sol6, BDD_Not(p0isAuthor));
	            sol6 = addToConjunction(sol6, p0isReviewer);
	            sol6 = addToConjunction(sol6, BDD_Not(p0isPaper));
	            sol6 = addToConjunction(sol6, BDD_Not(p0isSubmitReview));
	            sol6 = addToConjunction(sol6, BDD_Not(p0isReadPaper));
	            sol6 = addToConjunction(sol6, BDD_Not(p0isReview));
	            sol6 = addToConjunction(sol6, BDD_Not(p1isAuthor));
	            sol6 = addToConjunction(sol6, BDD_Not(p1isReviewer));
	            sol6 = addToConjunction(sol6, p1isPaper);
	            sol6 = addToConjunction(sol6, BDD_Not(p1isSubmitReview));
	            sol6 = addToConjunction(sol6, BDD_Not(p1isReadPaper));
	            sol6 = addToConjunction(sol6, BDD_Not(p1isReview));
	            sol6 = addToConjunction(sol6, BDD_Not(p2isAuthor));
	            sol6 = addToConjunction(sol6, BDD_Not(p2isReviewer));
	            sol6 = addToConjunction(sol6, BDD_Not(p2isPaper));
	            sol6 = addToConjunction(sol6, BDD_Not(p2isSubmitReview));
	            sol6 = addToConjunction(sol6, p2isReadPaper);
	            sol6 = addToConjunction(sol6, BDD_Not(p2isReview));
	            sol6 = addToConjunction(sol6, BDD_Not(p00isConflicted));
	            sol6 = addToConjunction(sol6, BDD_Not(p01isConflicted));
	            sol6 = addToConjunction(sol6, BDD_Not(p02isConflicted));
	            sol6 = addToConjunction(sol6, BDD_Not(p11isConflicted));
	            sol6 = addToConjunction(sol6, BDD_Not(p12isConflicted));
	            sol6 = addToConjunction(sol6, BDD_Not(p22isConflicted));
	            sol6 = addToConjunction(sol6, BDD_Not(p00isAssigned));
	            sol6 = addToConjunction(sol6, BDD_Not(p01isAssigned));
	            sol6 = addToConjunction(sol6, BDD_Not(p02isAssigned));
	            sol6 = addToConjunction(sol6, BDD_Not(p11isAssigned));
	            sol6 = addToConjunction(sol6, BDD_Not(p12isAssigned));
	            sol6 = addToConjunction(sol6, BDD_Not(p22isAssigned));

	            BDD sol7 = makeConjunctionFromLeaves(BDD_Not(p0isR), p0isS);
	            sol7 = addToConjunction(sol7, BDD_Not(p0isA));
	            sol7 = addToConjunction(sol7, BDD_Not(p1isS));
	            sol7 = addToConjunction(sol7, p1isR);
	            sol7 = addToConjunction(sol7, BDD_Not(p1isA));
	            sol7 = addToConjunction(sol7, BDD_Not(p2isS));
	            sol7 = addToConjunction(sol7, p2isA);
	            sol7 = addToConjunction(sol7, BDD_Not(p2isR));
	            sol7 = addToConjunction(sol7, p0isAuthor);
	            sol7 = addToConjunction(sol7, p0isReviewer);
	            sol7 = addToConjunction(sol7, BDD_Not(p0isPaper));
	            sol7 = addToConjunction(sol7, BDD_Not(p0isSubmitReview));
	            sol7 = addToConjunction(sol7, BDD_Not(p0isReadPaper));
	            sol7 = addToConjunction(sol7, BDD_Not(p0isReview));
	            sol7 = addToConjunction(sol7, BDD_Not(p1isAuthor));
	            sol7 = addToConjunction(sol7, BDD_Not(p1isReviewer));
	            sol7 = addToConjunction(sol7, p1isPaper);
	            sol7 = addToConjunction(sol7, BDD_Not(p1isSubmitReview));
	            sol7 = addToConjunction(sol7, BDD_Not(p1isReadPaper));
	            sol7 = addToConjunction(sol7, BDD_Not(p1isReview));
	            sol7 = addToConjunction(sol7, BDD_Not(p2isAuthor));
	            sol7 = addToConjunction(sol7, BDD_Not(p2isReviewer));
	            sol7 = addToConjunction(sol7, BDD_Not(p2isPaper));
	            sol7 = addToConjunction(sol7, BDD_Not(p2isSubmitReview));
	            sol7 = addToConjunction(sol7, p2isReadPaper);
	            sol7 = addToConjunction(sol7, BDD_Not(p2isReview));
	            sol7 = addToConjunction(sol7, BDD_Not(p00isConflicted));
	            sol7 = addToConjunction(sol7, BDD_Not(p01isConflicted));
	            sol7 = addToConjunction(sol7, BDD_Not(p02isConflicted));
	            sol7 = addToConjunction(sol7, BDD_Not(p11isConflicted));
	            sol7 = addToConjunction(sol7, BDD_Not(p12isConflicted));
	            sol7 = addToConjunction(sol7, BDD_Not(p22isConflicted));
	            sol7 = addToConjunction(sol7, BDD_Not(p00isAssigned));
	            sol7 = addToConjunction(sol7, BDD_Not(p01isAssigned));
	            sol7 = addToConjunction(sol7, BDD_Not(p02isAssigned));
	            sol7 = addToConjunction(sol7, BDD_Not(p11isAssigned));
	            sol7 = addToConjunction(sol7, BDD_Not(p12isAssigned));
	            sol7 = addToConjunction(sol7, BDD_Not(p22isAssigned));

		
		BDD thebdd = sol1.or(sol2).or(sol3).or(sol4).or(sol5).or(sol6).or(sol7);
		
		
		cuddStyleOutput(thebdd);
		//thebdd.printDot();

		// Remove variables that are always constants.
		
		thebdd = thebdd.restrict(factory.ithVar(0).and(factory.nithVar(1))
				.and(factory.nithVar(2))
				.and(factory.nithVar(3))
				.and(factory.nithVar(4))
				.and(factory.ithVar(5))
				.and(factory.nithVar(6))
				.and(factory.ithVar(7))
				.and(factory.nithVar(8))
		.and(factory.nithVar(11))
		.and(factory.nithVar(12))
		.and(factory.nithVar(13))
		.and(factory.nithVar(14))
		.and(factory.nithVar(15))
		.and(factory.nithVar(16))
		.and(factory.ithVar(17))
		.and(factory.nithVar(18))
		.and(factory.nithVar(19))
		.and(factory.nithVar(20))
		.and(factory.nithVar(21))
		.and(factory.nithVar(22))
		.and(factory.nithVar(23))
		.and(factory.nithVar(24))
		.and(factory.nithVar(25))
		.and(factory.ithVar(26))
		.and(factory.nithVar(27))
		
		.and(factory.nithVar(29))
		.and(factory.nithVar(30))
		.and(factory.nithVar(31))
		.and(factory.nithVar(32))
		.and(factory.nithVar(33))	
		.and(factory.nithVar(35))
		.and(factory.nithVar(36))
		.and(factory.nithVar(37))
		.and(factory.nithVar(38))	
		
		);
		// 9 & 10 differ
		// 11-16 are constant 0
		// 17 constant 1
		// next 8 (18-25) constant 0
		// 26 is constant 1
		// 27 is constant 0
		// 28 varies
		// 29-33 constant 0
		// 34 varies (a lot)
		// 35-38 constant 0
		
		// 9 10 28 34
		// swapping 9 & 10 is the key to getting 3 solns
		
		// Getting stuck on local minimum, here.
		
		//cuddStyleOutput(thebdd);
		factory.reorder(BDDFactory.REORDER_SIFT);
		thebdd.getFactory().reorder(BDDFactory.REORDER_WIN3ITE);
		thebdd.getFactory().reorder(BDDFactory.REORDER_RANDOM);
		
		cuddStyleOutput(thebdd);
				
		factory.done();
		
		

	}*/
	
	public static void addEasyRulesNew(MPolicyLeaf pol, MPolicyLeaf pol2, int numRules)
	throws MGEBadIdentifierName, MGEUnknownIdentifier, MGEArityMismatch, 
	MGEBadQueryString, MGEBadCombinator, MGECombineVocabs, MGEUnsortedVariable
	{
		ArrayList<String> templist = new ArrayList<String>();

		
		for(int ii=0;ii<numRules;ii++)
		{
			templist.clear();
			templist.add("FastEthernet0 entry-interface");
			templist.add("prot-TCP prot");

			String decision = "Accept";
			if(ii % 2 == 0) decision = "Drop";
			
			// this doesnt make sense anymore
			//templist.set(2, "IP"+ii+" src-addr");	 	

			// Now we have to enumerate the true and false bits for the IP we are interested in.
			// (This is in fact the main problem with converting to this metaphor. The important
			//  part is to show it may be worthwhile.)
			
			for(int ibit = 0; ibit < 32 ; ibit++)
			{
				// we want ip # ii
				
				long mod = (long)Math.pow(2, ibit);
								
				if((ii & mod) != 0)
					templist.add("IPBit"+ibit+" src-addr");
				else
					templist.add("!IPBit"+ibit+" src-addr");
				
			}
			
			//System.out.println(templist);
			
			pol.addRule("EasyRule"+ii, decision, templist);
			pol2.addRule("EasyRule"+ii, decision, templist);
			
			
		}		
	}

	
	public static MVocab setupFirewallVocabNew(String name)
	throws MGEBadIdentifierName, MGEUnknownIdentifier, MGEArityMismatch, 
	MGEBadQueryString, MGEBadCombinator, MGECombineVocabs, MGEUnsortedVariable
	{
		MVocab env = new MVocab(name);
		
		// No need to have IP Ranges or number of ports -- just a standard firewall vocab.
		env.addDecision("Accept");
		env.addDecision("Drop");
		
		String ipbits = "";
		for(int ii = 0;ii<32;ii++)
			ipbits += "IPBit"+ii+" ";
		String portbits = "";
		for(int ii = 0;ii<16;ii++)
			ipbits += "PortBit"+ii+" ";
		
		env.addSort("IPAddress", ipbits);
		env.addSort("Port", portbits);
		
		env.addSort("Protocol", "prot-ICMP prot-TCP prot-UDP");
		env.addSort("ICMPMessage", "icmp-N/A icmp-any");
		env.addSort("icmp-any", "icmp-echo icmp-echo-reply");
		env.addSort("Length", "len-other");
		env.addSort("Interface", "Vlan1 FastEthernet0");		
		
				
		env.addRequestVar("entry-interface", "Interface");
		env.addRequestVar("src-addr", "IPAddress");
		env.addRequestVar("src-addr_", "IPAddress");
		env.addRequestVar("dest-addr", "IPAddress");
		env.addRequestVar("dest-addr_", "IPAddress");
		env.addRequestVar("prot", "Protocol");
		env.addRequestVar("msg", "ICMPMessage");
		env.addRequestVar("src-port", "Port");
		env.addRequestVar("src-port_", "Port");
		env.addRequestVar("dest-port", "Port");
		env.addRequestVar("dest-port_", "Port");
		env.addRequestVar("length", "Length");
		env.addRequestVar("next-hop", "IPAddress");
		env.addRequestVar("exit-interface", "Interface");
		
				
		
		env.axioms.addConstraintDisjointAll("Protocol");
		env.axioms.addConstraintDisjointAll("ICMPMessage");
		env.axioms.addConstraintDisjointAll("Length");
		env.axioms.addConstraintDisjointAll("Interface");		
		
		return env;
	}
	
	public static MVocab setupFirewallVocab(String name, int numIPs, int rangeSize, int numPorts)
	throws MGEBadIdentifierName, MGEUnknownIdentifier, MGEArityMismatch, 
	MGEBadQueryString, MGEBadCombinator, MGECombineVocabs, MGEUnsortedVariable
	{
		MVocab env = new MVocab(name);
		
		env.addDecision("Accept");
		env.addDecision("Drop");
		
		String ipRanges = "";		
		int ipTemp = 0;
		while(ipTemp < numIPs)
		{
			String thisRange = "IPRange"+ipTemp+"_"+(ipTemp+rangeSize-1);
			ipRanges = ipRanges + thisRange+" ";						
			ipTemp += rangeSize;
		}
		
		env.addSort("IPAddress", ipRanges);
		
		ipTemp = 0;
		String[] ipRangesArr = ipRanges.split(" ");
		for(String range : ipRangesArr)
		{
			String ipAddrsInRange = "";
			for(int offset = 0;offset < rangeSize;offset++)				
				ipAddrsInRange += "IP" +(ipTemp+offset)+ " ";
			ipTemp+=rangeSize;
			env.addSort(range, ipAddrsInRange);
		}
				
		int portTemp = 0;
		String ports = "";
		while(portTemp < numPorts)
		{
			ports += "Port"+portTemp+" ";
			portTemp ++;
		}
		
		env.addSort("Port", ports);
		
		env.addSort("Protocol", "prot-ICMP prot-TCP prot-UDP");
		env.addSort("ICMPMessage", "icmp-N/A icmp-any");
		env.addSort("icmp-any", "icmp-echo icmp-echo-reply");
		env.addSort("Length", "len-other");
		env.addSort("Interface", "Vlan1 FastEthernet0");		
		
				
		env.addRequestVar("entry-interface", "Interface");
		env.addRequestVar("src-addr", "IPAddress");
		env.addRequestVar("src-addr_", "IPAddress");
		env.addRequestVar("dest-addr", "IPAddress");
		env.addRequestVar("dest-addr_", "IPAddress");
		env.addRequestVar("prot", "Protocol");
		env.addRequestVar("msg", "ICMPMessage");
		env.addRequestVar("src-port", "Port");
		env.addRequestVar("src-port_", "Port");
		env.addRequestVar("dest-port", "Port");
		env.addRequestVar("dest-port_", "Port");
		env.addRequestVar("length", "Length");
		env.addRequestVar("next-hop", "IPAddress");
		env.addRequestVar("exit-interface", "Interface");
		
				
		
		env.axioms.addConstraintDisjointAll("IPAddress");
		env.axioms.addConstraintDisjointAll("Port");
		env.axioms.addConstraintDisjointAll("Protocol");
		env.axioms.addConstraintDisjointAll("ICMPMessage");
		env.axioms.addConstraintDisjointAll("Length");
		env.axioms.addConstraintDisjointAll("Interface");
		
		
		
		for(String range : ipRangesArr)		
		{
			env.axioms.addConstraintDisjointAll(range);
			
			// would be BAD for tupling
			//env.axioms.addConstraintAbstract(range); // assume the range is covered by individuals
		}
		
		// IPAddress is not abstract! (allow the potential for some other IP to exist)		

		env.axioms.addConstraintAtMostOneAll("Port");
	//	env.axioms.addConstraintAbstract("Port");
						
		
		for(String range : ipRangesArr)
			env.axioms.addConstraintAtMostOneAll(range);
		
		
		return env;
	}
	
	public static void addEasyRules(MPolicyLeaf pol, MPolicyLeaf pol2, int numRules)
	throws MGEBadIdentifierName, MGEUnknownIdentifier, MGEArityMismatch, 
	MGEBadQueryString, MGEBadCombinator, MGECombineVocabs, MGEUnsortedVariable
	{
		ArrayList<String> templist = new ArrayList<String>();

		// don't clear templist, just set the final element
		templist.add("FastEthernet0 entry-interface");
		templist.add("prot-TCP prot");
		templist.add("");

	//	ThreadMXBean mxBean = ManagementFactory.getThreadMXBean();
//		long start = mxBean.getCurrentThreadCpuTime();		

		
		for(int ii=0;ii<numRules;ii++)
		{
			String decision = "Accept";
			if(ii % 2 == 0) decision = "Drop";
			
			templist.set(2, "IP"+ii+" src-addr");		
			//templist.add("Port"+(ii%3)+" dest-port");


			
			pol.addRule("EasyRule"+ii, decision, templist);
			pol2.addRule("EasyRule"+ii, decision, templist);
			
			
		}
		
	//	System.out.println((mxBean.getCurrentThreadCpuTime() - start) / 1000000);
	}

	public static void do_test_child_sort_exhaustive()
	throws MGEBadIdentifierName, MGEUnknownIdentifier, MGEArityMismatch, 
	MGEBadQueryString, MGEBadCombinator, MGECombineVocabs, MGEUnsortedVariable, MGEManagerException
	{
		
		// removed 1/28/10 TN
		
		/*MGVocab env = new MGVocab("test");
		MGPolicyLeaf pol = new MGPolicyLeaf("test", env);
		
		env.addDecision("Permit");
		env.addDecision("Deny");		
						
		env.addSort("A", "B otherA");		
			
		env.axioms.addConstraintDisjointAll("A");		
					
		// Need 2 existentials to trigger tupling at all!
		MGQuery qry = pol.queryPolicy("(forSome ax A (not (B ax)))");	

		// **************************
		// 1 solution: A but not B. 
		// (otherA used!)
		countTest("Child sort exhaustiveness in non-tupling (1)", qry, 1, 1, 1);	
		
		qry.doTupling = true;
		// **************************
		// 1 solution: A but not B.
		// (no notion of otherA, since not used in the query)

		// !!!!!!! 
		// Interesting to note: *not* a 1-1 correspondence between single models
		// since \sigma' may not include analogs of certain predicates.				
		countTest("Child sort exhaustiveness in tupling (1)", qry, 1, 1, 1);
		//qry.prettyPrintSolutions();	
		 		
		 */
		
		
	}
	
	public static void do_test_tupling_basic()
	throws MGEBadIdentifierName, MGEUnknownIdentifier, MGEArityMismatch, 
	MGEBadQueryString, MGEBadCombinator, MGECombineVocabs, MGEUnsortedVariable, MGEManagerException

	{
		System.out.println("ENTERING TEST BLOCK: do_test_tupling_basic()");
		MVocab fw = setupFirewallVocab("small", 50, 10, 10);
		MPolicyLeaf fwpol = new MPolicyLeaf("SmallFwTestPol1", fw);
		MPolicyLeaf fwpol2 = new MPolicyLeaf("SmallFwTestPol2", fw);
		addEasyRules(fwpol, fwpol2, 10);
		
		ArrayList<String> templist = new ArrayList<String>();
		templist.clear();		
		templist.add("FastEthernet0 entry-interface");
		templist.add("prot-UDP prot");
		templist.add("IP10 src-addr");		
		templist.add("IP10 dest-addr"); 
		fwpol2.addRule("Difference", "Accept", templist);
		
		fwpol.initIDBs();
		fwpol2.initIDBs();
				
		// Test to make sure disjointness is being done correctly
		List<MIDBCollection> pollist = new ArrayList<MIDBCollection>();
		pollist.clear();
		pollist.add(fwpol);
		
		// no longer allowed (for now)
		//fw.addPredicate("Connection", "IPAddress IPAddress Port Port");
		
		
		MQuery qry = fwpol.compareWithPolicy(fwpol2);
		qry.debug_verbosity = 2;
		qry.doTupling = true;
		//qry.prettyPrintOneSolution();
		countTest("Basic policy comparison with tupling", qry, 1, 450, 1);

		// TODO
		// 450 solns now? (was 1. is the diff. due to equality changes?)
		
		System.out.println("Basic policy comparison with Tupling + single IDB output: ");
		qry.addIDBOutputs("SmallFwTestPol2:Difference".toLowerCase());
						
	
		List<String> candidates = new ArrayList<String>();
		candidates.addAll(fwpol.getQualifiedIDBNameList());				
		candidates.addAll(fwpol2.getQualifiedIDBNameList());
		
		qry.addIDBOutputs(candidates);
		
		List<String> indexing = new ArrayList<String>(4);
		for(int ii = 1; ii <= 14; ii++)
			indexing.add(String.valueOf(ii));
		
		for(String cand : candidates)
			qry.addIDBOutputIndexing(cand, indexing);

		//qry.prettyPrintOneSolution();
		qry.debug_verbosity = 2;
		System.out.println("Starting qry:");
		MQueryResult qryResult = qry.runQuery();
		System.out.println(qryResult.getPopulatedRelationFinder().getPopulatedRelations(candidates));
		
		qry = MQuery.queryThesePolicies(
				fwpol.getExistentialRequestPrefix() + "(" + "smallfwtestpol1:easyrule1 "+
				fwpol.getRequestVarVector() + ")" +
				fwpol.getRequestPrefixClosing(), 
				pollist);
				
		qry.doTupling = true;
		candidates.clear();
		candidates.addAll(fwpol.getQualifiedIDBNameList());				
		qry.addIDBOutputs(candidates);
		for(String cand : candidates)
			qry.addIDBOutputIndexing(cand, indexing);

		qryResult = qry.runQuery();
		System.out.println(qryResult.getPopulatedRelationFinder().getPopulatedRelations(candidates));
		
		qry = MQuery.queryThesePolicies("(forsome x IPAddress (forsome y IPAddress (and (IPRange0_9 x) (IPRange10_19 x))))", pollist);
		qry.doTupling = true;
		countTest("Tupling disjointness", qry, 1, 0, 1);
		
		//System.out.println("********************\n\n\n*******************");
		
		// Test equality in query
		//=1,2; IP10_1
		// But we haven't *mentioned* IP10_2, so equality won't force it
		qry = MQuery.queryThesePolicies("(forsome x IPAddress (forsome y IPAddress (and (= x y) (IP10 x))))", pollist);
		qry.doTupling = true;
		//qry.debug_verbosity = 3;
		//qry.prettyPrintSolutions();
		countTest("Tupling with equality 1", qry, 1, 1, 1);
				

		
		// IP10_2 appears in the query in a completely meaningless way
		qry = MQuery.queryThesePolicies("(forsome x IPAddress (forsome y IPAddress (and (= x y) (IP10 x) (or (IP10 y)(not (IP10 y ))))))", pollist);
		qry.doTupling = true;
		countTest("Tupling with equality 2", qry, 1, 1, 1);
		;
		

		
		// Same as above, but without the equality. (This test makes sure that multiple results come up, 
		// so testing for 1 result above is a meaningful reduction.)
		qry = MQuery.queryThesePolicies("(forsome x IPAddress (forsome y IPAddress (and (IP10 x) (or (IP10 y)(not (IP10 y ))))))", pollist);
		qry.doTupling = true;
		//qry.debug_verbosity = 3;
		countTest("Tupling with equality 2 (control)", qry, 1, 3, 1);
		// 3 solutions: both have ip10_1. Options for 2: ip10, other ip address, other ip address in the 10_19 range
		// Note: this test also makes sure that tupling doesn't carry over abstractness unless all child sorts are present
		
		
		// Transitivity of equality
		qry = MQuery.queryThesePolicies("(forsome x IPAddress (forsome y IPAddress (forsome z IPAddress (and (= x y) (= y z) (IP10 x) (or (IP10 z) (not (= x z)))))))", pollist);
		qry.doTupling = true;
		countTest("Tupling with equality (transitivity) 1", qry, 1, 1, 1);
		// 1 solution: =13 must hold 
				
		qry = MQuery.queryThesePolicies("(forsome x IPAddress (forsome y IPAddress (forsome z IPAddress (and (= x y) (= y z) (IP10 x) (not (IP10 z))))))", pollist);
		qry.doTupling = true;
		countTest("Tupling with equality (transitivity) 2", qry, 1, 0, 1);
		// Unsatisfiable. =13 isn't mentioned explicitly but it is inferred, and it causes P_1 <--> P_3. 
		
		qry = MQuery.queryThesePolicies("(forsome x IPAddress (forsome y IPAddress (forsome z IPAddress (and (= x y) (= y z) (IP10 x)))))", pollist);
		qry.doTupling = true;
		countTest("Tupling with equality (transitivity) 2 (control)", qry, 1, 1, 1);
		// Control for above test: P_3 is allowed
				
		// Not transitivity per se (we never "care" about =13)
		// We don't want to require a proposition for IP10_2 if it's never mentioned, here.
		// Output should be able to *deduce* That since =12 and IP10_1 => IP10_2.
		// But still require IP10_3.
		qry = MQuery.queryThesePolicies("(forsome x IPAddress (forsome y IPAddress (forsome z IPAddress (and (= x y) (= y z) (IP10 x) (or (IP10 z)(not (IP10 z )))))))", pollist);
		qry.doTupling = true;
		//qry.debug_verbosity = 3;
		countTest("Tupling with equality 3", qry, 1, 1, 1);		
		// IP10_1, IPAddress_2, and IP10_3: IP10_2 can be inferred easily in output generation				
		
		// Make sure things are allowed to be NOT equal
		qry = MQuery.queryThesePolicies("(forsome x IPAddress (forsome y IPAddress (forsome z IPAddress (and (= x y) (not (= y z)) (IP10 x)))))", pollist);
		qry.doTupling = true;
		//qry.debug_verbosity = 3;
		countTest("Tupling with equality 4", qry, 1, 1, 1);
		// one solution: If x=y and y!=z, we can't also have x=z. This is since if we did,
		// y=x and x=z would imply (by transitivity) y=z.

		
		//  child-sorts
		qry = MQuery.queryThesePolicies("(forsome x IPAddress (forsome y IPAddress (and (IP10 x) (not (IP10 y)))))", pollist);
		qry.doTupling = true;
		//qry.debug_verbosity = 3;
		countTest("Tupling with child sorts 1", qry, 1, 2, 1);
		// Either y is in some unmentioned subsort of IPRange10_19, or it is in an unmentioned IP Range
		// (In either case, x is not equal to y)
		
		
		// child sorts + equality
		// Need to use a child sort of IPAddress_1 in a powerless way in order to trigger inclusion
		qry = MQuery.queryThesePolicies("(forsome x IPAddress (forsome y IPAddress (and (= x y) (or (IP27 x) (not (IP27 x ))) (or (IP17 x) (not (IP17 x ))) (not (IPRange10_19 y)))))", pollist);
		qry.doTupling = true;
		countTest("Tupling with child sorts, equality 1", qry, 1, 3, 1);		
		//qry.prettyPrintSolutions();
		// 3: Always x=y. Always "some other IPAddress" for y (meaning *not* the mentioned child). 
		// 3 options for x: IP27 (we know of it), "some other in range 20_29", or "some other ip in another range"
		// Cannot be IP17 since x=y and y isn't in 10-19.
		
				
		// reflexivity of equality	
		qry = MQuery.queryThesePolicies("(forsome x IPAddress (or (not (= x x)) (IP10 x )))", pollist);
		qry.doTupling = true;
		countTest("Tupling with equality (Reflexivity)", qry, 1, 1, 1);
		// only one solution: =11 always holds, so IP10_1 must as well
		
		
		// symmetry of equality	
		qry = MQuery.queryThesePolicies("(forsome x IPAddress (forsome y IPAddress (and (= x y) (or (not (= y x)) (IP10 x )))))", pollist);
		qry.doTupling = true;
		countTest("Tupling with equality (Symmetry)", qry, 1, 1, 1);
		// only one solution: =12 is the same as =21, and that implies IP10_1.
				
		// Tests for LONE and its interaction with equality
		qry = MQuery.queryThesePolicies("(forsome x IPAddress (forsome y IPAddress (and (IP10 x) (IP10 y) (not (= x y)))))", pollist);
		qry.doTupling = true;
		//qry.debug_verbosity = 2;
		countTest("Tupling with equality, LONE", qry, 1, 0, 1);		
		// should be unsatisfiable since IP10 is a LONE: IP10_1 and IP10_2 implies that =12.		
		
	
		// Full range of motion
		qry = MQuery.queryThesePolicies("(forsome x IPAddress (forsome y Port (and (or (IP10 x) (not (IP10 x))) (or (Port1 y) (not (Port1 y))))))", pollist);
		qry.doTupling = true;
		countTest("Tupling: Full range", qry, 1, 6, 1);		
		// IP10 < IPRange10_19 < IP; Port1 < Port. 6 options.
		
		// Full range of motion, equality
		qry = MQuery.queryThesePolicies("(forsome x IPAddress (forsome x2 IPAddress (forsome y Port (and (= x x2) (IPRange10_19 x2) (or (IP10 x) (not (IP10 x))) (or (Port1 y) (not (Port1 y)))))))", pollist);
		qry.doTupling = true;
		countTest("Tupling: Full range with equality", qry, 1, 4, 1);				
		// Now since x=x2, x must at least be in that RANGE. (Still may be some other IP in the range)
				
		// Equality appears, but need not be constrained
		qry = MQuery.queryThesePolicies("(forsome x IPAddress (forsome y IPAddress (and (not (IPRange10_19 x)) (not (IPRange20_29 y)) (or (not (= x y)) (= x y)))))", pollist);
		qry.doTupling = true;
		countTest("Tupling with equality, freedom re: equality", qry, 1, 2, 1);		
		
		// removed apr 2010: we are now representing all equalities by default
		// Control (equality unmentioned)
		//qry = MQuery.queryThesePolicies("(forsome x IPAddress (forsome y IPAddress (and (not (IPRange10_19 x)) (not (IPRange20_29 y)))))", pollist);
		//qry.doTupling = true;
		//countTest("Tupling with equality, above w/o explicit equality", qry, 1, 1, 1);		
		//qry.prettyPrintSolutions();
		
		// Make sure an "other" sort isn't created if no children are mentioned
		qry = MQuery.queryThesePolicies("(forsome x IPAddress (forsome y IPAddress (IPAddress x)))", pollist);
		qry.doTupling = true;
		System.out.println("\n This should be a single result, having only IPAddress_1 and IPAddress_2.");
		//qry.prettyPrintSolutions();
		System.out.println("\n");
		
		//qry.prettyPrintSolutions();

		// *********************
		// Test on >1-ary preds
		// This feature has been disabled for now.
/*		templist.clear();		
		templist.add("FastEthernet0 entry-interface");
		templist.add("Connection src-addr dest-addr src-port dest-port");
		fwpol.addRule("TestPred1", "Accept", templist);
		fwpol.initIDBs();
		
		qry = fwpol.compareWithPolicy(fwpol2);		
		qry.doTupling = true;
		qry.debug_verbosity = 1;
		// *************************
		// 1 solution (from before) + others for new connection rule
		countTest("Tupling with 4-ary predicate", qry, 1, 200, 1); // only correct if doing the standard 200/200 test
*/

		
		// ************
		// This example must be the last, since we are adding a strange constraint.
		
		// Add a subset constraint other than subsorting, and make sure it translates over when tupling.		
		fw.axioms.addConstraintSubset("IP10", "IP11"); // the only addresses that the policies disagree on
		fwpol.initIDBs();
		fwpol2.initIDBs();
		qry = fwpol.compareWithPolicy(fwpol2);						
		countTest("Subset constraints in Tupling (Step 1)", qry, -1, 0, 14);	
		qry.doTupling = true;
		countTest("Subset constraints in Tupling (Step 2)", qry, 1, 0, 1);						

		
		
		
		
		
		System.out.println("\n\n\n\n\n");
		MEnvironment myInterface = new MEnvironment(); // use default stream
				
		myInterface.savePolicyAs("SmallFwTestPol1", fwpol);
		myInterface.savePolicyAs("SmallFwTestPol2", fwpol2);
	
		String eol = System.getProperty("line.separator");

		
		String query1 = "EXPLORE SmallFwTestPol1:Accept(x, y, z)";		
		String query1Result = "MARGRAVE ERROR: \n"+
		"Arity Mismatch. Vector given was: [x, y, z], but collection expects arity 14. at Row: 0, Column: 8. This was around: smallfwtestpol1";
				
		// Under to deal with EDBs without talking about IDBs
		String query2 = "EXPLORE \"foozle:uri:subject:role=professor\"(x) UNDER SmallFwTestPol1";
		String query2Result = "MARGRAVE ERROR: \n"+
		"Unknown EDB foozle:uri:subject:role=professor. The given policies were unaware of this EDB name.";	
		
		
		String query2a = "EXPLORE ip5(x) UNDER SmallFwTestPol1";
		int query2aResult = 1;
		
		String query2b = "EXPLORE ip5(x) UNDER SomePolicyThatDoesNotExist";
		String query2bResult = "MARGRAVE ERROR: \n"+ 
			"Unknown symbol in UNDER clause: somepolicythatdoesnotexist";
		
		String query3 = "EXPLORE SmallFwTestPol1:\"foozle:uri:subject:role=professor\"(x)";
		String query3Result = "MARGRAVE ERROR: \n"+
"Unknown IDB for the collection smallfwtestpol1 at Row: 0, Column: 24. This was around: foozle:uri:subject:role=professor";
		
		String query4 = "EXPLORE SmallFwTestPol1:Accept("+fwpol.getRequestVarVectorWithCommas()+") \n\r"+
		                 "    AND src-port = dest-port AND port5(src-port) \r\n" +
		                 "    AND (src-addr=dest-addr AND ip3(src-addr)) \n" +
		                 "    AND icmp-echo(msg) \r"+
		                 "PUBLISH "+fwpol.getRequestVarVectorWithCommas()+"\n";
		int query4Result = 4;
		
		//System.out.println(fwpol.getRequestVarVectorWithCommas());
		
		String query4a = " INFO last";
		String query4aResult = 
"--------------------------------------------------"+eol+
"This is a SAVED QUERY." +eol+
"Parameters:" +eol+
"(1) entry-interface: interface" +eol+
"(2) src-addr: ip3" +eol+
"(3) src-addr_: ipaddress" +eol+
"(4) dest-addr: ipaddress" +eol+
"(5) dest-addr_: ipaddress" +eol+
"(6) prot: protocol" +eol+
"(7) msg: icmp-echo" +eol+
"(8) src-port: port5" +eol+
"(9) src-port_: port" +eol+
"(10) dest-port: port" +eol+
"(11) dest-port_: port" +eol+
"(12) length: length" +eol+
"(13) next-hop: ipaddress" +eol+
"(14) exit-interface: interface" +eol+
"--------------------------------------------------";
		
		String query4b = " INFO SmallFwTestPol1";
		String query4bResult = 
			"--------------------------------------------------"+eol+
			"This is a POLICY." +eol+
			"Parameters:" +eol+
			"(1) entry-interface: interface" +eol+
			"(2) src-addr: ipaddress" +eol+
			"(3) src-addr_: ipaddress" +eol+
			"(4) dest-addr: ipaddress" +eol+
			"(5) dest-addr_: ipaddress" +eol+
			"(6) prot: protocol" +eol+
			"(7) msg: icmpmessage" +eol+
			"(8) src-port: port" +eol+
			"(9) src-port_: port" +eol+
			"(10) dest-port: port" +eol+
			"(11) dest-port_: port" +eol+
			"(12) length: length" +eol+
			"(13) next-hop: ipaddress" +eol+
			"(14) exit-interface: interface" +eol+
			"--------------------------------------------------";			
		
		String query4c = " EXPLORE SmallFwTestPol1:Accept("+fwpol.getRequestVarVectorWithCommas()+") \n\r"+
						" and last(" + fwpol.getRequestVarVectorWithCommas() + ")";
		int query4cResult = 4;
		
		String query5 = "EXPLORE SmallFwTestPol1:Accept(_, srcip, _, destip, _, prot, msg, srcp, _, destp, _, _, _, _)" + // test no space
		 "AND srcp = destp AND port5(srcp) \r\n" +
         "    AND (srcip=destip AND ip3(srcip)) and icmp-echo(msg) and prot-tcp(prot)\n"+
         "SHOW ONE";
		int query5Result = 4; // SHOW ONE will give only one of them
					
		
		String query6 = query4 + "CEILING 2";
		int query6Result = 0;
		
		String query7 = query4 + "SHOW ONE TUPLING";
		int query7Result = 150; // SHOW ONE will give only one of them
		
		//String query8 = query4 + "DEBUG 1";
		//String query8Result = "";
		
		String query8a = query4 + "IS POSSIBLE?";
		String query8aResult = "true: Query had solutions.";
		
		
		// Test _ with multiple IDB apps to make sure we don't re-use vars (trust MFormulaManager) 
		String query9 = "EXPLORE SmallFwTestPol1:Accept(_, srcip, _, destip, _, prot, msg, srcp, _, destp, _, _, _, _) and " +
		"SmallFwTestPol2:Accept(_, srcip, _, destip, _, prot, msg, srcp, _, destp, _, _, _, _)"+
		 "AND srcp = destp AND port5(srcp) \r\n" +
        "    AND (srcip=destip AND ip3(srcip)) and icmp-echo(msg) and prot-tcp(prot)\n"+
        "SHOW ONE";
		int query9Result = 1;
		
		// Test w/ no results from inference (Expression.UNIV for quantifier sort)
		String query10 = "EXPLORE NOT prot-tcp(p) UNDER SmallFwTestPol1 SHOW ONE";				
		int query10Result = 1;
		
		// Test idboutput w/o vector (not for tupling)
		String query11 = "EXPLORE SmallFwTestPol1:Accept("+fwpol.getRequestVarVectorWithCommas()+")"+
		" UNDER SmallFwTestPol1 SHOW ONE IDBOUTPUT SmallFwTestPol1:Accept";
		int query11Result = 1;
		
		// idboutput vector w/ tupling
		String query12 = "EXPLORE SmallFwTestPol1:Accept("+fwpol.getRequestVarVectorWithCommas()+")"+
		" UNDER SmallFwTestPol1 SHOW ONE IDBOUTPUT SmallFwTestPol1:Accept("+ fwpol.getRequestVarVectorWithCommas()+") TUPLING";		
		int query12Result = 1;
		
		// multiple IDBs
		String query13 = "EXPLORE SmallFwTestPol1:Accept("+fwpol.getRequestVarVectorWithCommas()+") AND SmallFwTestPol1:EasyRule1("+fwpol.getRequestVarVectorWithCommas()+") "+
		" UNDER SmallFwTestPol1 SHOW ONE IDBOUTPUT "+
		"SmallFwTestPol1:Accept("+ fwpol.getRequestVarVectorWithCommas()+"), "+
		"SmallFwTestPol1:EasyRule1("+ fwpol.getRequestVarVectorWithCommas()+")"
		+" TUPLING";
		int query13Result = 1;
		
		// Multiple indexings of same IDB
		String query14 = "EXPLORE SmallFwTestPol1:Accept(entry-interface, src-addr, src-addr_, dest-addr, dest-addr_, prot, msg, src-port, src-port_, dest-port, dest-port_, length, next-hop, exit-interface) AND "+
		"SmallFwTestPol1:Accept(entry-interface, dest-addr, src-addr_, src-addr, dest-addr_, prot, msg, src-port, src-port_, dest-port, dest-port_, length, next-hop, exit-interface) "+
		" UNDER SmallFwTestPol1 SHOW ONE IDBOUTPUT "+
		"SmallFwTestPol1:Accept(entry-interface, src-addr, src-addr_, dest-addr, dest-addr_, prot, msg, src-port, src-port_, dest-port, dest-port_, length, next-hop, exit-interface), "+
		"SmallFwTestPol1:Accept(entry-interface, dest-addr, src-addr_, src-addr, dest-addr_, prot, msg, src-port, src-port_, dest-port, dest-port_, length, next-hop, exit-interface)"
		+" TUPLING";
		int query14Result = 1;
		
		// Populated w/o tupling
		String query15 = "EXPLORE SmallFwTestPol1:Accept("+fwpol.getRequestVarVectorWithCommas()+") "+
		" OR SmallFwTestPol2:Drop("+fwpol.getRequestVarVectorWithCommas()+")"+
		" UNDER SmallFwTestPol1 SHOW POPULATED SmallFwTestPol1:Accept, IPAddress, SmallFwTestPol1:Drop IDBOUTPUT SmallFwTestPol1:Accept, SmallFwTestPol1:Drop";
		Map<String, Set<String>> query15Result = new HashMap<String, Set<String>>();
		Set<String> q15_c1 = new HashSet<String>();
		q15_c1.add("smallfwtestpol1:drop");
		q15_c1.add("smallfwtestpol1:accept");
		q15_c1.add("ipaddress");
		query15Result.put("", q15_c1);

		// Populated w/o tupling + indexing (should be error)
		String query15error = "EXPLORE SmallFwTestPol1:Accept("+fwpol.getRequestVarVectorWithCommas()+") "+
		" OR SmallFwTestPol2:Drop("+fwpol.getRequestVarVectorWithCommas()+")"+
		" UNDER SmallFwTestPol1 SHOW POPULATED SmallFwTestPol1:Accept(entry-interface, src-addr, src-addr_, dest-addr, dest-addr_, prot, msg, src-port, src-port_, dest-port, dest-port_, length, next-hop, exit-interface), "+
		" IPAddress, SmallFwTestPol1:Drop IDBOUTPUT SmallFwTestPol1:Accept, SmallFwTestPol1:Drop";			
		String query15errorResult = "MARGRAVE ERROR: \n"+ 
			"TUPLING was not enabled for a SHOW (UN)POPULATED query but pred: smallfwtestpol1:accept in the SHOW clause was indexed.";		
		
		// UN populated w/o tupling
		String query15a = "EXPLORE SmallFwTestPol1:Accept("+fwpol.getRequestVarVectorWithCommas()+") "+
		" OR SmallFwTestPol2:Drop("+fwpol.getRequestVarVectorWithCommas()+")"+
		" UNDER SmallFwTestPol1 SHOW UNPOPULATED SmallFwTestPol1:Accept, IPAddress, SmallFwTestPol1:Drop IDBOUTPUT SmallFwTestPol1:Accept, SmallFwTestPol1:Drop";
		Map<String, Set<String>> query15aResult = new HashMap<String, Set<String>>();
		Set<String> q15a_c1 = new HashSet<String>();
		query15aResult.put("", q15a_c1);	
		
		// Populated w/ tupling
		// must have indexing!
		String query16 = "EXPLORE SmallFwTestPol1:Accept("+fwpol.getRequestVarVectorWithCommas()+")"+
		" UNDER SmallFwTestPol1 SHOW POPULATED SmallFwTestPol1:Accept(entry-interface, src-addr, src-addr_, dest-addr, dest-addr_, prot, msg, src-port, src-port_, dest-port, dest-port_, length, next-hop, exit-interface) "+
		"IDBOUTPUT SmallFwTestPol1:Accept(entry-interface, src-addr, src-addr_, dest-addr, dest-addr_, prot, msg, src-port, src-port_, dest-port, dest-port_, length, next-hop, exit-interface) TUPLING";
		Map<String, Set<String>> query16Result = new HashMap<String, Set<String>>();
		Set<String> q16_c1 = new HashSet<String>();
		q16_c1.add("smallfwtestpol1:accept[entry-interface, src-addr, src-addr_, dest-addr, dest-addr_, prot, msg, src-port, src-port_, dest-port, dest-port_, length, next-hop, exit-interface]");
		query16Result.put("", q16_c1);		
		
		// Multiple populated w/ tupling (both multiple indexings and multiple preds)
		// Only one un-populated across all models is Drop.
		// NOTE: must give indexing here even for EDBs.
		String query17 = "EXPLORE SmallFwTestPol1:Accept("+fwpol.getRequestVarVectorWithCommas()+")"+						
		" SHOW POPULATED SmallFwTestPol1:Accept(entry-interface, src-addr, src-addr_, dest-addr, dest-addr_, prot, msg, src-port, src-port_, dest-port, dest-port_, length, next-hop, exit-interface) "+
		" ,SmallFwTestPol1:Accept(entry-interface, dest-addr, src-addr_, src-addr, dest-addr_, prot, msg, src-port, src-port_, dest-port, dest-port_, length, next-hop, exit-interface) "+
		" ,SmallFwTestPol1:Drop(entry-interface, src-addr, src-addr_, dest-addr, dest-addr_, prot, msg, src-port, src-port_, dest-port, dest-port_, length, next-hop, exit-interface) "+
		" ,IPAddress(src-addr) "+
		" ,SmallFwTestPol1:EasyRule1_Applies(entry-interface, src-addr, src-addr_, dest-addr, dest-addr_, prot, msg, src-port, src-port_, dest-port, dest-port_, length, next-hop, exit-interface) "+
		" ,SmallFwTestPol1:EasyRule3_Applies(entry-interface, src-addr, src-addr_, dest-addr, dest-addr_, prot, msg, src-port, src-port_, dest-port, dest-port_, length, next-hop, exit-interface) "+
		" IDBOUTPUT SmallFwTestPol1:Accept(entry-interface, src-addr, src-addr_, dest-addr, dest-addr_, prot, msg, src-port, src-port_, dest-port, dest-port_, length, next-hop, exit-interface) "+
		" ,SmallFwTestPol1:Accept(entry-interface, dest-addr, src-addr_, src-addr, dest-addr_, prot, msg, src-port, src-port_, dest-port, dest-port_, length, next-hop, exit-interface) "+
		" ,SmallFwTestPol1:Drop(entry-interface, src-addr, src-addr_, dest-addr, dest-addr_, prot, msg, src-port, src-port_, dest-port, dest-port_, length, next-hop, exit-interface) "+
		" ,SmallFwTestPol1:EasyRule1_Applies(entry-interface, src-addr, src-addr_, dest-addr, dest-addr_, prot, msg, src-port, src-port_, dest-port, dest-port_, length, next-hop, exit-interface) "+
		" ,SmallFwTestPol1:EasyRule3_Applies(entry-interface, src-addr, src-addr_, dest-addr, dest-addr_, prot, msg, src-port, src-port_, dest-port, dest-port_, length, next-hop, exit-interface) "+
		" TUPLING";	
		Map<String, Set<String>> query17Result = new HashMap<String, Set<String>>();
		Set<String> q17_c1 = new HashSet<String>();
		q17_c1.add("smallfwtestpol1:easyrule3_applies[entry-interface, src-addr, src-addr_, dest-addr, dest-addr_, prot, msg, src-port, src-port_, dest-port, dest-port_, length, next-hop, exit-interface]");
		q17_c1.add("smallfwtestpol1:easyrule1_applies[entry-interface, src-addr, src-addr_, dest-addr, dest-addr_, prot, msg, src-port, src-port_, dest-port, dest-port_, length, next-hop, exit-interface]");
		q17_c1.add("smallfwtestpol1:accept[entry-interface, src-addr, src-addr_, dest-addr, dest-addr_, prot, msg, src-port, src-port_, dest-port, dest-port_, length, next-hop, exit-interface]");
		q17_c1.add("smallfwtestpol1:accept[entry-interface, dest-addr, src-addr_, src-addr, dest-addr_, prot, msg, src-port, src-port_, dest-port, dest-port_, length, next-hop, exit-interface]");
		q17_c1.add("ipaddress[src-addr]");
		query17Result.put("", q17_c1);
		
		// Same w/ UNpopulated
		String query18 = "EXPLORE SmallFwTestPol1:Accept("+fwpol.getRequestVarVectorWithCommas()+")"+						
		" SHOW UNPOPULATED SmallFwTestPol1:Accept(entry-interface, src-addr, src-addr_, dest-addr, dest-addr_, prot, msg, src-port, src-port_, dest-port, dest-port_, length, next-hop, exit-interface) "+
		" ,SmallFwTestPol1:Accept(entry-interface, dest-addr, src-addr_, src-addr, dest-addr_, prot, msg, src-port, src-port_, dest-port, dest-port_, length, next-hop, exit-interface) "+
		" ,SmallFwTestPol1:Drop(entry-interface, src-addr, src-addr_, dest-addr, dest-addr_, prot, msg, src-port, src-port_, dest-port, dest-port_, length, next-hop, exit-interface) "+
		" ,IPAddress(src-addr) "+
		" ,SmallFwTestPol1:EasyRule1_Applies(entry-interface, src-addr, src-addr_, dest-addr, dest-addr_, prot, msg, src-port, src-port_, dest-port, dest-port_, length, next-hop, exit-interface) "+
		" ,SmallFwTestPol1:EasyRule3_Applies(entry-interface, src-addr, src-addr_, dest-addr, dest-addr_, prot, msg, src-port, src-port_, dest-port, dest-port_, length, next-hop, exit-interface) "+
		" IDBOUTPUT SmallFwTestPol1:Accept(entry-interface, src-addr, src-addr_, dest-addr, dest-addr_, prot, msg, src-port, src-port_, dest-port, dest-port_, length, next-hop, exit-interface) "+
		" ,SmallFwTestPol1:Accept(entry-interface, dest-addr, src-addr_, src-addr, dest-addr_, prot, msg, src-port, src-port_, dest-port, dest-port_, length, next-hop, exit-interface) "+
		" ,SmallFwTestPol1:Drop(entry-interface, src-addr, src-addr_, dest-addr, dest-addr_, prot, msg, src-port, src-port_, dest-port, dest-port_, length, next-hop, exit-interface) "+
		" ,SmallFwTestPol1:EasyRule1_Applies(entry-interface, src-addr, src-addr_, dest-addr, dest-addr_, prot, msg, src-port, src-port_, dest-port, dest-port_, length, next-hop, exit-interface) "+
		" ,SmallFwTestPol1:EasyRule3_Applies(entry-interface, src-addr, src-addr_, dest-addr, dest-addr_, prot, msg, src-port, src-port_, dest-port, dest-port_, length, next-hop, exit-interface) "+
		" TUPLING";	 
		Map<String, Set<String>> query18Result = new HashMap<String, Set<String>>();
		Set<String> q18_c1 = new HashSet<String>();
		q18_c1.add("smallfwtestpol1:drop[entry-interface, src-addr, src-addr_, dest-addr, dest-addr_, prot, msg, src-port, src-port_, dest-port, dest-port_, length, next-hop, exit-interface]");
		query18Result.put("", q18_c1);
		
		
		//  FOR CASES w/ tupling POP
		// One impossible case (empty result)
		String query19 = "EXPLORE SmallFwTestPol1:Accept("+fwpol.getRequestVarVectorWithCommas()+")"+						
		" SHOW POPULATED SmallFwTestPol1:Accept(entry-interface, src-addr, src-addr_, dest-addr, dest-addr_, prot, msg, src-port, src-port_, dest-port, dest-port_, length, next-hop, exit-interface) "+
		" ,SmallFwTestPol1:Accept(entry-interface, dest-addr, src-addr_, src-addr, dest-addr_, prot, msg, src-port, src-port_, dest-port, dest-port_, length, next-hop, exit-interface) "+
		" ,SmallFwTestPol1:Drop(entry-interface, src-addr, src-addr_, dest-addr, dest-addr_, prot, msg, src-port, src-port_, dest-port, dest-port_, length, next-hop, exit-interface) "+
		" ,IPAddress(src-addr) "+
		" ,SmallFwTestPol1:EasyRule1_Applies(entry-interface, src-addr, src-addr_, dest-addr, dest-addr_, prot, msg, src-port, src-port_, dest-port, dest-port_, length, next-hop, exit-interface) "+
		" ,SmallFwTestPol1:EasyRule3_Applies(entry-interface, src-addr, src-addr_, dest-addr, dest-addr_, prot, msg, src-port, src-port_, dest-port, dest-port_, length, next-hop, exit-interface) "+
		"FOR CASES Port(src-port), Port(dest-port), Port(src-addr)"+
		" IDBOUTPUT SmallFwTestPol1:Accept(entry-interface, src-addr, src-addr_, dest-addr, dest-addr_, prot, msg, src-port, src-port_, dest-port, dest-port_, length, next-hop, exit-interface) "+
		" ,SmallFwTestPol1:Accept(entry-interface, dest-addr, src-addr_, src-addr, dest-addr_, prot, msg, src-port, src-port_, dest-port, dest-port_, length, next-hop, exit-interface) "+
		" ,SmallFwTestPol1:Drop(entry-interface, src-addr, src-addr_, dest-addr, dest-addr_, prot, msg, src-port, src-port_, dest-port, dest-port_, length, next-hop, exit-interface) "+
		" ,SmallFwTestPol1:EasyRule1_Applies(entry-interface, src-addr, src-addr_, dest-addr, dest-addr_, prot, msg, src-port, src-port_, dest-port, dest-port_, length, next-hop, exit-interface) "+
		" ,SmallFwTestPol1:EasyRule3_Applies(entry-interface, src-addr, src-addr_, dest-addr, dest-addr_, prot, msg, src-port, src-port_, dest-port, dest-port_, length, next-hop, exit-interface) "+
		" TUPLING";
		Map<String, Set<String>> query19Result = new HashMap<String, Set<String>>();
		Set<String> q19_c1 = new HashSet<String>();
		q19_c1.add("smallfwtestpol1:easyrule3_applies[entry-interface, src-addr, src-addr_, dest-addr, dest-addr_, prot, msg, src-port, src-port_, dest-port, dest-port_, length, next-hop, exit-interface]");
		q19_c1.add("smallfwtestpol1:easyrule1_applies[entry-interface, src-addr, src-addr_, dest-addr, dest-addr_, prot, msg, src-port, src-port_, dest-port, dest-port_, length, next-hop, exit-interface]");
		q19_c1.add("smallfwtestpol1:accept[entry-interface, src-addr, src-addr_, dest-addr, dest-addr_, prot, msg, src-port, src-port_, dest-port, dest-port_, length, next-hop, exit-interface]");
		q19_c1.add("smallfwtestpol1:accept[entry-interface, dest-addr, src-addr_, src-addr, dest-addr_, prot, msg, src-port, src-port_, dest-port, dest-port_, length, next-hop, exit-interface]");
		q19_c1.add("ipaddress[src-addr]");
		Set<String> q19_c2 = new HashSet<String>();
		q19_c2.add("smallfwtestpol1:easyrule3_applies[entry-interface, src-addr, src-addr_, dest-addr, dest-addr_, prot, msg, src-port, src-port_, dest-port, dest-port_, length, next-hop, exit-interface]");
		q19_c2.add("smallfwtestpol1:easyrule1_applies[entry-interface, src-addr, src-addr_, dest-addr, dest-addr_, prot, msg, src-port, src-port_, dest-port, dest-port_, length, next-hop, exit-interface]");
		q19_c2.add("smallfwtestpol1:accept[entry-interface, src-addr, src-addr_, dest-addr, dest-addr_, prot, msg, src-port, src-port_, dest-port, dest-port_, length, next-hop, exit-interface]");
		q19_c2.add("smallfwtestpol1:accept[entry-interface, dest-addr, src-addr_, src-addr, dest-addr_, prot, msg, src-port, src-port_, dest-port, dest-port_, length, next-hop, exit-interface]");
		q19_c2.add("ipaddress[src-addr]");
		Set<String> q19_c3 = new HashSet<String>();
		query19Result.put("port[dest-port]", q19_c1);
		query19Result.put("port[src-port]", q19_c2);
		query19Result.put("port[src-addr]", q19_c3);
		
		
		//  FOR CASES w/ tupling UNPOP
		// One impossible case (empty result)
		String query20 = "EXPLORE SmallFwTestPol1:Drop("+fwpol.getRequestVarVectorWithCommas()+")"+						
		" SHOW UNPOPULATED SmallFwTestPol1:Accept(entry-interface, src-addr, src-addr_, dest-addr, dest-addr_, prot, msg, src-port, src-port_, dest-port, dest-port_, length, next-hop, exit-interface) "+
		" ,SmallFwTestPol1:Accept(entry-interface, dest-addr, src-addr_, src-addr, dest-addr_, prot, msg, src-port, src-port_, dest-port, dest-port_, length, next-hop, exit-interface) "+
		" ,SmallFwTestPol1:Drop(entry-interface, src-addr, src-addr_, dest-addr, dest-addr_, prot, msg, src-port, src-port_, dest-port, dest-port_, length, next-hop, exit-interface) "+
		" ,IPAddress(src-addr) "+
		" ,SmallFwTestPol1:EasyRule1_Applies(entry-interface, src-addr, src-addr_, dest-addr, dest-addr_, prot, msg, src-port, src-port_, dest-port, dest-port_, length, next-hop, exit-interface) "+
		" ,SmallFwTestPol1:EasyRule3_Applies(entry-interface, src-addr, src-addr_, dest-addr, dest-addr_, prot, msg, src-port, src-port_, dest-port, dest-port_, length, next-hop, exit-interface) "+
		"FOR CASES Port(src-port), Port(dest-port), Port(src-addr)"+
		" IDBOUTPUT SmallFwTestPol1:Accept(entry-interface, src-addr, src-addr_, dest-addr, dest-addr_, prot, msg, src-port, src-port_, dest-port, dest-port_, length, next-hop, exit-interface) "+
		" ,SmallFwTestPol1:Accept(entry-interface, dest-addr, src-addr_, src-addr, dest-addr_, prot, msg, src-port, src-port_, dest-port, dest-port_, length, next-hop, exit-interface) "+
		" ,SmallFwTestPol1:Drop(entry-interface, src-addr, src-addr_, dest-addr, dest-addr_, prot, msg, src-port, src-port_, dest-port, dest-port_, length, next-hop, exit-interface) "+
		" ,SmallFwTestPol1:EasyRule1_Applies(entry-interface, src-addr, src-addr_, dest-addr, dest-addr_, prot, msg, src-port, src-port_, dest-port, dest-port_, length, next-hop, exit-interface) "+
		" ,SmallFwTestPol1:EasyRule3_Applies(entry-interface, src-addr, src-addr_, dest-addr, dest-addr_, prot, msg, src-port, src-port_, dest-port, dest-port_, length, next-hop, exit-interface) "+
		" TUPLING";	
		Map<String, Set<String>> query20Result = new HashMap<String, Set<String>>();
		Set<String> q20_c1 = new HashSet<String>();
		q20_c1.add("smallfwtestpol1:easyrule3_applies[entry-interface, src-addr, src-addr_, dest-addr, dest-addr_, prot, msg, src-port, src-port_, dest-port, dest-port_, length, next-hop, exit-interface]");
		q20_c1.add("smallfwtestpol1:easyrule1_applies[entry-interface, src-addr, src-addr_, dest-addr, dest-addr_, prot, msg, src-port, src-port_, dest-port, dest-port_, length, next-hop, exit-interface]");
		q20_c1.add("smallfwtestpol1:accept[entry-interface, src-addr, src-addr_, dest-addr, dest-addr_, prot, msg, src-port, src-port_, dest-port, dest-port_, length, next-hop, exit-interface]");
		Set<String> q20_c2 = new HashSet<String>();
		q20_c2.add("smallfwtestpol1:easyrule3_applies[entry-interface, src-addr, src-addr_, dest-addr, dest-addr_, prot, msg, src-port, src-port_, dest-port, dest-port_, length, next-hop, exit-interface]");
		q20_c2.add("smallfwtestpol1:easyrule1_applies[entry-interface, src-addr, src-addr_, dest-addr, dest-addr_, prot, msg, src-port, src-port_, dest-port, dest-port_, length, next-hop, exit-interface]");
		q20_c2.add("smallfwtestpol1:accept[entry-interface, src-addr, src-addr_, dest-addr, dest-addr_, prot, msg, src-port, src-port_, dest-port, dest-port_, length, next-hop, exit-interface]");
		Set<String> q20_c3 = new HashSet<String>();
		q20_c3.add("smallfwtestpol1:easyrule3_applies[entry-interface, src-addr, src-addr_, dest-addr, dest-addr_, prot, msg, src-port, src-port_, dest-port, dest-port_, length, next-hop, exit-interface]");
		q20_c3.add("smallfwtestpol1:easyrule1_applies[entry-interface, src-addr, src-addr_, dest-addr, dest-addr_, prot, msg, src-port, src-port_, dest-port, dest-port_, length, next-hop, exit-interface]");
		q20_c3.add("smallfwtestpol1:accept[entry-interface, src-addr, src-addr_, dest-addr, dest-addr_, prot, msg, src-port, src-port_, dest-port, dest-port_, length, next-hop, exit-interface]");
		q20_c3.add("smallfwtestpol1:accept[entry-interface, dest-addr, src-addr_, src-addr, dest-addr_, prot, msg, src-port, src-port_, dest-port, dest-port_, length, next-hop, exit-interface]");
		q20_c3.add("smallfwtestpol1:drop[entry-interface, src-addr, src-addr_, dest-addr, dest-addr_, prot, msg, src-port, src-port_, dest-port, dest-port_, length, next-hop, exit-interface]");		
		q20_c3.add("ipaddress[src-addr]");
		query20Result.put("port[dest-port]", q20_c1);
		query20Result.put("port[src-port]", q20_c2);
		query20Result.put("port[src-addr]", q20_c3);
		
		// Can't trust ordering of internals
		// Need separate test functions for diff. outputs
		
				// TODO need to fix these test cases...
		
		
		// TODO ALL(v)
		
		//myInterface.debugParser = true;

		testCommandOutput(myInterface, "Query 1", query1, query1Result);
		testCommandOutput(myInterface, "Query 2", query2, query2Result);
		testCommandCount(myInterface, "Query 2a", query2a, query2aResult);
		testCommandOutput(myInterface, "Query 2b", query2b, query2bResult);
		testCommandOutput(myInterface, "Query 3", query3, query3Result);
		testCommandCount(myInterface, "Query 4", query4, query4Result);
				
		testCommandOutput(myInterface, "Query 4a", query4a, query4aResult);
		
		testCommandOutput(myInterface, "Query 4b", query4b, query4bResult);
		testCommandCount(myInterface, "Query 4c", query4c, query4cResult);		
		
		testCommandCount(myInterface, "Query 5", query5, query5Result);
		testCommandCount(myInterface, "Query 6", query6, query6Result);
		testCommandCount(myInterface, "Query 7", query7, query7Result);
		//testCommandOutput(myInterface, "Query 8", query8, query8Result);
		
		testCommandOutput(myInterface, "Query 8a", query8a, query8aResult);
		testCommandCount(myInterface, "Query 9", query9, query9Result);
		testCommandCount(myInterface, "Query 10", query10, query10Result);
		
		testCommandCount(myInterface, "Query 11", query11, query11Result);
		testCommandCount(myInterface, "Query 12", query12, query12Result);
		testCommandCount(myInterface, "Query 13", query13, query13Result);
		testCommandCount(myInterface, "Query 14", query14, query14Result);
		
		// Populated/Unpopulated
		testCommandPop(myInterface, "Query 15", query15, query15Result);		
		testCommandOutput(myInterface, "Query 15error", query15error, query15errorResult);		
		testCommandPop(myInterface, "Query 15a", query15a, query15aResult);		
		testCommandPop(myInterface, "Query 16", query16, query16Result);
		testCommandPop(myInterface, "Query 17", query17, query17Result);
		testCommandPop(myInterface, "Query 18", query18, query18Result);
		testCommandPop(myInterface, "Query 19", query19, query19Result);
		testCommandPop(myInterface, "Query 20", query20, query20Result);
		
		System.exit(1);
		
	}
	
	public static boolean testCommandPop(MEnvironment iface, String desc, String cmd, Map<String, Set<String>> expected)
	{
		Object result = iface.commandSilent(cmd);
		if(result instanceof Map<?, ?>)
		{
			Map<?, ?> resultmap = (Map<?, ?>) result;
			if(resultmap.equals(expected))
			{
				System.out.println("Passed: "+desc);			
				return true;
			}
		}
		
		System.out.println("***FAILED***: "+desc);
		System.out.println("Expected:");
		System.out.println(expected);
		System.out.println("Received:");
		System.out.println(result);
		return false;
	}
	
	public static boolean testCommandCount(MEnvironment iface, String desc, String cmd, int expectedModels)
	{
		
		Object result = iface.commandSilent(cmd);
		if(result instanceof MQuery)
		{
			MQuery qry = (MQuery) result;
			try
			{
				int actualModels = qry.countSatisfyingSolutions();
				if(actualModels != expectedModels)
				{
					System.out.println("***FAILED***: "+desc);
					System.out.println("Actual Models: "+actualModels+" vs. expected: "+expectedModels);
				}
			}
			catch(Exception e)
			{
				System.out.println("***FAILED***: "+desc);
				System.out.println(e);				
			}
			System.out.println("Passed: "+desc);
			return true;
		}
		
		System.out.println("***FAILED***: "+desc);
		System.out.println("  Result was not an MQuery object. Instead, got: "+result);
		return false;
	}
	
	public static boolean testCommandOutput(MEnvironment iface, String desc, String cmd, String expectedPrint)
	{
		
		PrintStream saveErr = iface.errorStream;
		PrintStream saveOut = iface.outStream;
		
		OutputStream testOutputStream = new ByteArrayOutputStream();
		PrintStream testPrintStream = new PrintStream(testOutputStream);
		iface.outStream = testPrintStream;
		iface.errorStream = testPrintStream;

		// NOT silent
		iface.command(cmd);
		
		String printed = testOutputStream.toString().trim();
		boolean result = printed.equals(expectedPrint);
		
		iface.outStream = saveOut;
		iface.errorStream = saveErr;
		
		if(result)
			System.out.println("Passed: "+desc);
		else
		{
			System.out.println("***FAILED***: "+desc);
			System.out.println("Expected:");
			System.out.println(expectedPrint);
			System.out.println("Received:");
			System.out.println(printed);
			System.out.println("Length "+expectedPrint.length()+" vs. "+printed.length());
			
			for(int ii = 0; (ii< printed.length())&&(ii<expectedPrint.length()); ii++)
			{
				if(printed.getBytes()[ii] != expectedPrint.getBytes()[ii])
				{
					System.out.println("First mismatch at index = "+ii);
					System.out.print(expectedPrint.getBytes()[ii]);
					System.out.print(" expected, got: ");
					System.out.print(printed.getBytes()[ii]);
					System.out.println();
					break;
				}
			}

		}
		
		
		return result;
	}
	
	public static MVocab setup3Vocab(String name, int numSubs, int numActs, int numRes)
	throws MGEBadIdentifierName, MGEUnknownIdentifier, MGEArityMismatch, 
	MGEBadQueryString, MGEBadCombinator, MGECombineVocabs, MGEUnsortedVariable
	{
		MVocab env = new MVocab(name);
		
		env.addDecision("Permit");
		env.addDecision("Deny");
		
		String subs = "";
		String acts = "";
		String res = "";
		for(int ii = 0; ii<numSubs; ii++)
			subs += "Sub"+ii+" ";
		for(int ii = 0; ii<numActs; ii++)
			acts += "Act"+ii+" ";
		for(int ii = 0; ii<numRes; ii++)
			res += "Res"+ii+" ";
		
		
		env.addSort("Subject", subs);		
		env.addSort("Action", acts);
		env.addSort("Resource", res);		
				
		env.addRequestVar("s", "Subject");
		env.addRequestVar("a", "Action");
		env.addRequestVar("r", "Resource");
						
		
		env.axioms.addConstraintDisjointAll("Action");
		env.axioms.addConstraintDisjointAll("Resource");

		env.axioms.addConstraintAbstract("Subject");
		env.axioms.addConstraintAbstract("Action");
		env.axioms.addConstraintAbstract("Resource");
		
		env.axioms.addConstraintAtMostOneAll("Action");
		
		return env;
	}
	
	public static void addEasy3Rules(MPolicyLeaf pol, MPolicyLeaf pol2, int numRules)
	throws MGEBadIdentifierName, MGEUnknownIdentifier, MGEArityMismatch, 
	MGEBadQueryString, MGEBadCombinator, MGECombineVocabs, MGEUnsortedVariable
	{
		ArrayList<String> templist = new ArrayList<String>();
		
		for(int ii=0;ii<numRules;ii++)
		{
			templist.clear();
			String decision = "Permit";
			if(ii % 2 == 0) decision = "Deny";
			
			templist.add("Sub"+ii+" s");
			templist.add("Act"+ii+" a");
			templist.add("Res"+ii+" r");
			
			pol.addRule("EasyRule"+ii, decision, templist);
			pol2.addRule("EasyRule"+ii, decision, templist);
						
		}
		
	}

	public static void do_time_tupling_new()
	throws MGEBadIdentifierName, MGEUnknownIdentifier, MGEArityMismatch, MGEBadQueryString, 
	MGEBadCombinator, MGECombineVocabs, MGEUnsortedVariable, MGEManagerException
	{
		// 1000 rules, vector size = 14
		MVocab fw = setupFirewallVocabNew("large");						
		MPolicyLeaf fwpol = new MPolicyLeaf("FwTestPol1", fw);
		MPolicyLeaf fwpol2 = new MPolicyLeaf("FwTestPol2", fw);
		addEasyRulesNew(fwpol, fwpol2, 1000);	
		
		ThreadMXBean mxBean = ManagementFactory.getThreadMXBean();

		
		//fwpol.printPolicyInfo();
		
		// One difference!
		// 1 solution (IP49, port9, UDP, entry-interface)
		ArrayList<String> templist = new ArrayList<String>();
		templist.clear();		
		templist.add("FastEthernet0 entry-interface");
		templist.add("prot-UDP prot");
		templist.add("IPBit0 src-addr");
		templist.add("IPBit1 src-addr"); // 0.0.0.3
		templist.add("PortBit6 src-port"); // +64
		templist.add("PortBit4 src-port"); // +16 = 80
		fwpol2.addRule("Difference2", "Accept", templist);
		
		fwpol.initIDBs();
		fwpol2.initIDBs();
		
		int numTrials = 1;
		
		System.out.println("Done creating policies.");
		
		// Now we have 2 policies (1000 rules each) under the _new_ vocab.
		// Now how long, tupled non-tupled, to do sat checks?
		
		// ************************************************************
		// Smallest model = 6
		// standard cia, no extra restrictions
		long start = mxBean.getCurrentThreadCpuTime();				
		MQuery qry = fwpol.compareWithPolicy(fwpol2);	
		qry.debug_verbosity = 2;
		long tuptime = 0;
		for(int iCount = 0; iCount < numTrials; iCount++)
		{
			MInstanceIterator sol = qry.runQuery().getTotalIterator();
			tuptime += sol.getQueryTuplingTime();
			sol.hasNext(); // is sat?
			
			if(iCount == numTrials-1)
				qry.prettyPrintOneSolution(); // MAY DOUBLECOUNT
		}
		long msTotal = (mxBean.getCurrentThreadCpuTime()-start)/1000000;
		System.out.println("rules = 1000; k = 14; sm = 6; NO; mean tup="+tuptime/numTrials+" mean is-sat="+msTotal/numTrials);
		
		// ************************************************************
		start = mxBean.getCurrentThreadCpuTime();
		qry.doTupling = true;
		tuptime = 0;
		for(int iCount = 0; iCount < numTrials; iCount++)
		{
			MInstanceIterator sol = qry.runQuery().getTotalIterator();
			tuptime += sol.getQueryTuplingTime();
			sol.hasNext(); // is sat?
			
			if(iCount == numTrials-1)
				qry.prettyPrintOneSolution();// MAY DOUBLECOUNT
		}
		msTotal = (mxBean.getCurrentThreadCpuTime()-start)/1000000;
		System.out.println("rules = 1000; k = 14; sm = 6; YES; mean tup="+tuptime/numTrials+" mean is-sat="+msTotal/numTrials);

		
		// Remains to axiomatize (iso -> eq)
		
		// the problem: now fmla size is much much bigger (oh -- around 32x bigger maybe?)
		// 381311 vs. ~10k
		
	}
	
	public static void do_time_tupling()
	throws MGEBadIdentifierName, MGEUnknownIdentifier, MGEArityMismatch, MGEBadQueryString, 
	MGEBadCombinator, MGECombineVocabs, MGEUnsortedVariable, MGEManagerException
	{
		// Time trials of tupling vs. non-tupling
		// Test both a small s/a/r policy and a larger firewall ACL
		// Since tupling's advantage depends on making the ceiling smaller,
		// test queries w/ diff. smallest model sizes.
		
		ArrayList<String> templist = new ArrayList<String>();
		
		// Test multiple times and take the average. Individual queries have
		// enormous std. dev. 
		int numTrials = 100;
		ThreadMXBean mxBean = ManagementFactory.getThreadMXBean();
		
		System.out.println("Starting time trials. numTrials="+numTrials);
		
		// 100 rules, vector size = 3
		MVocab voc3 = setup3Vocab("voc3", 100, 100, 100);
		MPolicyLeaf pol3a = new MPolicyLeaf("subactres1", voc3);
		MPolicyLeaf pol3b = new MPolicyLeaf("subactres2", voc3);
		addEasy3Rules(pol3a, pol3b, 100);
		
		templist.clear();		
		templist.add("Sub15 s");
		templist.add("Act10 a");
		templist.add("Res5 r");		
		pol3b.addRule("Difference2", "Permit", templist);
		
		
		pol3a.initIDBs();
		pol3b.initIDBs();
		
		// ************************************************************
		// Smallest model = 1	
		long start = mxBean.getCurrentThreadCpuTime();				
		int tuptime = 0;
		MQuery qry = pol3a.queryPolicy("(forsome s Subject (sub10 s))");		
		for(int iCount = 0; iCount < numTrials; iCount++)
		{
			MInstanceIterator sol = qry.runQuery().getTotalIterator();
			tuptime += sol.getQueryTuplingTime();
			sol.hasNext(); // is sat?
		//	if(iCount == numTrials-1)
		//		sol.prettyPrintOneSolution();
		}
		long msTotal = (mxBean.getCurrentThreadCpuTime()-start)/1000000;
		System.out.println("rules = 100; k = 3; sm = 1; NO; mean tup="+tuptime/numTrials+" mean is-sat="+msTotal/numTrials);
		
		
		// inflated because so few are used in this first one (just sub10)
		// not a fair test
		
		
		
		// 
		// ************************************************************
		start = mxBean.getCurrentThreadCpuTime();
		tuptime = 0;
		qry.doTupling = true;
		for(int iCount = 0; iCount < numTrials; iCount++)
		{
			MInstanceIterator sol = qry.runQuery().getTotalIterator();
			tuptime += sol.getQueryTuplingTime();
			sol.hasNext(); // is sat?
			
		//	if(iCount == numTrials-1)
			//	sol.prettyPrintOneSolution();
		}
		msTotal = (mxBean.getCurrentThreadCpuTime()-start)/1000000;
		System.out.println("rules = 100; k = 3; sm = 1; YES; mean tup="+tuptime/numTrials+" mean is-sat="+msTotal/numTrials);

		
		// ************************************************************
		// Smallest model = 3
		start = mxBean.getCurrentThreadCpuTime();				
		qry = pol3a.compareWithPolicy(pol3b);	
		tuptime = 0;
		for(int iCount = 0; iCount < numTrials; iCount++)
		{
			MInstanceIterator sol = qry.runQuery().getTotalIterator();
			tuptime += sol.getQueryTuplingTime();
			sol.hasNext(); // is sat?
		}
		msTotal = (mxBean.getCurrentThreadCpuTime()-start)/1000000;
		System.out.println("rules = 100; k = 3; sm = 3; NO; mean tup="+tuptime/numTrials+" mean is-sat="+msTotal/numTrials);
		
		// ************************************************************
		tuptime = 0;
		start = mxBean.getCurrentThreadCpuTime();
		qry.doTupling = true;
		for(int iCount = 0; iCount < numTrials; iCount++)
		{
			MInstanceIterator sol = qry.runQuery().getTotalIterator();
			tuptime += sol.getQueryTuplingTime();
			sol.hasNext(); // is sat?
		}
		msTotal = (mxBean.getCurrentThreadCpuTime()-start)/1000000;
		System.out.println("rules = 100; k = 3; sm = 3; YES; mean tup="+tuptime/numTrials+" mean is-sat="+msTotal/numTrials);
		
		// ************************************************************
		
		// FOR NOW lower
		numTrials = 10;
		System.out.println("numTrials for firewall tests = "+numTrials);
		
		// 1000 rules, vector size = 14
		MVocab fw = setupFirewallVocab("large", 1050, 60, 50);						
		MPolicyLeaf fwpol = new MPolicyLeaf("FwTestPol1", fw);
		MPolicyLeaf fwpol2 = new MPolicyLeaf("FwTestPol2", fw);
		addEasyRules(fwpol, fwpol2, 1000);	
		
		// One difference!
		// 1 solution (IP49, port9, UDP, entry-interface)
		templist.clear();		
		templist.add("FastEthernet0 entry-interface");
		templist.add("prot-UDP prot");
		templist.add("IP49 src-addr");		
		templist.add("Port9 src-port"); 
		fwpol2.addRule("Difference2", "Accept", templist);
		
		fwpol.initIDBs();
		fwpol2.initIDBs();

		
		// ************************************************************
		// Smallest model = 6
		// standard cia, no extra restrictions
		start = mxBean.getCurrentThreadCpuTime();				
		qry = fwpol.compareWithPolicy(fwpol2);	
		tuptime = 0;
		for(int iCount = 0; iCount < numTrials; iCount++)
		{
			MInstanceIterator sol = qry.runQuery().getTotalIterator();
			tuptime += sol.getQueryTuplingTime();
			sol.hasNext(); // is sat?
			
			//if(iCount == numTrials-1)
			//	sol.prettyPrintOneSolution();
		}
		msTotal = (mxBean.getCurrentThreadCpuTime()-start)/1000000;
		System.out.println("rules = 1000; k = 14; sm = 6; NO; mean tup="+tuptime/numTrials+" mean is-sat="+msTotal/numTrials);
		
		// ************************************************************
		start = mxBean.getCurrentThreadCpuTime();
		qry.doTupling = true;
		tuptime = 0;
		for(int iCount = 0; iCount < numTrials; iCount++)
		{
			MInstanceIterator sol = qry.runQuery().getTotalIterator();
			tuptime += sol.getQueryTuplingTime();
			sol.hasNext(); // is sat?
			
			//if(iCount == numTrials-1)
			//	sol.prettyPrintOneSolution();
		}
		msTotal = (mxBean.getCurrentThreadCpuTime()-start)/1000000;
		System.out.println("rules = 1000; k = 14; sm = 6; YES; mean tup="+tuptime/numTrials+" mean is-sat="+msTotal/numTrials);
		
		
		// Smallest model = 10
		// cia w/ != restrictions
		
		// ************************************************************
		start = mxBean.getCurrentThreadCpuTime();
		List<MIDBCollection> idbCollections = new ArrayList<MIDBCollection>();
		idbCollections.add(fwpol);
		idbCollections.add(fwpol2);
		String qryString = fwpol.getExistentialRequestPrefix();
		qryString += "(and (or ";
		qryString += "(and (fwtestpol1:accept " + fwpol.getRequestVarVector() +") (not (fwtestpol2:accept "+fwpol.getRequestVarVector()+"))) ";
		qryString += "(and (fwtestpol2:accept " + fwpol.getRequestVarVector() +") (not (fwtestpol1:accept "+fwpol.getRequestVarVector()+"))) ";
		qryString += "(and (fwtestpol1:drop " + fwpol.getRequestVarVector() +") (not (fwtestpol2:drop "+fwpol.getRequestVarVector()+"))) ";
		qryString += "(and (fwtestpol2:drop " + fwpol.getRequestVarVector() +") (not (fwtestpol1:drop "+fwpol.getRequestVarVector()+"))) ";

		/*env.addRequestVar("entry-interface", "Interface");
		env.addRequestVar("src-addr", "IPAddress");
		env.addRequestVar("src-addr_", "IPAddress");
		env.addRequestVar("dest-addr", "IPAddress");
		env.addRequestVar("dest-addr_", "IPAddress");
		env.addRequestVar("prot", "Protocol");
		env.addRequestVar("msg", "ICMPMessage");
		env.addRequestVar("src-port", "Port");
		env.addRequestVar("src-port_", "Port");
		env.addRequestVar("dest-port", "Port");
		env.addRequestVar("dest-port_", "Port");
		env.addRequestVar("length", "Length");
		env.addRequestVar("next-hop", "IPAddress");
		env.addRequestVar("exit-interface", "Interface");*/
		
		qryString += ") (not (= src-addr dest-addr)) (not (= src-port dest-port)) (not (= entry-interface exit-interface)) (not (= dest-addr next-hop)) (not (= src-addr next-hop)))";  
		
		qryString += fwpol.getRequestPrefixClosing();
		qry = MQuery.queryThesePolicies(qryString, idbCollections);
		qry.sizeCeiling = 14;
		
		tuptime = 0;
		for(int iCount = 0; iCount < numTrials; iCount++)
		{
			MInstanceIterator sol = qry.runQuery().getTotalIterator();
			tuptime += sol.getQueryTuplingTime();
			sol.hasNext(); // is sat?
			
		//	if(iCount == numTrials-1)
		//		sol.prettyPrintOneSolution();
		}
		msTotal = (mxBean.getCurrentThreadCpuTime()-start)/1000000;
		System.out.println("rules = 1000; k = 14; sm = 10; NO; mean tup="+tuptime/numTrials+" mean is-sat="+msTotal/numTrials);
		
		// ************************************************************
		start = mxBean.getCurrentThreadCpuTime();
		qry.doTupling = true;
		tuptime = 0;
		for(int iCount = 0; iCount < numTrials; iCount++)
		{
			MInstanceIterator sol = qry.runQuery().getTotalIterator();
			tuptime += sol.getQueryTuplingTime();
			sol.hasNext(); // is sat?
			
			//if(iCount == numTrials-1)
			//	sol.prettyPrintOneSolution();
		}
		msTotal = (mxBean.getCurrentThreadCpuTime()-start)/1000000;
		System.out.println("rules = 1000; k = 14; sm = 10; YES; mean tup="+tuptime/numTrials+" mean is-sat="+msTotal/numTrials);
		
		
		
		// Smallest model = 14
		// cia w/ more != restrictions
		
		// ************************************************************
		start = mxBean.getCurrentThreadCpuTime();
		qryString = fwpol.getExistentialRequestPrefix();
		qryString += "(and (or ";
		qryString += "(and (fwtestpol1:accept " + fwpol.getRequestVarVector() +") (not (fwtestpol2:accept "+fwpol.getRequestVarVector()+"))) ";
		qryString += "(and (fwtestpol2:accept " + fwpol.getRequestVarVector() +") (not (fwtestpol1:accept "+fwpol.getRequestVarVector()+"))) ";
		qryString += "(and (fwtestpol1:drop " + fwpol.getRequestVarVector() +") (not (fwtestpol2:drop "+fwpol.getRequestVarVector()+"))) ";
		qryString += "(and (fwtestpol2:drop " + fwpol.getRequestVarVector() +") (not (fwtestpol1:drop "+fwpol.getRequestVarVector()+"))) ";

		/*env.addRequestVar("entry-interface", "Interface");
		env.addRequestVar("src-addr", "IPAddress");
		env.addRequestVar("src-addr_", "IPAddress");
		env.addRequestVar("dest-addr", "IPAddress");
		env.addRequestVar("dest-addr_", "IPAddress");
		env.addRequestVar("prot", "Protocol");
		env.addRequestVar("msg", "ICMPMessage");
		env.addRequestVar("src-port", "Port");
		env.addRequestVar("src-port_", "Port");
		env.addRequestVar("dest-port", "Port");
		env.addRequestVar("dest-port_", "Port");
		env.addRequestVar("length", "Length");
		env.addRequestVar("next-hop", "IPAddress");
		env.addRequestVar("exit-interface", "Interface");*/
		
		qryString += ") (not (= src-addr_ dest-addr_)) (not (= src-addr src-addr_)) (not (= dest-addr dest-addr_)) (not (= dest-port dest-port_)) (not (= src-addr dest-addr)) (not (= src-port dest-port)) (not (= entry-interface exit-interface)) (not (= src-port_ dest-port_)) (not (= src-port dest-port_)) (not (= src-port src-port_)) (not (= dest-addr src-addr_)) (not (= dest-addr_ next-hop)) (not (= dest-addr next-hop)) (not (= src-addr next-hop)) (not (= src-addr_ next-hop)) (not (= src-addr dest-addr_)) (not (= src-port_ dest-port)))";  
		
		qryString += fwpol.getRequestPrefixClosing();
		qry = MQuery.queryThesePolicies(qryString, idbCollections);
		qry.sizeCeiling = 14;
		
		tuptime = 0;
		for(int iCount = 0; iCount < numTrials; iCount++)
		{
			MInstanceIterator sol = qry.runQuery().getTotalIterator();
			tuptime += sol.getQueryTuplingTime();
			sol.hasNext(); // is sat?
			
			//if(iCount == numTrials-1)
			//	sol.prettyPrintOneSolution();
		}
		msTotal = (mxBean.getCurrentThreadCpuTime()-start)/1000000;
		System.out.println("rules = 1000; k = 14; sm = 14; NO; mean tup="+tuptime/numTrials+" mean is-sat="+msTotal/numTrials);
		
		// ************************************************************
		start = mxBean.getCurrentThreadCpuTime();
		qry.doTupling = true;
		tuptime = 0;
		for(int iCount = 0; iCount < numTrials; iCount++)
		{
			MInstanceIterator sol = qry.runQuery().getTotalIterator();
			tuptime += sol.getQueryTuplingTime();
			sol.hasNext(); // is sat?
			
			//if(iCount == numTrials-1)
			//	sol.prettyPrintOneSolution();
		}
		msTotal = (mxBean.getCurrentThreadCpuTime()-start)/1000000;
		System.out.println("rules = 1000; k = 14; sm = 14; YES; mean tup="+tuptime/numTrials+" mean is-sat="+msTotal/numTrials);
	
		
	}
	
	public static void do_test_tupling_1()
	throws MGEBadIdentifierName, MGEUnknownIdentifier, MGEArityMismatch, 
	MGEBadQueryString, MGEBadCombinator, MGECombineVocabs, MGEUnsortedVariable, MGEManagerException
	{
		//do_test_child_sort_exhaustive();
		System.out.println("ENTERING TEST BLOCK: do_test_tupling_1()");
		do_test_tupling_basic();
		
		ArrayList<String> templist = new ArrayList<String>();
		
		System.out.println("\n\n\n\n\n***\n  Testing large-sized ACL performance (1000 rules). Creating policies...");
		
		// 50 ip addrs over 5 ranges, 10 ports
		//MGVocab fw = setupFirewallVocab("small", 50, 10, 10);
		//MGVocab fw = setupFirewallVocab("medium", 200, 40, 30);
		//MGVocab fw = setupFirewallVocab("medium-large", 610, 40, 40);
		MVocab fw = setupFirewallVocab("large", 1050, 60, 50);				
		
		MPolicyLeaf fwpol = new MPolicyLeaf("FwTestPol1", fw);
		MPolicyLeaf fwpol2 = new MPolicyLeaf("FwTestPol2", fw);
		
		
		addEasyRules(fwpol, fwpol2, 1000);
				
		//addEasyRules(fwpol, fwpol2, 800);
		
		//addEasyRules(fwpol, fwpol2, 600);
		
		// 500 is ok at default
		//addEasyRules(fwpol, fwpol2, 400);
		
		//addEasyRules(fwpol, fwpol2, 200);
		//addEasyRules(fwpol, fwpol2, 40);
		//addEasyRules(fwpol, fwpol2, 1);
		
		fwpol.rCombine = "FAC"; 
		fwpol.initIDBs();		
		fwpol2.rCombine = "FAC";
		fwpol2.initIDBs();
		
		System.out.println("Done creating policies. Running queries...");
				
		MQuery qry;
		ArrayList<MIDBCollection> pollist = new ArrayList<MIDBCollection>();
		pollist.add(fwpol);
		pollist.add(fwpol2);
						
		
		qry = fwpol.compareWithPolicy(fwpol2);	
		//qry.sizeCeiling = 14;
		qry.doTupling = true; 		
		qry.debug_verbosity = 2;
		// *************************
		// s/b 0 solutions
		
		MFormulaManager.printStatistics();
		//MGFormulaManager.printAtoms("ip59");
		
		
		//qry.prettyPrintSolutions();
		
		//countTest("Identical policies means no difference", qry, 1, 0, 1);

		// *************************
		// Add an un-well-sorted atom.
		// s/b 0 solutions (Port9 src-addr) is not possible.
		templist.clear();		
		templist.add("FastEthernet0 entry-interface");
		templist.add("prot-UDP prot");
		templist.add("IP49 src-addr");		
		templist.add("Port9 src-addr"); // test "never happens"
		fwpol.addRule("Difference1", "Drop", templist);
				
		fwpol.initIDBs();
		qry = fwpol.compareWithPolicy(fwpol2);		
		qry.debug_verbosity = 2;
		qry.doTupling = true;

				
		
		//qry.prettyPrintSolutions();
		//countTest("Impossible situation is impossible!", qry, 1, 0, 1);

		
		// *************************
		// Add a difference
		// 1 solution (IP49, port9, UDP, entry-interface)
		templist.clear();		
		templist.add("FastEthernet0 entry-interface");
		templist.add("prot-UDP prot");
		templist.add("IP49 src-addr");		
		templist.add("Port9 src-port"); 
		fwpol2.addRule("Difference2", "Accept", templist);
				
		fwpol2.initIDBs();
				

		qry = fwpol.compareWithPolicy(fwpol2);		
		qry.doTupling = true;
		qry.debug_verbosity = 2;		

		
		// TEST IDB tupling speed
		List<String> candidates = new ArrayList<String>();
		candidates.addAll(fwpol.getQualifiedIDBNameList());				
		candidates.addAll(fwpol2.getQualifiedIDBNameList());	
		
		// Remove _applies and decision IDBs
		List<String> idbsToOutput = new ArrayList<String>();
		for(String candidate : candidates)
		{
			if(candidate.toLowerCase().contains("easyrule") && !candidate.toLowerCase().contains("_applies"))
				idbsToOutput.add(candidate);
			if(candidate.toLowerCase().contains("difference")) // include the important rule
				idbsToOutput.add(candidate);
			if(candidate.toLowerCase().contains("accept") || candidate.toLowerCase().contains("drop"))
				idbsToOutput.add(candidate);
		}
		
		qry.addIDBOutputs(idbsToOutput);		
		List<String> indexing = new ArrayList<String>();
		for(int ii = 1; ii <= 14; ii++)
			indexing.add(String.valueOf(ii));		
		for(String idbname : idbsToOutput)
			qry.addIDBOutputIndexing(idbname, indexing);
		
		

		// TODO
		// occasional "unbound variable" problem, not always the same var either
		// sometimes entry-interface, sometimes prot...
		// occurs in OUTPUT?
		// MTotalInstanceIterator.prepareNext
		// from IDB output -- new quantifiers?
		
		// Ugh, cannot reproduce.
		
		
		//qry.prettyPrintSolutions();
		
		qry.prettyPrintOneSolution();
				

		ThreadMXBean mxBean = ManagementFactory.getThreadMXBean();
		long start = mxBean.getCurrentThreadCpuTime();		

		System.out.println("Starting to run 1000-rule change impact with 1 difference, including IDB output.");
		
		
		
		
		
		System.out.println("\n\n\n"+qry.countSatisfyingSolutions()+"\n\n\n");
		// affected by all the new equality preds?
		// Actually 1560 possible solutions? yes. but are they all equality-variants?
		//qry.prettyPrintOneSolution();
		//qry.prettyPrintSolutions(10);
		
		//countTest("Added one rule at end: One model in difference. (IDB output included).", qry, 1, 1, 1);					
		System.out.println("Above test is complete. Time for all preprocessing and Kodkod: " + 
				(mxBean.getCurrentThreadCpuTime() - start)/1000000 + " ms.\n");
		
		
		
		// Test to make sure the abstract constraint is working if all child sorts appear...			
		String longStr = "";
		for(int ii=240;ii<300;ii++)
			longStr += "(not (ip"+ii+" addr)) ";
		
		fwpol.vocab.axioms.addConstraintAbstract("iprange240_299");
		
		qry = MQuery.queryThesePolicies("(forsome addr iprange240_299  (and "+
				longStr + "))", pollist);
		qry.doTupling = true;
		
		// only this range is abstract in the new vocab...
		countTest("Abstract iprange240_299", qry, 1, 0, 1);		
		
		//System.out.println("Beginning to test multiple executions of a single small query. (Only one vocab involved; no combining needed.)");
		
		List<MIDBCollection> singlePol = new ArrayList<MIDBCollection>(1);
		singlePol.add(fwpol);
		qry = MQuery.queryThesePolicies(fwpol.getExistentialRequestPrefix() +
				" (FwTestPol1:easyrule500_Applies " + fwpol.getRequestVarVector() + ")"
				+ fwpol.getRequestPrefixClosing()
				, singlePol);
		qry.doTupling = true;
		
	/*	start = mxBean.getCurrentThreadCpuTime();
		int numTries = 200;
		for(int iCount=0;iCount<numTries;iCount++)
		{
			if(iCount == 0)
				qry.debug_verbosity = 2;
			else
				qry.debug_verbosity = 0;
			System.out.print(".");
			qry.isQuerySatisfiable();
			
			// note: each iteration WITHOUT TUPLING takes a few seconds to run (but not super long)
		}
		System.out.println("\nDone. Time to run the query "+numTries+" times: "+ (mxBean.getCurrentThreadCpuTime() - start) / 1000000);
		System.out.println("Average time: "+((mxBean.getCurrentThreadCpuTime() - start) / 1000000) / numTries);*/
		
	}
	
	public static void testxacml(String mgDirectory, String xacml20SchemaFile) 
	throws MGEBadIdentifierName, MGEBadCombinator, MGEUnsupportedXACML, MGEUnknownIdentifier, MGEArityMismatch,
	MGEBadQueryString, MGEManagerException
	{
		// BEWARE: Filenames are case-sensitive.
		
		// Also may have to deal with / instead of \
		// File.separator 
		
		mgDirectory = MPolicy.convertSeparators(mgDirectory);
		xacml20SchemaFile = MPolicy.convertSeparators(xacml20SchemaFile);		
				
		// Test XACML 2.0 interface
		XACML20Reader.doComplianceTests(xacml20SchemaFile, mgDirectory+File.separator+"xacml2.0-ct-v.0.4"+File.separator);
		
		
		
		// Test XACML 1.0 interface (via Sun's implementation)		
		MPolicy pol = MPolicy.readXACML(mgDirectory+File.separator+
				                         "tests"+File.separator+
				                         "xacml"+File.separator+
				                         "continue"+File.separator+
				                         "CodeB"+File.separator+
				                         "RPSlist.xml", xacml20SchemaFile);
		
		if(!"rpslist".equals(pol.name) ||
		   !"MGPolicySet".equals(pol.getClass().getName()))
			System.err.println("Failed XACML 1.0 interface test!");
		else
			System.out.println("Passed XACML 1.0 interface test.");
		
	}
	
	public static void main(String[] args)
	  throws MGEUnknownIdentifier, MGEArityMismatch,
      MGEBadCombinator, MGECombineVocabs, MGEUnsortedVariable, MGEBadQueryString, 
      MGEBadIdentifierName, UnknownIdentifierException, ParsingException, 
      MGEUnsupportedXACML, MGEManagerException, MGEUnsupportedSQS,
      UnsupportedFormulaException
	{
				
	   /* Variable v = Variable.unary("v");
		Relation r = Relation.unary("r");
		
		WeakReference ref1 = new WeakReference<Variable>(v);
		WeakReference ref2 = new WeakReference<Variable>(v);
		
		System.out.println(ref1.equals(ref2));
		
		// ok, so references aren't equal based on referents.
		String x = "foo";
		ref1 = new WeakReference(x);
		ref2 = new WeakReference(x);
		
		System.out.println(ref1.equals(ref2));
		
		 Set<Formula> weakHashSet1 = Collections.newSetFromMap(
			        new WeakHashMap<Formula, Boolean>());

		 Set<Formula> weakHashSet2 = Collections.newSetFromMap(
			        new WeakHashMap<Formula, Boolean>());
		
		 Formula f = v.in(r);
		 
			System.out.println(weakHashSet1.size());
		weakHashSet1.add(f);
		weakHashSet2.add(f);
		
		System.out.println(weakHashSet1.equals(weakHashSet2));
		
		Set<Formula> strongSet = new HashSet<Formula>();
		strongSet.add(f);
		System.out.println();
		System.out.println(strongSet.equals(weakHashSet1));
		System.out.println(weakHashSet1.equals(strongSet));
		System.out.println(weakHashSet1.size());
		
		System.out.println(weakHashSet1);
		f = Formula.TRUE;
		
		System.gc();
						
		// cannot trust size()? but contains and empty are trustworthy
		System.out.println(weakHashSet1.size());
		System.out.println(weakHashSet1);
		System.out.println(weakHashSet1.size());
		System.out.println(weakHashSet1.contains(f));
		System.out.println(weakHashSet1.isEmpty());
			
		
	
		// Test common sub-exp removal
		
		Formula f1 = v.in(r);
		Formula f2 = v.in(r);
		
		BinaryFormula conj = (BinaryFormula) f1.and(f2);
							
		System.out.println(conj.left().hashCode());
		System.out.println(conj.right().hashCode());
		
		
		//System.exit(1);
		
		
		// cool idea: canonical weak ref in an object itself... but can't do that for Formula. (Without wrapper, anyway) Pity...
		
		HashMap<Formula, Integer> hm1 = new HashMap<Formula, Integer>();
		HashMap<Formula, Integer> hm2 = new HashMap<Formula, Integer>();
		
		System.out.println(hm1.equals(hm2));
		
		hm1.put(f2, Integer.valueOf(2));
		hm1.put(f, Integer.valueOf(1));
		
		System.out.println(hm1.equals(hm2));
		
		hm2.put(f, Integer.valueOf(1));
		hm2.put(f2, Integer.valueOf(2));		
		System.out.println(hm1.equals(hm2));
		
		System.exit(1); */
		
		/*MWeakArrayVector<Variable> arr1 = new MWeakArrayVector<Variable>(2);
		MWeakArrayVector<Variable> arr2 = new MWeakArrayVector<Variable>(2);
		
		Variable v1 = Variable.unary("v1");
		Variable v2 = Variable.unary("v2");
		Variable v3 = Variable.unary("v3");
		Variable v4 = Variable.unary("v4");
		Variable v5 = Variable.unary("v5");
		
		arr1.set(0, v1);
		arr2.set(0, v1);
		
		System.out.println(arr1.equals(arr2));
		System.out.println(arr1.hashCode() + " " + arr2.hashCode());
		
		arr1.set(1, v2);

		System.out.println(arr1.equals(arr2));
		System.out.println(arr1.hashCode() + " " + arr2.hashCode());

		arr2.set(1, v2);
		
		System.out.println(arr1.equals(arr2));
		System.out.println(arr1.hashCode() + " " + arr2.hashCode());
		
		System.exit(1);*/

		// do_time_tupling_new();
		// System.exit(1);
		
		// do_time_tupling();
		// System.exit(1);
		
	//	benchmarkXACML();
	//	System.exit(1);
		
		 /*
		  * Starting time trials. numTrials=100
rules = 100; k = 3; sm = 1; NO; mean tup=0 mean is-sat=141
rules = 100; k = 3; sm = 1; YES; mean tup=17 mean is-sat=19
rules = 100; k = 3; sm = 3; NO; mean tup=0 mean is-sat=691
rules = 100; k = 3; sm = 3; YES; mean tup=64 mean is-sat=212
numTrials for firewall tests = 10
rules = 1000; k = 14; sm = 6; NO; mean tup=0 mean is-sat=7923
rules = 1000; k = 14; sm = 6; YES; mean tup=457 mean is-sat=1053
rules = 1000; k = 14; sm = 10; NO; mean tup=0 mean is-sat=18239
rules = 1000; k = 14; sm = 10; YES; mean tup=493 mean is-sat=1115
rules = 1000; k = 14; sm = 14; NO; mean tup=0 mean is-sat=32751
rules = 1000; k = 14; sm = 14; YES; mean tup=558 mean is-sat=1148
		  */
		 
		try {
			FormulaSigInfo.unitTests();
		} catch (NotASortException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
				
		runTests();
		
		// kodkod time varies wildly. (as low as 1765ms, as high as nearly 8 sec. WHY?)
		
		// Everything from tests should be out of scope now.
		// Test that the weak references in the manager are doing their job.
		System.gc();
		System.out.println("State of Formula Manager after forced gc call: ");
		MFormulaManager.printStatistics();
		
		System.gc();
		MFormulaManager.ping();
		System.gc();
		MFormulaManager.printStatistics();
		
		/*
		 * Leave everything after the above printStatistics() call empty.
		 */
	}
	
	public static void benchmarkXACML(String continueFileName)
    throws MGEBadIdentifierName, MGEBadCombinator, MGEUnsupportedXACML, MGEUnknownIdentifier,
    MGEArityMismatch, MGEBadQueryString, MGEManagerException, MGECombineVocabs, MGEUnsortedVariable
    {
        ThreadMXBean mxBean = ManagementFactory.getThreadMXBean();
        long start = mxBean.getCurrentThreadCpuTime();
        
        int numTrials = 100;
        
        System.out.println("Beginning XACML benchmark.");
        
        // Load the CONTINUE policy. Do it n times.
        for(int ii = 0; ii<numTrials; ii++)
        {            
            MPolicy.readXACML10(continueFileName);
            
           // System.out.println(foo.idbs.keySet().size());
            //System.out.println(foo.idbs.keySet());
            
            // First test, make sure that data structures aren't being reused and saving us a bunch of time
            // on all but first iteration:
            //if(ii % 10 == 0)
            	System.out.println( ((mxBean.getCurrentThreadCpuTime()-start)/1000000) / (ii+1));
            	
            MFormulaManager.clearAll();
            System.gc();
        }
        
        System.out.println("Average time to load policy: "+((mxBean.getCurrentThreadCpuTime()-start)/1000000)/numTrials+
        		"ms. Number of trials was: "+numTrials);
        
        // Tests on the policy
        MPolicy continuePolicy = MPolicy.readXACML10(continueFileName);
    
        start = mxBean.getCurrentThreadCpuTime();        
        for(int ii = 0; ii<numTrials; ii++)
        {
        	MQuery q = continuePolicy.queryPolicy("(forsome s Subject (forsome a Action (forsome r Resource (forsome e Environment (RPSlist:Permit s a r e)))))");
        	q.isQuerySatisfiable();
        	System.out.println( ((mxBean.getCurrentThreadCpuTime()-start)/1000000) / (ii+1));
        	
        }
        
        System.out.println("Average time to run basic permit query: "+((mxBean.getCurrentThreadCpuTime()-start)/1000000)/numTrials+
        		"ms. Number of trials was: "+numTrials);

  
        
        start = mxBean.getCurrentThreadCpuTime();        
        for(int ii = 0; ii<numTrials; ii++)
        {
        	MQuery q = continuePolicy.queryPolicy("(forsome s Subject (forsome a Action (forsome r Resource (forsome e Environment (RPSlist:Permit s a r e)))))");
        	q.doTupling = true;
        	q.isQuerySatisfiable();
        	System.out.println( ((mxBean.getCurrentThreadCpuTime()-start)/1000000) / (ii+1));
        	
        }
        
        System.out.println("Average time to run basic permit query WITH TUPLING: "+((mxBean.getCurrentThreadCpuTime()-start)/1000000)/numTrials+
        		"ms. Number of trials was: "+numTrials);

        
        
    }
	
}




