/*
  	Copyright 2009-2010 Brown University and Worcester Polytechnic Institute.
    
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

import java.util.*;

import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

import com.sun.xacml.combine.CombiningAlgorithm;
import com.sun.xacml.combine.DenyOverridesPolicyAlg;
import com.sun.xacml.combine.DenyOverridesRuleAlg;
import com.sun.xacml.combine.FirstApplicablePolicyAlg;
import com.sun.xacml.combine.FirstApplicableRuleAlg;
import com.sun.xacml.combine.PermitOverridesPolicyAlg;
import com.sun.xacml.combine.PermitOverridesRuleAlg;

import kodkod.ast.Expression;
import kodkod.ast.Formula;
import kodkod.ast.Relation;
import kodkod.ast.Variable;


public class MPolicySet extends MPolicy
{
	// (List, rather than Set or HashMap, because we need ordering for first-applicable.)
	List<MPolicy> children = new LinkedList<MPolicy>();

	// See MPolicyLeaf for more information 
	Set <String> pCombineFA = new HashSet<String>();
	Map<String, Set<String>> pCombineWhatOverrides = new HashMap<String, Set<String>>();

	
	public MPolicySet(String n, MVocab env)
	{
		super(n, env);				
	}
	
	public void addChild(MPolicy ch)
	{
		children.add(ch); // allow adding multiple times
	}	
	
	void handleXACML2Combine(Node combAlgNode)
	throws MGEUnsupportedXACML
	{
		String result = "";

		NodeList children = combAlgNode.getChildNodes();

		if(children == null)
			throw new MGEUnsupportedXACML("Combination algorithm not provided in expected way by XACML policy.");
		if(children.getLength() != 1)
			throw new MGEUnsupportedXACML("Combination algorithm not provided in expected way by XACML policy.");

		result = children.item(0).getNodeValue();
		
		if("urn:oasis:names:tc:xacml:1.0:policy-combining-algorithm:deny-overrides".equals(result) ||
				"urn:oasis:names:tc:xacml:1.1:policy-combining-algorithm:ordered-deny-overrides".equals(result))
		{
			Set<String> denySet = new HashSet<String>();
			denySet.add("Deny");
			pCombineWhatOverrides.put("Permit", denySet);
		}

		else if("urn:oasis:names:tc:xacml:1.0:policy-combining-algorithm:permit-overrides".equals(result) ||
				"urn:oasis:names:tc:xacml:1.1:policy-combining-algorithm:ordered-permit-overrides".equals(result))
		{
			Set<String> permSet = new HashSet<String>();
			permSet.add("Permit");
			pCombineWhatOverrides.put("Deny", permSet);
		}

		else if("urn:oasis:names:tc:xacml:1.0:policy-combining-algorithm:first-applicable".equals(result))
		{
			pCombineFA.add("Permit");
			pCombineFA.add("Deny");	
		}

		/*
		 * The "Only-one-applicable" policy-combining algorithm only applies to policies. The
	414 result of this combining algorithm ensures that one and only one policy or policy set is applicable
	415 by virtue of their targets. If no policy or policy set applies, then the result is "NotApplicable", but if
	416 more than one policy or policy set is applicable, then the result is "Indeterminate". When exactly
	417 one policy or policy set is applicable, the result of the combining algorithm is the result of
	418 evaluating the single applicable policy or policy set.
		 */
		else if("urn:oasis:names:tc:xacml:1.0:policy-combining-algorithm:only-one-applicable".equals(result))
			throw new MGEUnsupportedXACML("The only-one-applicable policy combination algorithm is not currently supported.");		
	
		else
			throw new MGEBadCombinator("Unsupported rule combining algorithm: "+result);	


	}
	
	void handleXACMLCombine(CombiningAlgorithm ca) throws MGEBadCombinator
	{
		if(ca instanceof DenyOverridesPolicyAlg || ca instanceof DenyOverridesRuleAlg)
		{
			Set<String> denySet = new HashSet<String>();
			denySet.add("Deny");
			pCombineWhatOverrides.put("Permit", denySet);
		}
		else if(ca instanceof PermitOverridesPolicyAlg || ca instanceof PermitOverridesRuleAlg)
		{
			Set<String> permSet = new HashSet<String>();
			permSet.add("Permit");
			pCombineWhatOverrides.put("Deny", permSet);

		}
		else if(ca instanceof FirstApplicablePolicyAlg || ca instanceof FirstApplicableRuleAlg)
		{
			pCombineFA.add("Permit");
			pCombineFA.add("Deny");			
		}
				
		throw new MGEBadCombinator("Unsupported combining algorithm: "+ca);		
	}
	
	public void initIDBs() throws MUserException
	{
		// Do NOT call this!
		//super.initIDBs();
		
		// Recalling this method should "reset" all IDBs. Start out with a clean slate:
		decisions.clear();
		clearIDBs();						
				
		// Prepare the IDBs of child policies
		for(MPolicy dc : children)
		{
			dc.initIDBs();	
			
			// Add decisions AFTER initIDBs call, since a Set needs to init before discovering what decisions it has.
			decisions.addAll(dc.decisions);
		}

		if(pCombineFA.size() > 0 && pCombineWhatOverrides.size() > 0)
		{
			// For now, don't allow both kinds of *policy* combinators to be active at once.
			// TODO allow both FA&O in pcomb
			throw new MGEBadCombinator("A policy set cannot have both first-applicable and overrides combinators.");
		}

		// Decisions pre-combinators
		Map<String, Formula> preComb = new HashMap<String, Formula>();
		for(String dec : decisions)
		{	
			preComb.put(dec, Formula.FALSE);
			for(MPolicy dc : children)
			{
				// Don't forget to include the child's target!
				if(dc.containsIDB(dec))
					preComb.put(dec, MFormulaManager.makeOr(preComb.get(dec), 
							MFormulaManager.makeAnd(dc.getIDB(dec), dc.target)));
			}
		}
		
		
		// Unlike rule-combination, there are no _matches/_applies IDBs to create.
		// Need to propagate up the decisions of the children. Decisions will not
		// be set for this policy until this method is called.
		
		// (a) FA
		// If XACML, FA means first applicable target.
		// If not XACML, FA means first to give non-N/a
		
		// (b) Overrides
		// Same in XACML and non-XACML. 
		
		for(String dec : decisions)
		{
			// Construct this decision IDB.
			//System.err.println("------\nConstructing decision: "+dec);
			
			// FIRST-APPLICABLE
			if(pCombineFA.contains(dec))
			{
				Set<Formula> thisdec = new HashSet<Formula>(); // disj
				Set<Formula> negpriortargets = new HashSet<Formula>(); // conj
													
				// Order matters. Make sure to use the correct ordering.
				for(MPolicy child : children)
				{
					//System.err.println("  Processing: "+child.name);
					//System.err.println("    negpriortargets: "+negpriortargets);
					//System.err.println("    idbs of this child: "+child.idbs);
					
					// This decision applies (due to child) if
					// (1) No older child's target applied
					// (2) The child's IDB for this decision applies
					// (3) The child's target applies.
					if(child.containsIDB(dec))
					{
						thisdec.add(MFormulaManager.makeAnd(MFormulaManager.makeAnd(MFormulaManager.makeConjunction(negpriortargets), 
								child.getIDB(dec)), 
								child.target));
					}

						
					// !!! TN disabled for now. Not clear how to separate desired semantics (matches vs. applies) vis-a-vis policy targets.
					
					// Child exposes some IDBs. 
					// We must expose them, after making sure CHILD'S target applies, and no older children grabbed the mic.
					//for(String idbname : child.idbs.keySet())						
					//	idbs.put(child.name+MEnvironment.sIDBSeparator+idbname, MFormulaManager.makeAnd(child.idbs.get(idbname), 
					//			                                     MFormulaManager.makeAnd(child.target,
					//			                                    		 MFormulaManager.makeConjunction(negpriortargets))));
					
					// Younger children must respect their elders.
					// (Even if this child has no concept of this decision, if it "applies" (i.e., target matches) it overrules.)
					negpriortargets.add(MFormulaManager.makeNegation(child.target));										
				}
					
				putIDB(dec, MFormulaManager.makeDisjunction(thisdec));				
			}
			
			// OVERRIDES
			else if(pCombineWhatOverrides.containsKey(dec))
			{
				
				// In PolicyLeaf the formulas for rules were all available at the start. 
				// Here we have something similar: we can derive proto-decisions (just "matches")
				// --- preComb
				
				putIDB(dec, preComb.get(dec));
				for(String dOverrides : pCombineWhatOverrides.get(dec))
				{
					// If A > B, anytime A matches, B can't hold (even if A is itself overridden)
					putIDB(dec, MFormulaManager.makeAnd(getIDB(dec), 
							MFormulaManager.makeNegation(preComb.get(dOverrides))));
				}										
				
			}
			
			// NO OP
			else
			{
				// no-op. disjunction of all child policies' idbs for this decision
				
				Set<Formula> childrenHave = new HashSet<Formula>();
				
				for(MPolicy ch : children)
				{
					if(ch.containsIDB(dec))
						childrenHave.add(ch.getIDB(dec));
				}
				
				putIDB(dec, MFormulaManager.makeDisjunction(childrenHave));
			}
				
		}
			
		
	} // end initIDBs

	public void printPolicyInfo()
	{
		MEnvironment.errorWriter.println("###########################");
		MEnvironment.errorWriter.println("Policy Name: "+name);
		MEnvironment.errorWriter.println("This is a policy SET with policy combinator: "+printCombinators(pCombineFA, pCombineWhatOverrides));
		MEnvironment.errorWriter.println("Target Formula: "+target);		
		
		String cstr = "";
		for(MPolicy child : children)
		{
			if(cstr.length() < 1)
				cstr = child.name;
			else
				cstr = cstr + " " + child.name;
		}
		
		MEnvironment.errorWriter.println("Children: "+cstr);					
		
		MEnvironment.errorWriter.println("\nIDB and EDB predicates available for use in queries:");
		
		MEnvironment.errorWriter.println("IDBs:");		
		prettyPrintIDBs();		
		prettyPrintEDBs();
		MEnvironment.errorWriter.print("\n\n");
		
		MEnvironment.errorWriter.println("Policy-level constraints: ");
		
		MEnvironment.errorWriter.println("###########################\n");
	}
	
	public String getDecisionForRuleIDBName(String idbname)
	{
		// may pass canonical idb name ("PolicyName:Rule12" instead of "Rule12")
		if(idbname.startsWith(this.name+MEnvironment.sIDBSeparator)) // name is already lowercase
		{
			idbname = idbname.substring(this.name.length()+1);
		}	
	
		// Defer to child
		String[] split = idbname.split(MEnvironment.sIDBSeparator);
		String polName = split[0].toLowerCase();
		
		for(MPolicy pol : children)
		{
			if(pol.name.equals(polName))			
				return pol.getDecisionForRuleIDBName(idbname);
		}
		
		return "";
	}

	public List<String> ruleIDBsWithHigherPriorityThan(String rulename)
	{	
		return new LinkedList<String>(); // unsupported for now
	}
	
	public static void unitTests()
	{
		MEnvironment.writeErrLine("----- Begin MPolicySet Tests (No messages is good.) -----");
		
		MVocab voc = new MVocab();
		voc.addSort("Subject");
		voc.addSort("Action");
		voc.addSort("Resource");		
		voc.addPredicate("ownerOf", "Subject Resource");
		voc.addPredicate("restricted", "Resource");
		voc.addSubSort("Subject", "Admin");
		voc.addSubSort("Subject", "Employee");
		voc.addSubSort("Action", "BadThing");
		voc.addSubSort("Resource", "Computer");
		voc.addSubSort("Resource", "Bandwidth");
		// Bandwidth and Computer can overlap.
		voc.addSubSort("Computer", "BandCompLB");
		voc.addSubSort("Bandwidth", "BandCompLB");		
		
		voc.axioms.addConstraintAbstract("Subject");

		/////////////////////////////////////////////////////////////
		MPolicyLeaf pol1 = new MPolicyLeaf("Test Policy 1", voc);
				
		List<String> vSAR = new ArrayList<String>();
		vSAR.add("s"); vSAR.add("a"); vSAR.add("r");
		
		List<String> vSR = new ArrayList<String>();
		vSR.add("s"); vSR.add("r");
		
		Variable s = MFormulaManager.makeVariable("s");
		Variable a = MFormulaManager.makeVariable("a");
		Variable r = MFormulaManager.makeVariable("r");
		Map<Variable, Expression> varSorts = new HashMap<Variable, Expression>();
		varSorts.put(s, voc.getSort("Subject").rel);
		varSorts.put(a, voc.getSort("Action").rel);
		varSorts.put(r, voc.getSort("Resource").rel);
		
		Relation admin = voc.getSort("Admin").rel;		
		Relation ownerOf = voc.predicates.get("ownerOf").rel;
		Relation restricted = voc.predicates.get("restricted").rel;
		Formula aTarget;
		
		aTarget = MFormulaManager.makeAtom(s, admin);		
		pol1.addRule("Rule1", "permit", vSAR, aTarget, Formula.TRUE);
		aTarget = MFormulaManager.makeAtom(MFormulaManager.makeExprTuple(vSR), ownerOf);
		pol1.addRule("Rule2", "permit", vSAR, aTarget, Formula.TRUE);		
		aTarget = MFormulaManager.makeAtom(r, restricted);
		pol1.addRule("Rule3", "deny", vSAR, aTarget, Formula.TRUE);
				
		pol1.rCombineFA.clear();
		Set<String> sDeny = new HashSet<String>();
		sDeny.add("deny");
		pol1.rCombineWhatOverrides.put("permit", sDeny);				
		
		/////////////////////////////////////////////////////////////

		MPolicyLeaf pol2 = new MPolicyLeaf("Test Policy 2", voc);
		Relation BadThing = voc.getSort("BadThing").rel;
		aTarget = MFormulaManager.makeAtom(a, BadThing);		
		pol2.addRule("p2Rule1", "callpolice", vSAR, aTarget, Formula.TRUE);
		
		
		
		
		/////////////////////////////////////////////////////////////

		MPolicySet polparent = new MPolicySet("Test Parent Policy", voc);
		
		// Target VERY important here, since it dictates applicability of each policy.
		Relation Computer = voc.getSort("Computer").rel;
		Relation Bandwidth = voc.getSort("Bandwidth").rel;
		pol1.target = MFormulaManager.makeAtom(r, Computer);	 
		pol2.target = MFormulaManager.makeAtom(r, Bandwidth);	
		
		polparent.addChild(pol1);
		polparent.addChild(pol2);
		/////////////////////////////////////////////////////////////
		
		// First-applic. combination of child policies
		polparent.pCombineFA.add("permit"); polparent.pCombineFA.add("deny"); polparent.pCombineFA.add("callpolice");
		polparent.initIDBs();
		
		/////////////////////////////////////////////////////////////
		
		// Test
		//polparent.prettyPrintIDBs();
		
		/////////////////////////////////////////////////////////////
		// (1) We're FA, so callpolice requires a non-Computer request (pol1 handles those).
		Formula testFmla1 = polparent.getIDB("callpolice");
		testFmla1 = MFormulaManager.makeAnd(testFmla1, MFormulaManager.makeAtom(r, Computer));		
		
		if(MJavaTests.testIsSatisfiable(testFmla1, voc, 3, varSorts))
			MEnvironment.errorWriter.println("********** MPolicySet test 1: FAILED!");

		/////////////////////////////////////////////////////////////
		// (2) But can still satisfy callpolice 
		Formula testFmla2 = polparent.getIDB("callpolice");			
		
		if(!MJavaTests.testIsSatisfiable(testFmla2, voc, 3, varSorts))
			MEnvironment.errorWriter.println("********** MPolicySet test 2: FAILED!");
		
		
		
		/////////////////////////////////////////////////////////////
		// overrides PComb now.
		polparent.pCombineFA.clear();
		Set<String> sCallPolice = new HashSet<String>();
		sCallPolice.add("callpolice");
		Set<String> sDCP = new HashSet<String>();
		sDCP.add("callpolice"); 		sDCP.add("deny");
		polparent.pCombineWhatOverrides.put("deny", sCallPolice);
		polparent.pCombineWhatOverrides.put("permit", sDCP);
		polparent.initIDBs();
		
		//polparent.prettyPrintIDBs();
		
		
		/////////////////////////////////////////////////////////////
		// (3) Now have CP > Deny > Permit. The callpolice decision now applies 
		// even for a Computer (pol1's target) request.
		Formula testFmla3 = polparent.getIDB("callpolice");			
		testFmla3 = MFormulaManager.makeAnd(testFmla3, MFormulaManager.makeAtom(r, Computer));	
		
		if(!MJavaTests.testIsSatisfiable(testFmla3, voc, 3, varSorts))
			MEnvironment.errorWriter.println("********** MPolicySet test 3: FAILED!");
				
		MEnvironment.writeErrLine("----- End MPolicySet Tests -----");	

	}
	
}
