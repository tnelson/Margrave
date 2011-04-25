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

import kodkod.ast.Formula;


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
		idbs.clear();						
				
		// Prepare the IDBs of child policies
		for(MPolicy dc : children)
		{
			dc.initIDBs();			
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
				if(dc.idbs.containsKey(dec))
					preComb.put(dec, MFormulaManager.makeOr(preComb.get(dec), 
							MFormulaManager.makeAnd(dc.idbs.get(dec), dc.target)));
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
			
			// FIRST-APPLICABLE
			if(pCombineFA.contains(dec))
			{
				Set<Formula> thisdec = new HashSet<Formula>(); // disj
				Set<Formula> negpriortargets = new HashSet<Formula>(); // conj
													
				// Order matters. Make sure to use the correct ordering.
				for(MPolicy child : children)
				{
					// This decision applies (due to child) if
					// (1) No older child's target applied
					// (2) The child's IDB for this decision applies
					// (3) The child's target applies.
					thisdec.add(MFormulaManager.makeAnd(MFormulaManager.makeAnd(MFormulaManager.makeConjunction(negpriortargets), 
							child.idbs.get(dec)), 
							child.target));

						
					// !!! TN disabled for now. Not clear how to separate desired semantics (matches vs. applies) vis-a-vis policy targets.
					
					// Child exposes some IDBs. 
					// We must expose them, after making sure CHILD'S target applies, and no older children grabbed the mic.
					//for(String idbname : child.idbs.keySet())						
					//	idbs.put(child.name+":"+idbname, MFormulaManager.makeAnd(child.idbs.get(idbname), 
					//			                                     MFormulaManager.makeAnd(child.target,
					//			                                    		 MFormulaManager.makeConjunction(negpriortargets))));
					
					// Younger children must respect their elders.
					negpriortargets.add(MFormulaManager.makeNegation(child.target));										
				}
					
				idbs.put(dec, MFormulaManager.makeDisjunction(thisdec));				
			}
			
			// OVERRIDES
			else if(pCombineWhatOverrides.containsKey(dec))
			{
				
				// In PolicyLeaf the formulas for rules were all available at the start. 
				// Here we have something similar: we can derive proto-decisions (just "matches")
				// --- preComb
				
				idbs.put(dec, preComb.get(dec));
				for(String dOverrides : pCombineWhatOverrides.get(dec))
				{
					// If A > B, anytime A matches, B can't hold (even if A is itself overridden)
					idbs.put(dec, MFormulaManager.makeAnd(idbs.get(dec), 
							MFormulaManager.makeNegation(preComb.get(dOverrides))));
				}										
				
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
		if(idbname.startsWith(this.name+":")) // name is already lowercase
		{
			idbname = idbname.substring(this.name.length()+1);
		}	
	
		// Defer to child
		String[] split = idbname.split(":");
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
}
