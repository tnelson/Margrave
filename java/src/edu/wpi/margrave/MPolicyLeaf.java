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
import com.sun.xacml.combine.OrderedDenyOverridesPolicyAlg;
import com.sun.xacml.combine.OrderedDenyOverridesRuleAlg;
import com.sun.xacml.combine.OrderedPermitOverridesPolicyAlg;
import com.sun.xacml.combine.OrderedPermitOverridesRuleAlg;
import com.sun.xacml.combine.PermitOverridesPolicyAlg;
import com.sun.xacml.combine.PermitOverridesRuleAlg;

import kodkod.ast.Expression;
import kodkod.ast.Formula;
import kodkod.ast.Relation;
import kodkod.ast.Variable;

/*
 * MGPolicyLeaf
 * A policy tree node without any children.
 */

public class MPolicyLeaf extends MPolicy
{	
	// Needs to be a list, rather than a set, in order to support "first applicable": ORDER MATTERS!
	protected LinkedList<MRule> rules;
	protected HashMap<String, MRule> rulemap;
	protected HashMap<String, Formula> disjunctionOfRules_cache;
	
	// Populated by doCombineRules
	protected HashMap<String, Set<String>> decisionUsesPredicates =
		new HashMap<String, Set<String>>();
	
	// New rule combination as of 3.1
	// A overridden by B, C
	// B overridden by C, D ...
	// first-applicable { A, B, ... }
	// [For NOW, only one first-applicable set. If needed later, s/b easy enough to add.]
	// Semantics: first-applicable is applied first, then overrides (for any overlap)
	Set <String> rCombineFA = new HashSet<String>();
	// A -> { decisions that override A }
	// Warning: this is **NOT** transitive!
	Map<String, Set<String>> rCombineWhatOverrides = new HashMap<String, Set<String>>();
					
	private int dupeRuleSuffix = 2;
	
	public MPolicyLeaf(String n, MVocab voc)
	{		
		super(n, voc);
		
		// Default Rcombination is none (all IDBs get produced independently)
		
		rules = new LinkedList<MRule>();		
		rulemap = new HashMap<String, MRule>();
		disjunctionOfRules_cache = new HashMap<String, Formula>();
	}
			
	void handleXACML2Combine(Node combAlgNode)
	{
		NodeList children = combAlgNode.getChildNodes();

		if(children == null)
			throw new MGEUnsupportedXACML("Combination algorithm not provided in expected way by XACML policy.");
		if(children.getLength() != 1)
			throw new MGEUnsupportedXACML("Combination algorithm not provided in expected way by XACML policy.");

		String result = children.item(0).getNodeValue();

		// From XACML 2.0 spec
		/*
		 * In the entire set of rules in the policy, if any rule evaluates to "Deny", then the result of the
	5136 rule combination SHALL be "Deny". If any rule evaluates to "Permit" and all other rules
	5137 evaluate to "NotApplicable", then the result of the rule combination SHALL be "Permit". In
	5138 other words, "Deny" takes precedence, regardless of the result of evaluating any of the
	5139 other rules in the combination. If all rules are found to be "NotApplicable" to the decision
	5140 request, then the rule combination SHALL evaluate to "NotApplicable".
		 */

		if("urn:oasis:names:tc:xacml:1.0:rule-combining-algorithm:deny-overrides".equals(result) || 
				"urn:oasis:names:tc:xacml:1.1:rule-combining-algorithm:ordered-deny-overrides".equals(result))
		{
			Set<String> denySet = new HashSet<String>();
			denySet.add("Deny");
			rCombineWhatOverrides.put("Permit", denySet);
		}			

		else if("urn:oasis:names:tc:xacml:1.0:rule-combining-algorithm:permit-overrides".equals(result) ||
				"urn:oasis:names:tc:xacml:1.1:rule-combining-algorithm:ordered-permit-overrides".equals(result))
		{
			Set<String> permSet = new HashSet<String>();
			permSet.add("Permit");
			rCombineWhatOverrides.put("Deny", permSet);
		}

		// From XACML 2.0 spec
		/*
		 * Each rule SHALL be evaluated in the order in which it is listed in the policy. For a
	5373 particular rule, if the target matches and the condition evaluates to "True", then the
	5374 evaluation of the policy SHALL halt and the corresponding effect of the rule SHALL be the
	5375 result of the evaluation of the policy (i.e. "Permit" or "Deny"). For a particular rule selected
	5376 in the evaluation, if the target evaluates to "False" or the condition evaluates to "False",
	5377 then the next rule in the order SHALL be evaluated. If no further rule in the order exists,
	5378 then the policy SHALL evaluate to "NotApplicable".
		 */

		else if("urn:oasis:names:tc:xacml:1.0:rule-combining-algorithm:first-applicable".equals(result))
		{
			rCombineFA.add("Permit");
			rCombineFA.add("Deny");	
		}

		/* [re: ordered-deny/permit]
		 * 
		 * The behavior of this algorithm is identical to that of the Permit-overrides rule-combining
	5363 algorithm with one exception. The order in which the collection of rules is evaluated SHALL
	5364 match the order as listed in the policy.
		 */

		// http://docs.oasis-open.org/xacml/2.0/access_control-xacml-2.0-core-spec-os.pdf
		else
			throw new MGEBadCombinator("XACML 2.x Leaf: Unsupported rule combining algorithm: "+result);	
	}




	

	void handleXACMLCombine(CombiningAlgorithm ca) throws MGEBadCombinator
	{
		if(ca instanceof DenyOverridesPolicyAlg || ca instanceof DenyOverridesRuleAlg ||
				ca instanceof OrderedDenyOverridesPolicyAlg || ca instanceof OrderedDenyOverridesRuleAlg)
		{
			Set<String> denySet = new HashSet<String>();
			denySet.add("Deny");
			rCombineWhatOverrides.put("Permit", denySet);
		}
		else if(ca instanceof PermitOverridesPolicyAlg || ca instanceof PermitOverridesRuleAlg   ||
				ca instanceof OrderedPermitOverridesPolicyAlg || ca instanceof OrderedPermitOverridesRuleAlg)
		{
			Set<String> permSet = new HashSet<String>();
			permSet.add("Permit");
			rCombineWhatOverrides.put("Deny", permSet);

		}
		else if(ca instanceof FirstApplicablePolicyAlg || ca instanceof FirstApplicableRuleAlg)
		{
			rCombineFA.add("Permit");
			rCombineFA.add("Deny");			
		}
		else
		{
			throw new MGEBadCombinator("XACML 1.x Leaf: Unsupported combining algorithm: "+ca);
		}
	}

	
	public void printPolicyInfo()
	{
		MEnvironment.errorWriter.println("###########################");
		MEnvironment.errorWriter.println("Policy Name: "+name);
		MEnvironment.errorWriter.println("This is a ground policy with rule combinator: "+printCombinators(rCombineFA, rCombineWhatOverrides));
		MEnvironment.errorWriter.println("Target Formula: "+target);		
		
		MEnvironment.errorWriter.println("Rules:");
		prettyPrintRules();

		MEnvironment.errorWriter.println("\nIDB and EDB predicates available for use in queries:");
		
		MEnvironment.errorWriter.println("IDBs:");
		prettyPrintIDBs();		
		prettyPrintEDBs();
		MEnvironment.errorWriter.print("\n\n");
				
		MEnvironment.errorWriter.println("###########################\n");
	}

	/**
	 * Prints the rules of the policy in a (somewhat) readable manner.
	 */
	public void prettyPrintRules()
	{
		MEnvironment.errorWriter.println("Rules (Name ::= Decision :- Target, Condition. [Used Predicates]):");
		
		for(MRule r : rules)
		{
			MEnvironment.errorWriter.println(r.name + " ::= " + r.decision() + " :- "+ r.target + ", "+r.condition+". "+r.involvesPredicates);
		}
		MEnvironment.errorWriter.println("");
	}

	public List<String> getRulesList(String decname, boolean qualified)
	{
		List<String> result = new ArrayList<String>();
		
		String qualstr = "";
		if(qualified)
			qualstr = name + MEnvironment.sIDBSeparator;
		
		// Use rules, not idbs, since rules contains the actual rule ordering
		for(MRule aRule : rules)
		{			
			// Do we care about its decision?
			if(decname == null || decname.length() == 0)
			{
				result.add(qualstr+ aRule.name);
			}
			else
			{
				if(aRule.decision().equalsIgnoreCase(decname))
					result.add(qualstr + aRule.name);
			}

		}
		
		return result;
		
	}
	
	
	protected boolean hasRule(String rulename)
	{
		try
		{
			getRule(rulename);
			return true;
		}
		catch(MGEUnknownIdentifier e)
		{
			return false;
		}
	}
	
	public List<String> ruleIDBsWithHigherPriorityThan(String rulename)
	{		
		// may pass canonical idb name ("PolicyName:Rule12" instead of "Rule12")
		if(rulename.startsWith(this.name+MEnvironment.sIDBSeparator)) 
			rulename = rulename.substring(this.name.length()+1);		
		
		List<String> result = new LinkedList<String>();
		
		// bad rule name, return empty
		if(!rulemap.containsKey(rulename))
			return result;
		
		MRule theRule = rulemap.get(rulename);
		
		Set<MRule> over1 = rulesThatApplyBefore(theRule);
		Set<MRule> over2 = rulesThatCanOverride(theRule);
		
		for(MRule r1 : over1)
			result.add(this.name+MEnvironment.sIDBSeparator+r1.name);
		for(MRule r2 : over2)
			result.add(this.name+MEnvironment.sIDBSeparator+r2.name);			
		
		return result;
	}
	
	protected MRule getRule(String rulename) throws MGEUnknownIdentifier
	{
		// If not a rule, throw an exception
		if(rulemap.containsKey(rulename))
			return rulemap.get(rulename);
		
		throw new MGEUnknownIdentifier(rulename+" is not a rule name in policy "+name);
	}
	
	public void initIDBs()	
	throws MUserException
	{				
		// Do NOT call this!
		//super.initIDBs();
		
		clearIDBs();
		disjunctionOfRules_cache.clear();
		
		// Combine rules
		doCombineRules();
		
		// Substitute out local IDBs that appeared on the RHS of rules.
		// e.g. permit(x) <- not deny(x); disallowing cycles of course.
		doSubstituteIDBs();
		
	}
		
	protected String noSuccessors(Map<String, Set<String>> tempUses)
	{
		for(String candidate : tempUses.keySet())
		{
			if(tempUses.get(candidate).size() < 1)
			{
				return candidate;
			}
		}
		
		return "";
	}
	
	protected void doSubstituteIDBs() 
	{
		MEnvironment.writeToLog("\nMPolicyLeaf.doSubstituteIDBs() ...");
		
		// Handle rules that refer to IDBs in a stratified manner.
		
		/////////////////////////////////////////////////////////////
		// (1) topologically sort decisions using decisionUsesPredicates 
		List<String> sortedDecisions = new ArrayList<String>(decisions.size());
		Map<String, Set<String>> tempUses = new HashMap<String, Set<String>>();
		
		// Populate temp map
		for(String dec : decisionUsesPredicates.keySet())
		{
			tempUses.put(dec, new HashSet<String>());
			
			for(String aPred : decisionUsesPredicates.get(dec))
			{
				if(decisions.contains(aPred))
					tempUses.get(dec).add(aPred);
			}
		}
		
		MEnvironment.writeToLog("  Removed non-decisions from mapping. Got: "+tempUses);
		
		while(tempUses.keySet().size() > 0)
		{
			String aSafeDecision = noSuccessors(tempUses);
			if(aSafeDecision.length() < 1)
			{
				MEnvironment.writeToLog("  Found cycle in: "+tempUses);
				throw new MUserException("Policy contained cyclic references among rules for decisions: "+tempUses.keySet());
			}
			
			// Remove from candidates
			tempUses.remove(aSafeDecision);
			
			// Remove from dependencies
			for(String dec : tempUses.keySet())
				tempUses.get(dec).remove(aSafeDecision);
				
			// Add to sorted list
			sortedDecisions.add(aSafeDecision);
		}
		MEnvironment.writeToLog("  Sort complete. Got: "+sortedDecisions);
		
		/////////////////////////////////////////////////////////////
		// (2) Traverse the sorts in order, substituting.
		
		for(String dec : sortedDecisions)
		{
			// What predicates for this decision use?
			for(String aPred : decisionUsesPredicates.get(dec))
			{
				// Concerned with decisions only, not EDBs
				if(!decisions.contains(aPred))
					continue;
				
				MCommunicator.writeToLog("\n  Substituting to resolve cyclic IDB: "+aPred);
				
				// Replace (with substitution). Potentially different substitution for each occurrence, so use a visitor.
				Formula fullFormula = getIDB(dec);
				Formula subIdbFormula = getIDB(aPred); 
				Relation targetRel = MFormulaManager.makeRelation(aPred, varOrderings.get(aPred).size());
				List<Variable> targetVars = varOrderings.get(aPred);
				MIDBReplacementV vis = new MIDBReplacementV(targetRel, targetVars, subIdbFormula, vocab.exprToTerm);
				putIDB(dec, fullFormula.accept(vis), varOrderings.get(dec));																			
			}
		}
		
	}

	// For test cases 
	protected void addRule(String rulename, String decision, List<String> ruleVarNameOrdering, 
			Formula aTarget, Formula aCondition)
	{
		addRule(rulename, decision, ruleVarNameOrdering, aTarget, aCondition, null);
	}
	
	public void addRule(String rulename, String decision, List<String> ruleVarNameOrdering, 
			Formula aTarget, Formula aCondition, MExploreCondition helper)
	  throws MGEUnknownIdentifier, MGEArityMismatch, MGEBadIdentifierName
	{	
		/////////////////////////////////////////////////////////////
		// Allow a policy to try to re-define the same rule, just know that they are separate.
		if(hasRule(rulename))
		{			
			rulename = rulename + "-" + dupeRuleSuffix;
			dupeRuleSuffix++;
		}
		
		/////////////////////////////////////////////////////////////
		// Variable ordering on this rule from the rule head; 
		// e.g. (permit s a r) :- ...
		List<Variable> ruleVarOrdering = new ArrayList<Variable>();
		for(String vname : ruleVarNameOrdering)
		{
			ruleVarOrdering.add(MFormulaManager.makeVariable(vname));
		}
		
		/////////////////////////////////////////////////////////////
		if(!idbKeys().contains(decision)) // add the DECISION'S IDB
		{
			// First time we saw this IDB. Need to add it (and the free var ordering) to the policy.
			// (Sorts of all these variables should be known already.)			
			putIDB(decision, Formula.FALSE, ruleVarOrdering);			
			
			// Add the decision to the list as well
			decisions.add(decision);
		}
		
		/////////////////////////////////////////////////////////////
		// Make sure The decision is not expecting a different arity.
		// This is what we used after receiving prior rules with the same decision.
		List<Variable> expectedIDBFreeVars = varOrderings.get(decision);
		
		// order-independent
		Set<Variable> expectedSet = new HashSet<Variable>(expectedIDBFreeVars);
		Set<Variable> thisSet = new HashSet<Variable>(ruleVarOrdering);
		
		if(!expectedSet.equals(thisSet))
		{
			String hashStr = "";
			
			int ii = 0;
			for(Variable v1 : expectedIDBFreeVars)
			{
				Variable v2 = ruleVarOrdering.get(ii);
				if(!v1.equals(v2))
				{
					hashStr += "Mismatch between var "+v1.toString()+" (hash="+v1.hashCode()+") and var "+
					v2.toString()+" (hash="+v2.hashCode()+")\n";
					hashStr += "MFormulaManager had hash="+MFormulaManager.makeVariable(v1.toString()).hashCode();
				}
				ii++;
			}
			hashStr += "\n";
			hashStr += "Formula Manager's variable cache was: "+MFormulaManager.varCacheToString()+"\n";
			
			
			throw new MGEArityMismatch("The decision "+decision+" was used with two different variable orderings. "+
					"First was: "+expectedIDBFreeVars+"; second was: "+ruleVarOrdering+". Hashes were: \n"+hashStr);

		}			
		
		// TODO check that those variables are indeed free in the rule.		
		
		// TODO re-order and substitute if order is different
		
		/////////////////////////////////////////////////////////////
		// Add this rule to the rule set.
		MRule newrule = new MRule(this);
		newrule.setDecision(decision);
		newrule.name = rulename;	
		newrule.ruleVarOrdering = ruleVarOrdering;
		newrule.target = aTarget;
		newrule.condition = aCondition;
				
		// Remember which relation names this rule depends on.
		if(helper != null)
		{
			for(Relation r : helper.madeEDBs)
				newrule.involvesPredicates.add(r.name());
		}
		
		newrule.target_and_condition = MFormulaManager.makeAnd(newrule.target, newrule.condition);		
		rules.add(newrule);
		rulemap.put(newrule.name, newrule);
		
		//FOR DEBUGGING
		//MCommunicator.writeToLog("\n In MPolicyLeaf.addRule\n newRule.target: " + newrule.target + "\nnewRule.condition: " + newrule.condition);				
	}	
	
	public String getDecisionForRuleIDBName(String idbname)
	{
		idbname = idbname.toLowerCase();
		
		// may pass canonical idb name ("PolicyName:Rule12" instead of "Rule12")
		if(idbname.startsWith(this.name+MEnvironment.sIDBSeparator)) // name is already lowercase
		{
			idbname = idbname.substring(this.name.length()+1);
		}		
				
		// TODO rules should be a hash map
		for(MRule rule : rules)
		{
			if(rule.name.equals(idbname))
				return rule.decision();
		}
		
		return "";
	}
	
	
	Set<MRule> rulesThatApplyBefore(MRule aRule)
	{
		// If aRule has decision D, and decision P is in the first-applicable set with D,
		// then all rules appearing BEFORE aRule with decision P are in the result.
		
		Set<MRule> results = new HashSet<MRule>();
		
		String aDecision = aRule.decision();
		
		// If this decision is not one of the FA decisions, 
		// no rules apply before this one by virtue of the ordering
		if(!rCombineFA.contains(aDecision))
			return results;
		
		// TODO room for optimization here, if its a chokepoint
		for(MRule r : rules)
		{
			// Stop once we reach the appropriate rule.
			if(r.equals(aRule))
				break;
			
			if(rCombineFA.contains(r.decision()))
				results.add(r);
		}
		
		return results;
	}
	
	Set<MRule> rulesThatCanOverride(MRule aRule)
	{
		// If aRule has decision D, and decision P overrides D, 
		// then all rules for P are in the result.
		
		Set<MRule> results = new HashSet<MRule>();
		
		String aDecision = aRule.decision();
		
		// Protect against null ptr
		if(!rCombineWhatOverrides.containsKey(aDecision))
			return results;
		
		Set<String> overBy = rCombineWhatOverrides.get(aDecision);
		
		// TODO room for optimization here, if its a chokepoint
		for(MRule r : rules)
		{
			if(overBy.contains(r.decision()))
				results.add(r);
		}
				
		return results;
	}
	
	Map<String, Formula> buildConciseIDBFAs()
	{
		Map<String, Formula> result = new HashMap<String, Formula>();

		// Make sure all the formulas are appropriately initialized, since we or onto them below.
		for(String d : decisions)
		{
			result.put(d, Formula.FALSE);
			decisionUsesPredicates.put(d, new HashSet<String>());
		}
				
		List<MRule> backwards = new ArrayList<MRule>(rules);
		Collections.reverse(backwards);
			
		for(MRule r : backwards)
		{
			//System.err.println("Processing rule: "+r.name);
			
			// Process this rule:
			// For this rule's decision, add (current_idb OR rule)
			// For other decisions, add (current_idb AND not(rule)).

			for(String dec : decisions)
			{
				if(dec.equals(r.decision()))
				{
					Formula theFmla;

					// Always use conditions for rule actually firing
					if(!isXACML)
						theFmla = MFormulaManager.makeOr(result.get(dec), r.target_and_condition);
					// If FA doesn't apply for this decision, even in an XACML policy [impossible situation?]
					else if(!rCombineFA.contains(dec))
						theFmla = MFormulaManager.makeOr(result.get(dec), r.target_and_condition);
					else
						// But XACML is complicated. Need to say "This rule applies fully, or its target was missed and a later rule applied." 
						// This is needed: consider situation where r's condition fails but its target matches. Since this is XACML, none of the
						// later rules (which we processed first) can apply.
						theFmla = MFormulaManager.makeOr(MFormulaManager.makeAnd(result.get(dec), MFormulaManager.makeNegation(r.target)), r.target_and_condition);  
							
					//System.err.println("Put: "+dec+" -> "+theFmla);
					
					decisionUsesPredicates.get(dec).addAll(r.involvesPredicates);
					result.put(dec, theFmla);
				} // end same decision				
				
				else
				{
					Formula theFmla;
					
					// Only add the restriction if this decision is FA along with r's
					if(!rCombineFA.contains(dec) || !rCombineFA.contains(r.decision()))
						continue;
					
					// Don't add conjunction if nothing so far for this decision (NOT "rule's" decision)
					if(result.get(dec).equals(Formula.FALSE))
						continue;					

					// From the XACML 2.0 docs:
					/* 
						420 In the case of the "First-applicable" combining algorithm, the combined result is the same as the
						421 result of evaluating the first <Rule>, <Policy> or <PolicySet> element in the list of rules
						422 whose target is applicable to the decision request.  */

					if(!isXACML)
						theFmla = MFormulaManager.makeAnd(result.get(dec), MFormulaManager.makeNegation(r.target_and_condition));
					else
						theFmla = MFormulaManager.makeAnd(result.get(dec), MFormulaManager.makeNegation(r.target)); 		
					
					//System.err.println("Put: "+dec+" -> "+theFmla);
					
					decisionUsesPredicates.get(dec).addAll(r.involvesPredicates);
					result.put(dec, theFmla);
				} // end other decision
			}  // end for each decision
		} // end for each rule in reverse
		
		return result;	
	} // end buildConciseIDBFA
	
	void doCombineRules() throws MGEUnknownIdentifier, MGEBadCombinator, MGEBadIdentifierName
	{
		// This method applies the given RULE combination algorithm to the rules and populates the decision idbs.
		// If called multiple times, the idbs will be re-populated with new formulas for the most recently given combination options.
		
		// Want to (re)create IDBs for:
		// (a) Each rule's matches
		// (b) Each rule's applies
		// (c) Each decision IDB

		/////////////////////////////////////////////////////////////////
		// (a) These are easy:
		for(MRule r : rules)
		{
			String idbName = r.name+"_matches";
			putIDB(idbName, r.target_and_condition, r.ruleVarOrdering);			
			decisionUsesPredicates.put(idbName, r.involvesPredicates);
		}
		
		/////////////////////////////////////////////////////////////////
		// (b) A rule applies if it matches and no potentially overriding rule matches, either.
		// OPT likely a lot of repeated work here, but not optimizing yet
		for(MRule r : rules)
		{
			Set<String> usesPreds = new HashSet<String>(r.involvesPredicates);
			Set<Formula> rFmlas = new HashSet<Formula>();
			rFmlas.add(r.target_and_condition);
			for(MRule overR1 : rulesThatApplyBefore(r))
			{
				if(isXACML)
					rFmlas.add(MFormulaManager.makeNegation(overR1.target));
				else
					rFmlas.add(MFormulaManager.makeNegation(overR1.target_and_condition));
				
				usesPreds.addAll(overR1.involvesPredicates);
			}
			for(MRule overR2 : rulesThatCanOverride(r))
			{
				usesPreds.addAll(overR2.involvesPredicates);
				rFmlas.add(MFormulaManager.makeNegation(overR2.target_and_condition));
			}
			String idbName = r.name+"_applies";
			decisionUsesPredicates.put(idbName, usesPreds);
			putIDB(idbName, MFormulaManager.makeConjunction(rFmlas), r.ruleVarOrdering);
		}

		
		/////////////////////////////////////////////////////////////////
		// (c) The decision IDBs
		// IDB_FA: Either the IDB (if not a FA decision) or the IDB taking FA into account
		// After IDB_FA is computed, tack on negation of all overrides rules.
		// FA is made more efficient by a single traversal:
		Map<String, Formula> IDB_FAs = buildConciseIDBFAs();
		
		MEnvironment.writeToLog("\nIn doCombineRules...  "+decisions);
		for(String decName : decisions)
		{			
			MEnvironment.writeToLog("\nIn doCombineRules for decision "+decName);
			
			Formula IDB_FA = IDB_FAs.get(decName);
			// decFmla = IDB_FA and none of the overrides decision rules apply
									
			Set<Formula> negPriors = new HashSet<Formula>();
			
			// If anything overrides decName at all (avoid null ptr)
			if(rCombineWhatOverrides.containsKey(decName))
			{
				Set<String> overrideDecs = rCombineWhatOverrides.get(decName);
				
				for(MRule r : rules)
				{
					//  OPT likely a lot of repeated work here, but not optimizing yet
					if(overrideDecs.contains(r.decision()))
					{
						negPriors.add(MFormulaManager.makeNegation(r.target_and_condition));
						decisionUsesPredicates.get(decName).addAll(r.involvesPredicates);
					}
					
				}
			}
			
			negPriors.add(IDB_FA); // don't forget that the original rule applies!			
			Formula decFmla = MFormulaManager.makeConjunction(negPriors);
			
			MEnvironment.writeToLog("\n  IDB_FA: "+IDB_FA);
			MEnvironment.writeToLog("\n  decFmla: "+decFmla);
			MEnvironment.writeToLog("\n  Uses Predicates:"+decisionUsesPredicates.get(decName));
			putIDB(decName, decFmla, varOrderings.get(decName));
		}
					
		
		/////////////////////////////////////////////////////////
		// Finally, each of these IDBs only apply if the _policy's_ target is met. 		
		
		MEnvironment.writeToLog("\nUpdating with target="+target);
		
		for(String idbname : idbKeys())
		{			
			putIDB(idbname, MFormulaManager.makeAnd(getIDB(idbname), target), varOrderings.get(idbname));		
		}
		
	} // end doCombineRules

	public static void unitTests()
	{
		MEnvironment.writeErrLine("----- Begin MPolicyLeaf Tests (No messages is good.) -----");
		
		MVocab voc = new MVocab();
		voc.addSort("Subject");
		voc.addSort("Action");
		voc.addSort("Resource");		
		voc.addPredicate("ownerOf", "Subject Resource");
		voc.addPredicate("restricted", "Resource");
		voc.addSubSort("Subject", "Admin");
		voc.addSubSort("Subject", "Employee");
		voc.axioms.addConstraintAbstract("Subject");
		
		
		
		MPolicyLeaf pol = new MPolicyLeaf("Test Policy 1", voc);
		pol.declareVariable("s", "Subject");
		pol.declareVariable("a", "Action");
		pol.declareVariable("r", "Resource");
		
		/////////////////////////////////////////////////////////////
		// Decisions are added implicitly by adding rules.
		// Make sure adding a rule has the proper effects.
		
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
		pol.addRule("Rule1", "permit", vSAR, aTarget, Formula.TRUE);
		aTarget = MFormulaManager.makeAtom(MFormulaManager.makeExprTuple(vSR), ownerOf);
		pol.addRule("Rule2", "permit", vSAR, aTarget, Formula.TRUE);		
		aTarget = MFormulaManager.makeAtom(r, restricted);
		pol.addRule("Rule3", "deny", vSAR, aTarget, Formula.TRUE);
		
		// Now, test combinators.
		
		/////////////////////////////////////////////////////////////
		//pol.prettyPrintRules();
		/////////////////////////////////////////////////////////////
		
		/////////////////////////////////////////////////////////////
		pol.rCombineFA.add("permit");
		pol.rCombineFA.add("deny");
		
		pol.initIDBs();		
		
		
		
		
		/////////////////////////////////////////////////////////////
		// (1) Rule 1 matches; this is FA so Rule 3 can never apply, so deny won't happen.
		Formula testFmla1 = pol.getIDB("deny");
		testFmla1 = MFormulaManager.makeAnd(testFmla1, MFormulaManager.makeAtom(s, admin));		
		
		if(MJavaTests.testIsSatisfiable(testFmla1, voc, 3, varSorts))
			MEnvironment.errorWriter.println("********** MPolicyLeaf test 1: FAILED!");
		/////////////////////////////////////////////////////////////
		// (2) Restricted, we should see deny apply.
		
		Formula testFmla2 = pol.getIDB("deny");
		testFmla2 = MFormulaManager.makeAnd(testFmla2, MFormulaManager.makeAtom(r, restricted));
		
		if(!MJavaTests.testIsSatisfiable(testFmla2, voc, 3, varSorts))
			MEnvironment.errorWriter.println("********** MPolicyLeaf test 2: FAILED!");
		//pol.prettyPrintIDBs();
		/////////////////////////////////////////////////////////////
		
		/////////////////////////////////////////////////////////////
		pol.rCombineFA.clear();
		Set<String> sDeny = new HashSet<String>();
		sDeny.add("deny");
		pol.rCombineWhatOverrides.put("permit", sDeny);
		
		pol.initIDBs();		
		
		
		/////////////////////////////////////////////////////////////
		// (3) This is now Deny overrides. Make sure that happens.
		Formula testFmla3 = pol.getIDB("permit");
		testFmla3 = MFormulaManager.makeAnd(testFmla3, MFormulaManager.makeAtom(r, restricted));		
		
		if(MJavaTests.testIsSatisfiable(testFmla3, voc, 3, varSorts))
			MEnvironment.errorWriter.println("********** MPolicyLeaf test 3: FAILED!");
		
		//pol.prettyPrintIDBs();
		MEnvironment.writeOutLine(pol.vocab.asSExpression(pol.name));
		MEnvironment.writeOutLine(pol.asSExpression());
		/////////////////////////////////////////////////////////////
		
		
		
		
		MEnvironment.writeErrLine("----- End MPolicyLeaf Tests -----");	
	}
	
	public String asSExpression()
	{
		StringBuffer buf = new StringBuffer();
		buf.append("(Policy uses "+name+MEnvironment.eol);
		
		buf.append("  (Variables "+MEnvironment.eol);
		for(Variable v : varSorts.keySet())
		{
			buf.append("("+v.name()+" "+varSorts.get(v)+")"+MEnvironment.eol);
		}		
		buf.append(")"+MEnvironment.eol); // end vars
	
		buf.append("  (Rules "+MEnvironment.eol);
		for(MRule r : rules)
		{
			r.toSExpression(buf);			
		}		
		buf.append(")"+MEnvironment.eol); // end rules
		
		buf.append("(RComb ");
		//rCombineWhatOverrides
		if(rCombineFA.size() > 0)
		{
			buf.append("(fa ");
			for(String s : rCombineFA)
			{
				buf.append(s+" ");
			}
			buf.append(")");
		}
		if(rCombineWhatOverrides.size() > 0)
		{
			for(String k : rCombineWhatOverrides.keySet())
			{
				// // A -> { decisions that override A }
				// Warning: this is **NOT** transitive!
				buf.append("(over "+k+" ");
				for(String s : rCombineWhatOverrides.get(k))
				{
					buf.append(s+" ");
				}
				buf.append(")");

			}
		}
		buf.append(")"); // end rcomb
		
		buf.append(")"+MEnvironment.eol);
		return buf.toString();
	}

	public void declareVariable(String varname, String typename) {
		varSorts.put(MFormulaManager.makeVariable(varname),
				     vocab.getSort(typename).rel);		
	}

}
