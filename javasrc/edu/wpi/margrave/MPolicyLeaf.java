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

import kodkod.ast.Decls;
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
			throw new MGEBadCombinator("Unsupported rule combining algorithm: "+result);	
	}




	

	void handleXACMLCombine(CombiningAlgorithm ca) throws MGEBadCombinator
	{
		if(ca instanceof DenyOverridesPolicyAlg || ca instanceof DenyOverridesRuleAlg)
		{
			Set<String> denySet = new HashSet<String>();
			denySet.add("Deny");
			rCombineWhatOverrides.put("Permit", denySet);
		}
		else if(ca instanceof PermitOverridesPolicyAlg || ca instanceof PermitOverridesRuleAlg)
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
				
		throw new MGEBadCombinator("Unsupported combining algorithm: "+ca);		
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
		MEnvironment.errorWriter.println("Rules (Name ::= Decision :- Target, Condition):");
		Iterator<MRule> blergh = rules.iterator();
		while(blergh.hasNext())			
		{
			MRule blerghr = blergh.next();
			MEnvironment.errorWriter.println(blerghr.name + " ::= " + blerghr.decision() + " :- "+ blerghr.target + ", "+blerghr.condition);
		}
		MEnvironment.errorWriter.println("");
	}

	public List<String> getRulesList(String decname, boolean qualified)
	{
		List<String> result = new ArrayList<String>();
		
		String qualstr = "";
		if(qualified)
			qualstr = name + ":";
		
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
		if(rulename.startsWith(this.name+":")) 
			rulename = rulename.substring(this.name.length()+1);		
		
		List<String> result = new LinkedList<String>();
		
		// bad rule name, return empty
		if(!rulemap.containsKey(rulename))
			return result;
		
		MRule theRule = rulemap.get(rulename);
		
		Set<MRule> over1 = rulesThatApplyBefore(theRule);
		Set<MRule> over2 = rulesThatCanOverride(theRule);
		
		for(MRule r1 : over1)
			result.add(this.name+":"+r1.name);
		for(MRule r2 : over2)
			result.add(this.name+":"+r2.name);			
		
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
		
		idbs.clear();
		disjunctionOfRules_cache.clear();
		
		// Combine rules
		doCombineRules();
	}
			
	/*
	 * 
	 */
//	public void addRule(String rulename, String decision, List<String> freeVarOrdering, List<String> conjuncts) 
	public void addRule(String rulename, String decision, List<String> freeVarOrdering, Formula aTarget, Formula aCondition)
	  throws MGEUnknownIdentifier, MGEArityMismatch, MGEBadIdentifierName
	{	
		/////////////////////////////////////////////////////////////
		// Allow a policy to try to re-define the same rule, just know that they are separate.
		if(hasRule(rulename))
		{			
			rulename = rulename + "-" + dupeRuleSuffix;
			dupeRuleSuffix++;
		}
		
		// XXX to remove. renaming is done by caller now.
		// We rename bound variables internally. e.g. if policy declares variable x : A,
		// and two separate rules use x as a rule-scope existential, we create x_rule1 and x_rule2
		// to standardize apart.
		HashMap<String, Variable> otherVarLocals = new HashMap<String, Variable>();

		/////////////////////////////////////////////////////////////
		// Variable ordering on this rule 
		List<Variable> ruleFreeVars = new ArrayList<Variable>();
		for(String vname : freeVarOrdering)
		{
			ruleFreeVars.add(MFormulaManager.makeVariable(vname));
		}
		
		/////////////////////////////////////////////////////////////
		if(!idbs.keySet().contains(decision))
		{
			// First time we saw this IDB. Need to add it (and the free var ordering) to the policy.
			// (Sorts of all these variables should be known already.)			
			idbs.put(decision, Formula.FALSE);
			this.varOrderings.put(decision, ruleFreeVars);	
			
			// Add the decision to the list as well
			decisions.add(decision);
		}
		
		/////////////////////////////////////////////////////////////
		// Make sure The decision is not expecting a different arity.
		List<Variable> expectedIDBFreeVars = varOrderings.get(decision);
		if(!ruleFreeVars.equals(expectedIDBFreeVars))
		{
			throw new MGEArityMismatch("The decision "+decision+" was used with two different variable orderings. "+
					"First was: "+expectedIDBFreeVars+"; second was: "+ruleFreeVars);
		}
		
		
		// For each literal fmla in the list
		// XXX to remove. string-handling for what used to be conjunction of literals
	/*	for(String conj : conjuncts)
		{
			conj = conj.toLowerCase();
			
			if(conj.compareTo("true")==0)
				continue; // no restrictions		
				
			Formula atom;
			boolean pred_used_request_vars_only = true;
			
			String[] breakdown = conj.split(" "); // split by spaces
							
			boolean negation = false;
			
			// Just in case the user is cruel and double-negates...
			while(breakdown[0].startsWith("!"))
			{
				negation = !negation;
				breakdown[0] = breakdown[0].substring(1);			
			}
						
			// getRelation will throw an exception if the name is unknown.
			// It will also apply reflexive, transitive closure if the final char is *.
			// special treatment for equality
			
			try
			{
				if("=".equals(breakdown[0]))
				{
					// EQ
					
					// May see a rule-scope existential for the first time here.
					
					Variable v1; 
					Variable v2;
					
					if(!ruleFreeVars.contains(breakdown[1]))
					{
						otherVarLocals.put(breakdown[1], 
								          MFormulaManager.makeVariable(rulename+"_"+breakdown[1]));
						v1 = otherVarLocals.get(breakdown[1]);
						pred_used_request_vars_only = false;							
					}
					else
						v1 = MFormulaManager.makeVariable(breakdown[1]);
					
					if(!ruleFreeVars.contains(breakdown[2]))
					{
						otherVarLocals.put(breakdown[2], 
								          MFormulaManager.makeVariable(rulename+"_"+breakdown[2]));
						v2 = otherVarLocals.get(breakdown[2]);
						pred_used_request_vars_only = false;							
					}
					else
						v2 = MFormulaManager.makeVariable(breakdown[2]);
					
					
					// Any exceptions for bad variable name will propagate up.					
					
					atom = MFormulaManager.makeEqAtom(v1, v2);
				}	
				else
				{
					// IN
				
					Expression relexp = getRelationExpr(breakdown[0]);					
					
					// breakdown elements 1-k are variables. (There are no function calls allowed syntactically:
					// use "(Knight f) (Father s f)" rather than "(Knight (Father s))"
					// loop to build up varexp Expression.

					List<String> tupleList = new ArrayList<String>(breakdown.length-1);
					for(int ii = 1; ii< breakdown.length;ii++)
					{
						// Handle rule-scope local variables and request variables differently.
							
						// to be removed: TN april 2011
						// Caller must have explicitly added all variables (and say what type they are) already
						//if(!vocab.requestVariables.containsKey(breakdown[ii]) &&
						//		!vocab.otherVarDomains.containsKey(breakdown[ii]))
						//	throw new MGEUnknownIdentifier("Error: Unknown variable name: "+breakdown[ii]);
							
						if(!ruleFreeVars.contains(breakdown[ii]))
						{
							otherVarLocals.put(breakdown[ii], 
									          MFormulaManager.makeVariable(rulename+"_"+breakdown[ii]));
							tupleList.add(rulename+"_"+breakdown[ii]);
							pred_used_request_vars_only = false;							
						}
						else
							tupleList.add(breakdown[ii]);
					}
														
						
					Expression varexp = MFormulaManager.makeVarTuple(tupleList);
					atom = MFormulaManager.makeAtom(varexp, relexp);
					
					// Sanity check: arity
					if(varexp.arity() != relexp.arity())
						throw new MGEArityMismatch("Error: Arity mismatch between: "+varexp+" and "+relexp);

				}
				
				// Done preparing the atom
				Formula literal = atom;
				if(negation)
					literal = MFormulaManager.makeNegation(atom); 
				
				
				// Is this a target or a condition?
				// Just in case we need to distinguish between them in the future; they are XACML constructs 
				// TARGET: domains/subdomains only, also can only ask about REQUEST variables!
				// CONDITION: all else
				if(vocab.isSort(breakdown[0]) && pred_used_request_vars_only)
					thisruletarget.add(literal);
				else
					thisrulecondition.add(literal);

			
			}
			catch(MGEManagerException e)
			{
				throw new MGEUnknownIdentifier(e.toString());
			}
		
			
		} // end for each conjunct
		*/
		
		/////////////////////////////////////////////////////////////
		// Add this rule to the rule set.
		MRule newrule = new MRule();
		newrule.setDecision(decision);
		newrule.name = rulename;	
		newrule.target = aTarget;
		newrule.condition = aCondition;
				
		// XXX to remove: quantification s/b done already in the construction of aTarget, aCondition
		
		// Any rule-scope variables here need explicit existential quantification.
		// (added to the CONDITION -- since the target cannot involve non-request vars.)
		/*
		for(Variable v : otherVarLocals.values())
		{
			// What was it originally?
			for(String vname : otherVarLocals.keySet())
				if(v.equals(otherVarLocals.get(vname)))
				{
					try
					{
						Decls d = MFormulaManager.makeOneOfDecl(v, varSorts.get(vname));					
						newrule.condition = MFormulaManager.makeExists(newrule.condition, d);
					}
					catch(MGEManagerException e)
					{
						throw new MGEUnknownIdentifier("Error adding rule-scope quantification for rule "+rulename+". "+e);
					}
					break;
				}
		}
		*/
		
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
		if(idbname.startsWith(this.name+":")) // name is already lowercase
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
			result.put(d, Formula.FALSE);
				
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
			idbs.put(r.name+"_matches", r.target_and_condition);
		}
		
		/////////////////////////////////////////////////////////////////
		// (b) A rule applies if it matches and no potentially overriding rule matches, either.
		// OPT likely a lot of repeated work here, but not optimizing yet
		for(MRule r : rules)
		{
			Set<Formula> rFmlas = new HashSet<Formula>();
			rFmlas.add(r.target_and_condition);
			for(MRule overR1 : rulesThatApplyBefore(r))
			{
				if(isXACML)
					rFmlas.add(MFormulaManager.makeNegation(overR1.target));
				else
					rFmlas.add(MFormulaManager.makeNegation(overR1.target_and_condition));
			}
			for(MRule overR2 : rulesThatCanOverride(r))
				rFmlas.add(MFormulaManager.makeNegation(overR2.target_and_condition));
			idbs.put(r.name+"_applies", MFormulaManager.makeConjunction(rFmlas));
		}

		
		/////////////////////////////////////////////////////////////////
		// (c) The decision IDBs
		// IDB_FA: Either the IDB (if not a FA decision) or the IDB taking FA into account
		// After IDB_FA is computed, tack on negation of all overrides rules.
		// FA is made more efficient by a single traversal:
		Map<String, Formula> IDB_FAs = buildConciseIDBFAs();
		
		for(String decName : decisions)
		{			
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
					}
					
				}
			}
			
			negPriors.add(IDB_FA); // don't forget that the original rule applies!			
			Formula decFmla = MFormulaManager.makeConjunction(negPriors);
			idbs.put(decName, decFmla);
		}
					
		
		/////////////////////////////////////////////////////////
		// Finally, each of these IDBs only apply if the _policy's_ target is met. 		
		
		for(String idbname : idbs.keySet())
		{			
			idbs.put(idbname, MFormulaManager.makeAnd(idbs.get(idbname), target));		
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
		Relation admin = voc.getSort("Admin").rel;		
		Relation ownerOf = voc.predicates.get("ownerOf").rel;
		Relation restricted = voc.predicates.get("restricted").rel;
		Formula aTarget;
		
		aTarget = MFormulaManager.makeAtom(s, admin);		
		pol.addRule("Rule1", "permit", vSAR, aTarget, Formula.TRUE);
		aTarget = MFormulaManager.makeAtom(MFormulaManager.makeVarTuple(vSR), ownerOf);
		pol.addRule("Rule2", "permit", vSAR, aTarget, Formula.TRUE);		
		aTarget = MFormulaManager.makeAtom(r, restricted);
		pol.addRule("Rule3", "deny", vSAR, aTarget, Formula.TRUE);
		
		// Now, test combinators.
		
		/////////////////////////////////////////////////////////////
		pol.prettyPrintRules();
		/////////////////////////////////////////////////////////////
		
		/////////////////////////////////////////////////////////////
		pol.rCombineFA.add("permit");
		pol.rCombineFA.add("deny");
		
		pol.initIDBs();		
		pol.prettyPrintIDBs();
		/////////////////////////////////////////////////////////////
		
		/////////////////////////////////////////////////////////////
		pol.rCombineFA.clear();
		Set<String> sDeny = new HashSet<String>();
		sDeny.add("deny");
		pol.rCombineWhatOverrides.put("permit", sDeny);
		
		pol.initIDBs();		
		pol.prettyPrintIDBs();
		/////////////////////////////////////////////////////////////
		
	
		// TODO: make these actual test cases rather than strings to inspect.
		
		MEnvironment.writeErrLine("----- End MPolicyLeaf Tests -----");	
	}
	
}
