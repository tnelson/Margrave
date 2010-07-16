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

package a;

import java.lang.management.ManagementFactory;
import java.lang.management.ThreadMXBean;
import java.util.*;

import kodkod.ast.Decls;
import kodkod.ast.Expression;
import kodkod.ast.Formula;
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
	
	public String rCombine;
		
	private int dupeRuleSuffix = 2;
	
	public MPolicyLeaf(String n, MVocab voc)
	{		
		super(n, voc);
		
		rules = new LinkedList<MRule>();
		rCombine = "FAC"; // default to intuitive (condition-included) first-applicable
		rulemap = new HashMap<String, MRule>();
		disjunctionOfRules_cache = new HashMap<String, Formula>();
	}
			
	/**
	 * 
	 * Tests whether a given policy can be compared realistically with this one.
	 * In other words, whether they have the same vocabulary object. 
	 * 
	 * Note that this sameness is at the object level, so two policies loading the same vocabulary
	 * need to use the same MGVocab object.
	 * 
	 * @param p A policy to attempt comparison with.
	 * @return true if comparison with p is valid, otherwise false.
	 * @see MPolicy
	 * @see MVocab
	 * 
	 */
	public boolean canDiffWith(MPolicyLeaf p)
	{
		// Does p have the same vocabulary?
		if(p.vocab.equals(vocab))
			return true;		
		return false; 
	}	

	public void printPolicyInfo()
	{
		System.out.println("###########################");
		System.out.println("Policy Name: "+name);
		System.out.println("This is a ground policy with rule combinator: "+rCombine);
		System.out.println("Target Formula: "+target);		
		
		System.out.println("Rules:");
		prettyPrintRules();

		System.out.println("\nIDB and EDB predicates available for use in queries:");
		
		System.out.println("IDBs:");
		prettyPrintIDBs();		
		prettyPrintEDBs();
		System.out.print("\n\n");
		
		System.out.println("Policy-level assumptions: ");
		assumptions.printConstraints();
		
		System.out.println("###########################\n");
	}

	/**
	 * Prints the rules of the policy in a (somewhat) readable manner.
	 */
	public void prettyPrintRules()
	{
		System.out.println("Rules (Name ::= Decision :- Target, Condition):");
		Iterator<MRule> blergh = rules.iterator();
		while(blergh.hasNext())			
		{
			MRule blerghr = blergh.next();
			System.out.println(blerghr.name + " ::= " + blerghr.decision() + " :- "+ blerghr.target + ", "+blerghr.condition);
		}
		System.out.println("");
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
		// stored lowercase
		rulename = rulename.toLowerCase();
		
		// may pass canonical idb name ("PolicyName:Rule12" instead of "Rule12")
		if(rulename.startsWith(this.name+":")) // name is already lowercase
		{
			rulename = rulename.substring(this.name.length()+1);
		}		
		
		List<String> result = new LinkedList<String>();
		if(!rulemap.containsKey(rulename))
			return result;		
		MRule theRule = rulemap.get(rulename);
		
		if(rCombine.toUpperCase().startsWith("FA"))
		{
			// Return the the sub-list before this rule.
			List<MRule> ruleList = rules.subList(0, rules.indexOf(theRule));
			for(MRule rule : ruleList)
				result.add(this.name+":"+rule.name);						
		}
		else if(rCombine.toUpperCase().startsWith("O "))
		{
			// What rules have decisions that override theRule's?
			String[] ordering = rCombine.substring(2).split(" ");
			
			Set<String> higherPriority = new HashSet<String>();
			for(String dec : ordering)
			{
				if(dec.equals(theRule.decision()))
					break;
				higherPriority.add(dec);
			}
			
			for(MRule rule : rules)
			{
				if(higherPriority.contains(rule.decision()))
					result.add(this.name+":"+rule.name);
			}
			
		}
		
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
	throws MGEBadCombinator, MGEUnknownIdentifier, MGEArityMismatch, MGEBadQueryString, MGEManagerException, MGEBadIdentifierName
	{				

		idbs.clear();
		disjunctionOfRules_cache.clear();
		
		// Combine rules
		doCombineRules();
		
		try
		{
			handlePolicyAssumptions();
		}
		catch(MGEBadIdentifierName e)
		{
			System.err.println("Bad identifier name in initIDBs().");
			System.exit(1);
		}
		
		/*
		// Simplify
		SimplifyFormulaV simplifier = new SimplifyFormulaV();
		for(String idbname : idbs.keySet())
			idbs.put(idbname, idbs.get(idbname).accept(simplifier));
			*/	
	}
			
	public void addRule(String rulename, String decision, List<String> conjuncts) 
	  throws MGEUnknownIdentifier, MGEArityMismatch, MGEBadIdentifierName
	{
		// decision(requestVars) :- clauses[0] and clauses[1] and ...
		// where each clause is a String of the form: <pred-name> <arg 1> ... <arg k>
		// and each arg is either a variable name or a function call.		
		
		if(hasRule(rulename))
		{
		//	throw new MGEBadIdentifierName("Rule name "+rulename+" was already present in this policy.");
			rulename = rulename + "-" + dupeRuleSuffix;
			dupeRuleSuffix++;
		}
		
		rulename = vocab.validateIdentifier(rulename, true);
		decision = vocab.validateIdentifier(decision, true);
		
		Set<Formula> thisruletarget = new HashSet<Formula>(); 
		Set<Formula> thisrulecondition = new HashSet<Formula>();
		
		// We rename, so need to connect old-name to new var.
		HashMap<String, Variable> otherVarLocals = new HashMap<String, Variable>();
		
		if(!vocab.decisions.contains(decision))
			throw new MGEUnknownIdentifier("Unknown decision type: "+decision);
		
		// For each clause in the list
		for(String conj : conjuncts)
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
					
					if(!vocab.requestVariables.containsKey(breakdown[1]))
					{
						otherVarLocals.put(breakdown[1], 
								          MFormulaManager.makeVariable(rulename+"_"+breakdown[1]));
						v1 = otherVarLocals.get(breakdown[1]);
						pred_used_request_vars_only = false;							
					}
					else
						v1 = vocab.getRequestVariable(breakdown[1]);
					
					if(!vocab.requestVariables.containsKey(breakdown[2]))
					{
						otherVarLocals.put(breakdown[2], 
								          MFormulaManager.makeVariable(rulename+"_"+breakdown[2]));
						v2 = otherVarLocals.get(breakdown[2]);
						pred_used_request_vars_only = false;							
					}
					else
						v2 = vocab.getRequestVariable(breakdown[2]);
					
					
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
							
						// Caller must have explicitly added all variables (and say what type they are) already
						if(!vocab.requestVariables.containsKey(breakdown[ii]) &&
								!vocab.otherVarDomains.containsKey(breakdown[ii]))
							throw new MGEUnknownIdentifier("Error: Unknown variable name: "+breakdown[ii]);
							
						if(!vocab.requestVariables.containsKey(breakdown[ii]))
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
		
		// Add this rule to the rule set.
		MRule newrule = new MRule();
		newrule.setDecision(decision);
		newrule.name = rulename;
		
		// If this rule has no target (or condition), the object's target (respectively condition)
		// element will be trivially true. When evaluating the rule, they are connected via .and().		

		newrule.target = MFormulaManager.makeConjunction(thisruletarget);
		newrule.condition = MFormulaManager.makeConjunction(thisrulecondition);
				
		// Any rule-scope variables here need explicit existential quantification.
		// (added to the CONDITION -- since the target cannot involve non-request vars.)
		
		for(Variable v : otherVarLocals.values())
		{
			// What was it originally?
			for(String vname : otherVarLocals.keySet())
				if(v.equals(otherVarLocals.get(vname)))
				{
					try
					{
						Decls d = MFormulaManager.makeOneOfDecl(v, vocab.otherVarDomains.get(vname));					
						newrule.condition = MFormulaManager.makeExists(newrule.condition, d);
					}
					catch(MGEManagerException e)
					{
						throw new MGEUnknownIdentifier("Error adding rule-scope quantification for rule "+rulename+". "+e);
					}
					break;
				}
		}
		
		
		newrule.target_and_condition = MFormulaManager.makeAnd(newrule.target, newrule.condition);
		
		rules.add(newrule);
		rulemap.put(newrule.name, newrule);
		
		// No IDB for the rule by itself! (Combination will add IDBS for rule *applicability* in this policy's context)
	}
	
	private Formula disjunctionOfRules(String dec)
	{
		if(disjunctionOfRules_cache.containsKey(dec))
			return disjunctionOfRules_cache.get(dec);
		
		Set<Formula> resultSet = new HashSet<Formula>();
		
		// Rules is a list, so...
		for(MRule r : rules)
		{
			// Decision can occur if r's target and condition are BOTH true
			if(r.decision().equals(dec))
				resultSet.add(r.target_and_condition);						
		}
		
		// will need to clear this out if initIDBs is recalled. 
		disjunctionOfRules_cache.put(dec, MFormulaManager.makeDisjunction(resultSet));
		
		return disjunctionOfRules_cache.get(dec);
	}
	
	protected void updateVocab(MVocab uber) throws MGEUnknownIdentifier, MGEBadIdentifierName
	{
		// Update the policy and policy formulas to use this new vocabulary.
		// ASSUMED that the vocab matches everything required. This should therefore 
		// not be called outside of MGQuery.queryThesePolicies.
		
		// First, replace instances of KodKod objects as needed:
		
		RelationAndVariableReplacementV vis = MIDBCollection.getReplacementVisitor(vocab, uber);
		
		// Target
		
		
		target = target.accept(vis);
		
		// On all rules
		for(MRule r : rules)
		{
			r.condition = r.condition.accept(vis);
			r.target = r.target.accept(vis);
			r.target_and_condition = r.target_and_condition.accept(vis); 
		}
		
		// On all IDBs.
		for(String dec : idbs.keySet())
			idbs.put(dec, idbs.get(dec).accept(vis));
			
		
		// Finally, set the reference.
		vocab = uber;
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
	
	void doCombineRules() throws MGEUnknownIdentifier, MGEBadCombinator, MGEBadIdentifierName
	{
		// This method applies the given RULE combination algorithm to the rules and populates the decision idbs.
		// If called multiple times, the idbs will be re-populated with new formulas for the most recently given combinator.
		
		// Careful: Don't replace rCombine variable...
		rCombine = rCombine.toLowerCase(); // except here
		
		//ThreadMXBean mxBean = ManagementFactory.getThreadMXBean();
		//long start = mxBean.getCurrentThreadCpuTime();	
		
		if("NONE".equals(rCombine.toUpperCase()))
		{
			// No rule combinator: Decision IDB formulas will be generated independently of each other.
			// This changes the semantics of a "policy": a request is mapped to a SET of decisions!
			
			for(String dec : vocab.decisions)			
				idbs.put(dec, disjunctionOfRules(dec));	
			
			// May have >1 rule apply!
			for(MRule r : rules)
			{
				idbs.put(vocab.validateIdentifier(r.name+"_applies", true), r.target_and_condition);
				idbs.put(vocab.validateIdentifier(r.name, true), r.target_and_condition);
			}
		}
								
		else if(rCombine.toUpperCase().startsWith("O "))
		{
			// Some decision overrides. (e.g., "O Permit" for permit overrides.)			
			
			// Simple case when only 2 decisions. But our policy type may be more complex.
			// Thus, the user gives us a total ordering on the decisions, here. 			
			// eg, for {"Call Police", "Permit", "Deny"}, the combtype "O Call-police Deny Permit" says
			// Call-Police overrides Deny and Permit, then Deny overrides Permit. 
									
			String[] ordering = rCombine.substring(2).split(" ");
			if(!vocab.isAllDecs(ordering))
				throw new MGEBadCombinator("In order to use an override rule combinator, " +
						"you must provide an ordered list of all priority decisions.\n" +
						"You provided: "+rCombine+".");
			
			Set<Formula> negprior = new HashSet<Formula>();
			for(String dec : ordering)
			{
				// Make sure this is a valid decision.
				if(!vocab.decisions.contains(dec))
					throw new MGEUnknownIdentifier("Unknown decision: "+dec);

				// For this decision, it applies if one of its rules applies AND
				// none of the prior decisions in the list applied.
				
				Formula thisdec = disjunctionOfRules(dec);
				// If there are no rules for this decision, the above will return Formula.FALSE and thus never happens
				idbs.put(dec, MFormulaManager.makeAnd(thisdec, MFormulaManager.makeConjunction(negprior)));
	
				// For each rule rendering this decision, make an IDB for when it applies
				for(MRule r : rules)
					if(r.decision().equals(dec.toLowerCase()))
					{
						idbs.put(vocab.validateIdentifier(r.name+"_applies", true),
								MFormulaManager.makeAnd(r.target_and_condition, 
								                         MFormulaManager.makeConjunction(negprior)));
						idbs.put(vocab.validateIdentifier(r.name, true), r.target_and_condition);
					}
				
				negprior.add(MFormulaManager.makeNegation(thisdec));				
			}
			
			
			// TODO all these validateIdentifier calls -- are they really needed?
			
		}
		else if(rCombine.toUpperCase().equals("FAC") || rCombine.toUpperCase().equals("FAX"))
		{
			// first applicable (in order)
			// include only TARGET of rule
			// (This combinator added for XACML support, since XACML's First Applicable uses target only) 
			
			// TARGET ONLY for XACML 2.0 standard "FA" -- see docs.
			boolean include_condition = false;
			if(rCombine.toUpperCase().equals("FAC"))
				// FAC: Full first applicable, includes conditions as well
				include_condition = true;
			
			// Make sure all the decision IDBs are appropriately initialized, since we .or onto them below.
			for(String d : vocab.decisions)
				idbs.put(d, Formula.FALSE);
	
			Set<Formula> negpriorrules = new HashSet<Formula>();
			for(MRule r : rules)
			{
				Formula thisruleapplies;				
				
				// The rule itself *always* takes condition into account before actually taking effect.
				thisruleapplies = MFormulaManager.makeAnd(r.target_and_condition, MFormulaManager.makeConjunction(negpriorrules));
				
				// Policy target is applied at end of this method, to ALL idbs.
															
				// add IDB for this rule's applicability.
				idbs.put(r.name+"_applies", thisruleapplies);
				idbs.put(r.name, r.target_and_condition);
				
				// ***************
				// Could do this, but it is very, very inefficient. 
				// Continue to do this loop so that we have RuleX_applies IDBs, but 
				// make a more efficient set of decision IDBs below.
				// ***************
				
				// add this rule (w/ prior rule restriction) to its decision's IDB 
				//idbs.put(r.decision, MGFormulaManager.makeOr(idbs.get(r.decision), thisruleapplies));									
				
				
				// keep up
				// Note: Do NOT use thisruleapplies here. Will cause rule-scope existentials, turned to 
				// universals the first time, to become existentials again due to double-negation.
				// (And besides, it's pointless and inefficient anyway.)
				if(include_condition)
					negpriorrules.add(MFormulaManager.makeNegation(r.target_and_condition));
				else										
					negpriorrules.add(MFormulaManager.makeNegation(r.target));
				
			} // end loop per rule
			
			List<MRule> backwards = new ArrayList<MRule>(rules);
			Collections.reverse(backwards);
			
			for(MRule r : backwards)
			{
				// For this rule's decision, add (current_idb OR rule)
				// For other decisions, add (current_idb AND not(rule)).
				
				for(String dec : vocab.decisions)
				{
					if(dec.toLowerCase().equals(r.decision()))
					{
						Formula newidb;

						// Always use conditions for rule actually firing
						if(include_condition)
							newidb = MFormulaManager.makeOr(idbs.get(dec), r.target_and_condition); // dec, not r.decision 
						
						// But XACML is complicated. Need to say "This rule applies fully, or its target was missed and a later rule applied."
						else
							newidb = MFormulaManager.makeOr(
									      MFormulaManager.makeAnd(idbs.get(dec), MFormulaManager.makeNegation(r.target)), r.target_and_condition); // dec, not r.decision
						idbs.put(dec, newidb); // dec
					}
					else
					{
						// Don't add conjunction if nothing so far for this decision (NOT "rule's" decision)
						if(idbs.get(dec).equals(Formula.FALSE))
							continue;
						
						// But use of conditions for rule applying depends on FAX vs. FAC.
												
						// From the XACML 2.0 docs:
						/* 
						420 In the case of the "First-applicable" combining algorithm, the combined result is the same as the
						421 result of evaluating the first <Rule>, <Policy> or <PolicySet> element in the list of rules
						422 whose target is applicable to the decision request.  */
						
						// So we need to be sure to exclude higher-priority rules EVEN IF they have the same decision? ugh...
						// (For FAC that is.)
						
						Formula newidb;
						if(include_condition)
							newidb = MFormulaManager.makeAnd(idbs.get(dec), // dec, not r.decision 
									                          MFormulaManager.makeNegation(r.target_and_condition));
						else
							newidb = MFormulaManager.makeAnd(idbs.get(dec), // dec, not r.decision 
			                          MFormulaManager.makeNegation(r.target));
						
						idbs.put(dec, newidb); // dec				
					}
				}
			}
			
		} // end FA/FAC

		else			
			throw new MGEBadCombinator("Unknown rule combination algorithm: "+rCombine);
	
		//System.out.println("Building IDBs time : " + (mxBean.getCurrentThreadCpuTime()-start) / 1000000);
		
		// *******************************************
		// Finally, each of these IDBs only apply if the _policy's_ target is met. 
		// Re-use work done in the simplifier!
		
		
		//start = mxBean.getCurrentThreadCpuTime();	
				
		// vs 150ms to build the idbs in the first place.)
		//SimplifyFormulaV simplifyVisitor = new SimplifyFormulaV();
		for(String idbname : idbs.keySet())
		{			
			idbs.put(idbname, MFormulaManager.makeAnd(idbs.get(idbname), target)); //.accept(simplifyVisitor));		
		}
		//System.out.println("Simplify idbs time: " + (mxBean.getCurrentThreadCpuTime()-start) / 1000000);

		
	}

}
