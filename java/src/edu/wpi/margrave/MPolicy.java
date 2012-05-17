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
import java.net.*;
import java.io.*;

import org.xml.sax.SAXException;

import kodkod.ast.*;

import com.sun.xacml.*;
import com.sun.xacml.ctx.Result;
import com.sun.xacml.finder.impl.*;
import com.sun.xacml.finder.*;
import com.sun.xacml.cond.*;
import com.sun.xacml.attr.*;


//***********************************************
//Supporting Objects
//***********************************************

/**
 * DO NOT instantiate this class directly. Use MPolicyLeaf.addRule instead.
 * @author Tim
 *
 */
class MRule
{
	// Stores a single rule. As caller adds rules to the policy, these objects are created.
	// Can't directly add to the idbs in the policy b/c of different policy combinator options.
	
	String name;	
	private String myDecision;

	MPolicyLeaf mypolicy;
	
	Formula target = Formula.TRUE;
	Formula condition = Formula.TRUE;
	
	// Single reference to both, must be updated whenever target or condition is.
	Formula target_and_condition = Formula.TRUE;
		
	// ids of predicates this rule refers to. Get from MExploreCondition
	Set<String> involvesPredicates = new HashSet<String>();
	
	List<Variable> ruleVarOrdering = new ArrayList<Variable>();
	
	protected MRule(MPolicyLeaf pol)
	{
		// DO NOT Instantiate directly. Instead, use MPolicyLeaf.addRule(...)
		mypolicy = pol;
	}
			
	protected void setDecision(String s)
	{
		myDecision = s;		
	}
	
	protected String decision()
	{
		return myDecision;		
	}
	
	public void toSExpression(StringBuffer buf)
	{
		buf.append("("+name+" = "+"("+decision()+" ");
		for(Variable v : ruleVarOrdering)
			buf.append(v.name()+" ");
		buf.append(") :- "+MFormulaManager.toSExpression(mypolicy.vocab, target_and_condition));
		buf.append(")"+MEnvironment.eol);		
	}
	
}


/*
 * MGIDBCollection
 * Top level class, should never be instantiated.
 * Extended by user defined view class as well as policy class. 
 */
abstract class MIDBCollection
{
	// The name of this set. (If a policy, will be the policy name.)
	String name;
	
	protected MVocab vocab;
		
	// IDBs: note that these are IF AND ONLY IF.
	// Formulas for Policy decisions, rule applicability, etc.
	// This is PRIVATE because we want to force the use of the accessors below.
	// (Return a nice friendly Formula.FALSE if the IDB isn't defined.)
	private HashMap<String, Formula> idbs = new HashMap<String, Formula>();	
	
	// The entire collection can contain IDBs with different arities and 
	// different vectors of free variables.
	
	protected Map<String, List<Variable>> varOrderings = new HashMap<String, List<Variable>>();	
	protected HashMap<Variable, Expression> varSorts = new HashMap<Variable, Expression>();	
	
	Formula getIDB(String idbname)
	{
		if(idbs.containsKey(idbname))
			return idbs.get(idbname);
		return Formula.FALSE;
	}
	void putIDB(String idbname, Formula val, List<Variable> varOrdering)
	{
		idbs.put(idbname, val);
		if(varOrdering == null)
			throw new MUserException("Internal Margrave error: putIDB given null for varOrdering. idbname="+idbname+"; collection name="+name);
		varOrderings.put(idbname, new ArrayList<Variable>(varOrdering));
		//System.err.println(idbname+":"+varOrdering);
	}
	
	boolean containsIDB(String idbname)
	{
		return idbs.containsKey(idbname);
	}
	int size()
	{
		return idbs.size();
	}
	Set<String> idbKeys()
	{
		return idbs.keySet();
	}
	String idbsAsString()
	{
		return idbs.toString();
	}
	void clearIDBs()
	{
		idbs.clear();
	}
	
	void initIDBs()
	throws MUserException
	{
		// If this is a Policy Leaf, the decisions are known, but not until the object is created and the rules are all passed.
		// If this is a Policy Set, the decisions aren't known until initIDB is called.
		// If this is a saved query, there is only one decision and it should be handled in the constructor.
		if(this instanceof MPolicyLeaf)
			((MPolicyLeaf)this).initIDBs();
		else if(this instanceof MPolicySet)
			((MPolicySet)this).initIDBs();
		else if(this instanceof MQuery)
			((MQuery)this).initIDBs();

	}

} // end MIDBCollection







/*
 * MGPolicy
 * Specifies a policy. Is either a MGPolicyLeaf or MGPolicyParent.
 * Should only instantiate children.
 */

public abstract class MPolicy extends MIDBCollection
{	
	// Formula which dictates when this policy can apply. (Used mostly for XACML.)
	public Formula target = Formula.TRUE; // default to always-applies.	
	
	Set<String> decisions = new HashSet<String>();
	
	// Affects how first-applicable is handled.
	public boolean isXACML = false;
	
	abstract String asSExpression();	
	
	MPolicy(String n, MVocab voc)
	{
		super();
		
		n = n.replaceAll("\\s+", "_");
		
		vocab = voc;
		name = n;					
	}
			
	public String printCombinators(Set<String> combineFA, Map<String, Set<String>> combineWhatOverrides)
	{
		if(combineFA.size() < 1 && combineWhatOverrides.size() < 1)
			return "None";
		
		StringBuffer theBuffer = new StringBuffer();
		
		if(combineFA.size() > 1)
		{
			theBuffer.append("FA: ");
			for(String dec : combineFA)
				theBuffer.append(dec + " ");
			theBuffer.append("\n");
		}

		if(combineWhatOverrides.size() > 1)
		{			
			for(String dec1 : combineWhatOverrides.keySet())
			{
				theBuffer.append(dec1+" overridden by: ");
				for(String dec2 : combineWhatOverrides.get(dec1))
					theBuffer.append(dec2 + " ");
			theBuffer.append("\n");
			}
		}
		
		return theBuffer.toString();
	}
	
	public void setTarget(Formula targetfmla)
	throws MGEBadIdentifierName, MGEUnknownIdentifier, MGEManagerException
	{		
		this.target = targetfmla;			
	}

	protected Expression getRelationExpr(String name) throws MGEUnknownIdentifier, MGEBadIdentifierName
	{
		boolean closure = false;

		// Defunct now; we use Expression.eq 
		//if(name.equals("="))
			//return Relation.IDEN;
		
		if(name.endsWith("*"))
		{
			closure = true;
			name = name.substring(0, name.length()-1);
		}
		
		Expression result;
		
		// make sure the predicate is known
		result = vocab.getRelation(name);
		
		if(closure)
			result = result.reflexiveClosure();
		
		return result;
	
	}
	
/*	public static MExploreCondition makeCompareCondition(MPolicy p1, MPolicy p2)
	{
		List<MExploreCondition> criteria = new ArrayList<MExploreCondition>();
		
		List<String> reqVector = new ArrayList<String>();
		for(Variable v : p1.vocab.requestVectorOrder)
			reqVector.add(v.name());

		//MEnvironment.outWriter.println(p1);
		//MEnvironment.outWriter.println(p2);
		//MEnvironment.outWriter.println(p1.name);
		//MEnvironment.outWriter.println(p2.name);
		//MEnvironment.outWriter.println(p1.varOrdering);
		//MEnvironment.outWriter.println(p2.varOrdering);
		//MEnvironment.outWriter.println(p1.varSorts);
		//MEnvironment.outWriter.println(p2.varSorts);
		
		// For each decision, generate a potential diff condition
		// vocab combination will be done in the final check
		for(String decision : p1.vocab.decisions)
		{			
			// Holds in p1, not in p2			
			MExploreCondition cond_p1 = new MExploreCondition(p1.idbs.get(decision), p1, reqVector);
			MExploreCondition cond_np2 = (new MExploreCondition(p2.idbs.get(decision), p2, reqVector)).not();			
							
			// or holds in p2 not in p1
			MExploreCondition cond_p2 = new MExploreCondition(p2.idbs.get(decision), p2, reqVector);
			MExploreCondition cond_np1 = (new MExploreCondition(p1.idbs.get(decision), p1, reqVector)).not();

			// Checking in both directions means N/a is covered.

			criteria.add(cond_p1.and(cond_np2).or(cond_p2.and(cond_np1)));					
		}
						
		return mapOrOverCriteria(criteria);
	}*/
	
	/*private static MExploreCondition mapOrOverCriteria(List<MExploreCondition> criteria)
	{
		if(criteria.size() == 1)
			return criteria.get(0);
		
		boolean isFirst = true;
		MExploreCondition result = null;
		for(MExploreCondition criterion : criteria)
		{
			if(isFirst)
			{
				isFirst = false;
				result = criterion;
			}
			else
			{
				result = result.or(criterion);
			}
		}
		
		return result;
	}*/
	
	//////////////////////////
	// TN April 2011
	// re: comparison
	// 
	// Removed compareWith functions for now since we have more "decisions" than before.
	// (We now refer to all IDBs as decisions. So the question is now, compare with
	// respect to WHICH decisions?)
	// re-implement when we enable the COMPARE command in Racket?
	//////////////////////////
	/*
	public MQuery compareWithPolicy(MPolicy p2, boolean tupling, int debugLevel, int ceilingLevel)
	throws MUserException
	{
		MExploreCondition exploreCondition = makeCompareCondition(this, p2);		
		
		// force the list to be IN ORDER (same ordering as vocab's)
		List<String> publishList = new ArrayList<String>();
		for(Variable v : vocab.requestVectorOrder)
			publishList.add(v.name());
		
		return MQuery.createFromExplore(
				exploreCondition, 
				publishList, new HashMap<String, Set<List<String>>>(), tupling, debugLevel, ceilingLevel);
	}
	
	public MQuery compareWithPolicy(MPolicy p2) 
	throws MUserException
	{		
		return compareWithPolicy(p2, false, 0, -1);				
	}
	*/
	
		
	
	/**
	 * Prints the IDBs of the policy in a (somewhat) readable manner.
	 * This is not trustworthy until initIDBs() is called.
	 * @see MPolicy.initIDBs
	 */
	public void prettyPrintIDBs()
	{
		MEnvironment.errorWriter.println("IDBs in policy "+name+":");
		for(String n : idbKeys())
		{
			MEnvironment.errorWriter.println(n+": "+getIDB(n));
			if(this instanceof MPolicyLeaf)
			{
				MEnvironment.errorWriter.println("    Uses relation names: "+((MPolicyLeaf) this).decisionUsesPredicates.get(n));
			}
		}
		MEnvironment.errorWriter.println("");
	}	
	
	public void prettyPrintEDBs()
	{
		MEnvironment.errorWriter.println("Type EDBs: ");
		
		//boolean commaneeded = false;
		for(MSort t : vocab.sorts.values())
		{
			if(t.parents.size() == 0)
				MEnvironment.errorWriter.println(t.name+" <-- base type");
			else
				MEnvironment.errorWriter.println(t.name + " < "+ t.parents +" ");			
		}
		
		MEnvironment.errorWriter.println("Non-type Predicate EDBs: ");
		for(MPredicate aPred : vocab.predicates.values())
		{
			MEnvironment.errorWriter.println(aPred.name + ": "+ aPred.type +" ");
		}

	}	
	
	

	public abstract void printPolicyInfo();
	
	// A leaf will just return the decision for a rule name, "" if no such rule.
	// A set will split by : and defer to the given child, "" if no such child
	public abstract String getDecisionForRuleIDBName(String idbname);
	
	public abstract List<String> ruleIDBsWithHigherPriorityThan(String rulename);
					
	public List<String> getIDBNameList()
	{
		// Order independent list
		ArrayList<String> result = new ArrayList<String>(size());		
		result.addAll(idbKeys());		
		return result;
	}

	public List<String> getQualifiedIDBNameList()
	{
		// Order independent list
		// Same as getIDBNameList, but include polname: prefix
		ArrayList<String> result = new ArrayList<String>(size());
		for(String idbname : idbKeys())
			result.add(name + MEnvironment.sIDBSeparator + idbname);			
		return result;
	}
		  
	public void setName(String n)
	{
		name = n.toLowerCase();
	}
	
		
	public static String convertSeparators(String s)
	{
		// Strip off leading "file:" if it is there.
		// (SISC will prepend everything with it.)
		//if(s.startsWith("file:"))
		//	s = s.substring(5);
		// ^^ Removed Sept 2010, no more SISC. - TN
		
		// Convert separator characters
		if(File.separator.equals("/"))
			s = s.replace("\\", "/");
		else if(File.separator.equals("\\"))
			s = s.replace("/", "\\");
		
		return s;
	}
	
	// Amazon SQS (JSON)
	
	public static MPolicy loadSQS(String polId, String sFileName) 
	throws MUserException
	{
		return SQSReader.loadSQS(polId, sFileName);
	}
			
	
	// XACML Input	
		
	public static MPolicy readXACML(String xacmlFileName, String xacml20SchemaFile) 
	throws MUserException
	{
		xacmlFileName = convertSeparators(xacmlFileName);
		xacml20SchemaFile = convertSeparators(xacml20SchemaFile);
		
		// Given an XACML file name, parse the policy file and return an MGPolicy object.
		//MEnvironment.errorStream.println("* (XACML) Reading file: "+xacmlFileName);
		
		// Try to load as XACML 2.0, then fall back on 1.0 (Sun's implementation).
		try
		{
			File f = new File(xacmlFileName);			
						
			MPolicy result =  XACML20Reader.loadXACML20(xacmlFileName, xacml20SchemaFile, f.getParent());
			//MEnvironment.errorStream.println("* XACML 2.0 succeeded in reading: "+result.name);
			return result;
		}
		catch(MGEUnsupportedXACML e)
		{
			// Continue with Sun's 1.0 implementation
			//MEnvironment.errorStream.println("* File was not a valid XACML 2 policy or policy set.");
		}
		catch(SAXException e)
		{
			// Continue with Sun's 1.0 implementation
			//MEnvironment.errorStream.println("* File was not a valid XACML 2 policy or policy set.");
		}
		
		//MEnvironment.errorStream.println("* Attempting to treat file as XACML 1...");
		return readXACML10(xacmlFileName);
	}
	
	public static MPolicy readXACML10(String xacmlFileName)
	throws MUserException
	{					
		MVocab voc = makeXACMLVocab(xacmlFileName);
				
        // setup the PolicyFinder and FilePolicyModule
		// FilePolicyModule does not support IDReference, so Margrave has its own mini-module.
        PolicyFinder policyFinder = new PolicyFinder();
		FilePolicyModule fpm = new FilePolicyModule();
		MXACMLPolicyFinderModule mgfind = new MXACMLPolicyFinderModule(policyFinder);        
        Set<PolicyFinderModule> policyModules = new HashSet<PolicyFinderModule>();
        policyModules.add(fpm);
        policyModules.add(mgfind);
        policyFinder.setModules(policyModules);
                      
        // Load the abstract policy via Sun's XACML API                      
        mgfind.setLocalPath(xacmlFileName);
        AbstractPolicy p = FilePolicyModule.loadPolicy(xacmlFileName, policyFinder);       
        
        if(p == null)
        	throw new MGEUnsupportedXACML("XACML 1.0 implementation could not load: "+xacmlFileName);
        
        // Instantiate MGPolicy object
        MPolicy thepol = recXACMLPolicy(p, voc);
        
        if(thepol == null)
        	throw new MGEUnsupportedXACML("XACML 1.0 converter could not create policy: "+p.getId());
        
        // Trigger calculation of decision Formulas.        
        thepol.initIDBs();
        return thepol;
        
             
	}
	
	public static MVocab makeXACMLVocab(String filename) throws MGEBadIdentifierName, MGEUnknownIdentifier
	{
		MVocab env = new MVocab();
		env.addSort("Subject"); 
		env.addSort("Action");
		env.addSort("Resource");
		env.addSort("Environment");
				
		// Note that Margrave does not support any language features that could result in an
		// Indeterminate decision. (In particular, MustBePresent, among others.)
		return env;
	}

	
	private static Formula handleXACMLTargetMatch(TargetMatch tm, MVocab env, String varname, 
			String sortname) 
	throws MGEUnsupportedXACML, MGEUnknownIdentifier, MGEBadIdentifierName, MGEManagerException
	{
		Evaluatable ev = tm.getMatchEvaluatable();

		if(!(ev instanceof AttributeDesignator))
			throw new MGEUnsupportedXACML(ev+" was not an AttributeDesignator.");
		
		if(!(tm.getMatchFunction() instanceof EqualFunction || tm.getMatchFunction() instanceof MatchFunction))
			throw new MGEUnsupportedXACML(tm.getMatchFunction() +
					" was not an Equal or Match function, which is required in the TARGET element.");
		//if(tm.getMatchFunction() instanceof MatchFunction)
		//	if("urn:oasis:names:tc:xacml:1.0:function:regexp-string-match".equals(tm.getMatchFunction().getIdentifier().toString()))
		//		throw new MGEUnsupportedXACML("The regexp-string-match function is not supported.");
		if(tm.getMatchFunction().returnsBag())
			throw new MGEUnsupportedXACML("Bag functions in the TARGET element are unsupported.");
		
		
		// TODO support for AttributeSelector in TARGET. (See 2.0 interface)
		
		// = signifies match function	
		// We add relname+":" because not all attribute names are descriptive ones. Could have overlap
		// between sorts, and that would be bad.
		String newpredname = (sortname+":"+((AttributeDesignator)ev).getId().getSchemeSpecificPart() + "="+tm.getMatchValue().encode()).toLowerCase(); 
		//String propprefix = (relname+":"+((AttributeDesignator)ev).getId().getSchemeSpecificPart() + "=").toLowerCase();
		newpredname = MVocab.validateIdentifierFromExternalPolicy(newpredname, true);
		if(!env.isSort(newpredname))
		{			
			// New predicate (NOT sort!)
			env.addPredicate(newpredname, sortname);			
			
			// Cannot infer a disjointness constraint between two values for the same attribute: 			
			// A request context may contain multiple values for the same attribute.
			// For instance, a request's subject may easily involve multiple roles.			
		}
	
		Relation r = env.getRelation(newpredname);
			
		// Only ever one variable named "s" now. So manager will have it.
		return MFormulaManager.makeAtom(MFormulaManager.makeVariable(varname), r);
	}
			
	// Otherwise the cast from List to List<List<TargetMatch>> will give a warning.
	// (The XACML 1.0 library we currently use does not use generics.)
	@SuppressWarnings("unchecked")
	private static Formula handleXACMLTarget(Target targ, MVocab env) 
	throws MGEUnsupportedXACML,MGEUnknownIdentifier, MGEBadIdentifierName, MGEManagerException
	{
		// If we see new subtypes for S/A/R, add them.
		
		// This handles SUBJECTS/ACTIONS/RESOURCES elements -- so

		// DISJUNCTION of CONJUNCTIONS -- default to false. 
		Set<Formula> subf = new HashSet<Formula>(); 
		Set<Formula> actf = new HashSet<Formula>();
		Set<Formula> resf = new HashSet<Formula>();
		
		// No restrictions!
		if(targ == null)
			return Formula.TRUE;
					
		// null for any of these below means no restrictions to target for that sort
				
		// Subjects
		if(targ.getSubjects() != null) {			 
			for(List<TargetMatch> subl : (List<List<TargetMatch>>)targ.getSubjects()) {				
				Formula thissubject = Formula.TRUE; 
				for(TargetMatch tm : subl)
					thissubject = MFormulaManager.makeAnd(thissubject, handleXACMLTargetMatch(tm, env,  "s", "Subject"));
				subf.add(thissubject);
			}
		}
		else {
			subf.add(Formula.TRUE); // no restrictions to subjects
		}
		
		// Actions
		if(targ.getActions() != null)
			for(List<TargetMatch> subl : (List<List<TargetMatch>>)targ.getActions())
			{
				Formula thisaction = Formula.TRUE;
				for(TargetMatch tm : subl)
					thisaction = MFormulaManager.makeAnd(thisaction, handleXACMLTargetMatch(tm, env, "a", "Action"));
				actf.add(thisaction);
			}
		else
			actf.add(Formula.TRUE); // no restrictions to actions

		
		// Resources
		
		if(targ.getResources() != null)
			for(List<TargetMatch> subl : (List<List<TargetMatch>>)targ.getResources())
			{
				Formula thisresource = Formula.TRUE;
				for(TargetMatch tm : subl)
					thisresource = MFormulaManager.makeAnd(thisresource, handleXACMLTargetMatch(tm, env, "r", "Resource"));
				resf.add(thisresource);
			}
		else
			resf.add(Formula.TRUE); // no restrictions to resources


		// Interface does not provide a "getEnvironments" method...
		

		Set<Formula> allparts = new HashSet<Formula>();
		allparts.add( MFormulaManager.makeDisjunction(subf));
		allparts.add( MFormulaManager.makeDisjunction(actf));
		allparts.add( MFormulaManager.makeDisjunction(resf));
		return MFormulaManager.makeConjunction(allparts);
	}
		
	private static String buildConditionPredicate(Apply func, Set<String> involves) throws MGEUnsupportedXACML
	{
		String result = func.getFunction().getIdentifier().getSchemeSpecificPart().toString() + "(";
		String connective = "";
		
//		foo.getFunction(); // function
		//foo.getChildren(); // conditions
		

		
		// for each function parameter
		for(Object _child : func.getChildren())
		{			
			if(!(_child instanceof Evaluatable))
				throw new MGEUnsupportedXACML("buildConditionPredicate expected Evaluatable object, got: "+_child);
			Evaluatable child = (Evaluatable) _child;
			
			if(child instanceof Apply)
			{
				// another function application				
				result = result + connective + buildConditionPredicate((Apply)child, involves);
				connective = ",";
				
			}
			else if (child instanceof AttributeValue)
			{
				// constant value
				result = result + connective + ((AttributeValue)child).encode();
				connective = ",";
			}
			else if(child instanceof AttributeSelector)
			{
				// AttributeSelector is used for resolving values from the request using XPath expressions
				// (For instance, reading from the resource file to see if it is a letter addressed to the subject.)
				// Always returns a Bag of attribute values!
				
				result = result + connective + ((AttributeSelector) child).getContextPath();
			}
			else if(child instanceof AttributeDesignator)
			{
				
				AttributeDesignator childdesig = (AttributeDesignator)child;
				if(childdesig.getDesignatorType() == AttributeDesignator.SUBJECT_TARGET)
				{
					result = result + connective + "subject:" + childdesig.getId().getSchemeSpecificPart();
					involves.add("s");
				}
				else if (childdesig.getDesignatorType() == AttributeDesignator.ACTION_TARGET)
				{
					result = result + connective + "action:" + childdesig.getId().getSchemeSpecificPart();
					involves.add("a");
				}
				else if (childdesig.getDesignatorType() == AttributeDesignator.RESOURCE_TARGET)
				{					
					result = result + connective + "resource:" + childdesig.getId().getSchemeSpecificPart();
					involves.add("r");
				}
				else if (childdesig.getDesignatorType() == AttributeDesignator.ENVIRONMENT_TARGET)
				{
					result = result + connective + "environment:"+childdesig.getId().getSchemeSpecificPart();
					involves.add("e");
				}
				else				
					throw new MGEUnsupportedXACML("AttributeDesignator with non subj/act/res/env type.");
				
				connective = ",";
			}
			else
				throw new MGEUnsupportedXACML("Unexpected type in Apply child: "+child);
		}
		
		return MVocab.validateIdentifierFromExternalPolicy(result+")", true);		
	}
	
	private static Formula handleXACMLRuleCondition(Apply cond, MVocab env)
	throws MGEUnsupportedXACML, MGEManagerException, MGEBadIdentifierName
	{
		Formula aCondition = Formula.TRUE;		
		if(cond == null) return aCondition; // no condition!
							
		// Build a state predicate that governs whether or not this condition holds.
		// Predicate name will be this function's name (child names, possibly recursively derived)

		Set<String> involves = new HashSet<String>();		
		String newname = buildConditionPredicate(cond, involves);
						
		// Which variables got used?
		List<String> involves_list = new ArrayList<String>();
		if(involves.contains("s"))
			involves_list.add("Subject");
		if(involves.contains("a"))
			involves_list.add("Action");
		if(involves.contains("r"))
			involves_list.add("Resource");
		if(involves.contains("e"))
			involves_list.add("Environment");
					
		// We want to support "nullary" state predicates. The problem is that 
		// Kodkod requires Relation arity to always be >= 1. 
		// So we kludge it: If a Condition is truly nullary, we pretend it depends
		// on the Environment.		

		if(involves_list.size() < 1)
			involves_list.add("Environment");
		
		try
		{			
			String construct = "";
			for(String s : involves_list)	
				construct = construct + s + " ";
			
			// create the predicate
			env.addPredicate(newname, construct);
			
		}
		catch(MGEBadIdentifierName e)
		{
			throw new MGEUnsupportedXACML("Identifier problem: "+e.getMessage());
		}
			
		
		// create the formula for this condition in the rule object		
		
		try
		{  						
			List<String> varsList = new ArrayList<String>(involves_list.size());
			for(String s : involves)
				varsList.add(s);			
			
			// New XACML library has an Expression type as well. Be specific.			
			kodkod.ast.Expression tuple = MFormulaManager.makeExprTuple(varsList);
			return MFormulaManager.makeAtom(tuple, env.getRelation(newname)); 
		}
		catch(MGEUnknownIdentifier e)
		{
			throw new MGEUnsupportedXACML("Identifier problem with getVariable: "+e.getMessage());
		}		
		
	}
	
	private static void addXACMLRule(MPolicyLeaf pol, Rule r, MVocab env)
	throws MGEUnsupportedXACML,MGEUnknownIdentifier, MGEBadIdentifierName, MGEManagerException
	{
		// A rule target in XACML contains a SUBJECTS element
		// A SUBJECTS element contains a *disjunctive* sequence of SUBJECT elements.
		// A SUBJECT element contains a *conjunctive* sequence of SubjectMatch elements.
		// (and similarly for resources and actions)
		
		// For the moment, 1 XACML rule -> 1 Margrave Rule. This means that Margrave rules may not be
		// simple conjunctive queries anymore. But we could factor out if we needed to.
		
		List<String> ruleVarNameOrdering = new ArrayList<String>(4);
		ruleVarNameOrdering.add("s");
		ruleVarNameOrdering.add("a");
		ruleVarNameOrdering.add("r");
		ruleVarNameOrdering.add("e");
			
		
		// Decision
		String decision = "";
		if(r.getEffect() == Result.DECISION_PERMIT)
			decision = "permit";
		else
			decision = "deny";
	
		// target
		Formula aTarget = handleXACMLTarget(r.getTarget(), env); 
		
		// Condition
		Formula aCondition = handleXACMLRuleCondition(r.getCondition(), env);			
		
		pol.addRule(r.getId().toString().toLowerCase(), decision, ruleVarNameOrdering, aTarget, aCondition);	
	}
	
	private static MPolicy recXACMLPolicy(AbstractPolicy p, MVocab voc) throws 
	MGEBadCombinator, MGEUnsupportedXACML, MGEUnknownIdentifier, MGEBadIdentifierName, MGEManagerException
	{		
		// Policy Target
		Target targ = p.getTarget();		
	
		if(p.getChildren() == null || p.getChildren().size() < 1)
		{
			// no children, return empty leaf policy
			// (In XACML parser, "children" is EITHER rules or proper children.)
			MPolicyLeaf pol = new MPolicyLeaf(p.getId().toString(), voc);					
			pol.declareVariable("s", "Subject");
	        pol.declareVariable("a", "Action");
	        pol.declareVariable("r", "Resource");
	        pol.declareVariable("e", "Environment");
			return pol;
		}
					
		// Children (if any)
		else if((p instanceof PolicySet) ||
				
				// Children via *reference*  
				// No obvious way to get at the reference's type
				// That is, whether it's a POLICYSET_REFERENCE or a POLICY_REFERENCE.
				
				(p instanceof PolicyReference && !(p.getChildren().get(0) instanceof Rule)))
		{
			
			MPolicySet pol = new MPolicySet(p.getId().toString(), voc);
			pol.target = handleXACMLTarget(targ, voc); // env may be updated with new sorts		
			pol.isXACML = true;

			// Combining Alg
			
			pol.handleXACMLCombine(p.getCombiningAlg());	
		
			for(Object _cp : p.getChildren())
			{
				if(!(_cp instanceof AbstractPolicy))
					throw new MGEUnsupportedXACML("recXACMLPolicy expected AbstractPolicy. Got: "+_cp);
				AbstractPolicy cp = (AbstractPolicy) _cp;				
				
				pol.addChild(recXACMLPolicy(cp, voc));				
			}
			
						
			return pol;
		}

		// Rules (if any)
		// Root Policy, or reference to a root policy
		else
		{
			
			MPolicyLeaf pol = new MPolicyLeaf(p.getId().toString(), voc);
			pol.target = handleXACMLTarget(targ, voc); // env may be updated with new sorts	
			pol.isXACML = true;

			pol.declareVariable("s", "Subject");
	        pol.declareVariable("a", "Action");
	        pol.declareVariable("r", "Resource");
	        pol.declareVariable("e", "Environment");

			// Combining Alg
			pol.handleXACMLCombine(p.getCombiningAlg()); 
			
			for(Object _r : p.getChildren())
			{				
				Rule r = (Rule) _r;
				addXACMLRule(pol, r, voc);
			}
			return pol;
			
		}		
		// do nothing after this conditional
	} 
}

class MXACMLPolicyFinderModule extends PolicyFinderModule
{
	PolicyFinder finder;
	String dir;
	
	HashMap<URI, AbstractPolicy> cache = new HashMap<URI, AbstractPolicy>();
	
	public MXACMLPolicyFinderModule(PolicyFinder pf)
	{
		finder = pf;
		dir = "";
	}
	
	public void init(PolicyFinder pf)
	{
		finder = pf;
	}
		
	public void setLocalPath(String path)
	{
		File file = new File(path);        
		if(file.isDirectory())
			dir = file.getPath();
		else
			dir = file.getParent();
	}
	
	public PolicyFinderResult findPolicy(URI idReference, int type) 
	{
		// Allow parsing of policy sets that reference external policies or policysets.
		
		// TODO this won't find policies with ID different from their filename.
		// Note 10/12/09 -- see XACML 2.0
				
		
		// The docs say it is the responsibility of the finder object to cache.
		// This is called _every time_ we need to do something with an AbstractPolicy
		// that is really a reference (it won't automatically resolve to a real policy.)
		
		if(cache.containsKey(idReference))
			return new PolicyFinderResult(cache.get(idReference));
		
		AbstractPolicy pol = FilePolicyModule.loadPolicy(dir + File.separator + idReference.toString() + ".xml", finder);
		
		
		if(pol == null)
		{
			cache.put(idReference, null);
			return new PolicyFinderResult();
		}
		else
		{
			cache.put(idReference, pol);
			return new PolicyFinderResult(pol);
		}
	}
	
	public boolean isIdReferenceSupported()
	{
		return true;
	}
	
}
