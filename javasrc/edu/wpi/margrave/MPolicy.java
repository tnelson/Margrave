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

import java.util.*;
import java.net.*;
import java.io.*;

import org.w3c.dom.Document;
import org.xml.sax.SAXException;

import kodkod.ast.*;

import com.sun.xacml.*;
import com.sun.xacml.ctx.Result;
import com.sun.xacml.finder.impl.*;
import com.sun.xacml.finder.*;
import com.sun.xacml.cond.*;
import com.sun.xacml.attr.*;
import com.sun.xacml.combine.*;


//***********************************************
//Supporting Objects
//***********************************************

class MRule
{
	// Stores a single rule. As caller adds rules to the policy, these objects are created.
	// Can't directly add to the idbs in the policy b/c of different policy combinator options.
	
	String name;
	private String myDecision;
	Formula target;
	Formula condition;
	
	// Single reference to both, must be updated whenever target or condition is.
	Formula target_and_condition;
		
	MRule()
	{
		target = Formula.TRUE;
		condition = Formula.TRUE;
		
		target_and_condition = Formula.TRUE;
	}
	
	protected void setDecision(String s)
	{
		myDecision = s.toLowerCase();
	}
	
	protected String decision()
	{
		return myDecision;
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
	
	MVocab vocab;
		
	// IDBs: note that these are IF AND ONLY IF.
	// Formulas for Policy decisions, rule applicability, etc.
	protected HashMap<String, Formula> idbs = new HashMap<String, Formula>();	
	
	// The entire collection also has a single distinct vector of free variables
	// over which all its idbs range. Not all idbs use all these variables.
	protected List<Variable> varOrdering = new ArrayList<Variable>();
	
	protected HashMap<Variable, Expression> varSorts = new HashMap<Variable, Expression>();	
	
	protected static RelationAndVariableReplacementV getReplacementVisitor(MVocab vocab, MVocab uber) 
	throws MGEUnknownIdentifier, MGEBadIdentifierName
	{
		HashMap<Variable, Variable> varpairs = new HashMap<Variable, Variable>();
		HashMap<Relation, Relation> relpairs = new HashMap<Relation, Relation>();
		
		// Each type 
		for(MSort t : vocab.sorts.values())
		{
			Relation newrel = uber.getRelation(t.name);
			if(newrel != t.rel)
				relpairs.put(t.rel, newrel);
		}
		
		// Each predicate
		for(String s : vocab.predicates.keySet())
		{
			Relation newrel = uber.predicates.get(s);
			Relation oldrel = vocab.predicates.get(s);
			if(oldrel != newrel)
				relpairs.put(oldrel, newrel);
		}
		
		// Each request var
		for(String s : vocab.requestVariables.keySet())
		{
			Variable oldvar = vocab.requestVariables.get(s);
			Variable newvar = uber.requestVariables.get(s);
			if(oldvar != newvar)
				varpairs.put(oldvar, newvar);
		}
		
		return new RelationAndVariableReplacementV(relpairs, varpairs);
	}	

}







/*
 * MGPolicy
 * Specifies a policy. Is either a MGPolicyLeaf or MGPolicyParent.
 * Should only instantiate children.
 */

public abstract class MPolicy extends MIDBCollection
{	
	// Formula which dictates when this policy can apply. (Used mostly for XACML.)
	public Formula target;
	
	// Assumptions
	public MConstraints assumptions;
	
	
	MPolicy(String n, MVocab voc)
	{
		super();
		
		vocab = voc;
		name = n.toLowerCase();				
		
		
		// This policy is an IDB collection; it must publish the vector that its IDBs take.
		varOrdering = new ArrayList<Variable>(vocab.requestVectorOrder);		
		for(String s : vocab.requestVariables.keySet())
		{		
			if(vocab.requestVarDomains.containsKey(s))
				varSorts.put(vocab.requestVariables.get(s), vocab.requestVarDomains.get(s));
		}
		
		
		target = Formula.TRUE; // default to always-applies.
		
		// Presumably the vocab object has some decisions in it. Initialize our *decision* IDBs
		for(String d : vocab.decisions)
			if(!idbs.containsKey(d))
				idbs.put(d, Formula.FALSE);

		assumptions = new MConstraints(n+"_assumptions", voc);
	}
			
	public void setTarget(List<String> conjuncts)
	throws MGEBadIdentifierName, MGEUnknownIdentifier, MGEManagerException
	{
		// Elements of conjunction must be of the form "Domain var" or "Subdomain var"
		// where "var" is in requestVariables.
		// Anything else will cause an error!
		
		Set<Formula> targetSet = new HashSet<Formula>();
		
		for(String s : conjuncts)
		{
			s = s.toLowerCase();
			String breakdown[] = s.split(" ");
			
			if(!vocab.requestVariables.containsKey(breakdown[1]))
				throw new MGEBadIdentifierName("Identifier "+breakdown[1]+" is not a request variable.");
			if(!vocab.isSort(breakdown[0]))
				throw new MGEBadIdentifierName("Relation "+breakdown[0]+" is not a type.");

			if("=".equals(breakdown[0]))
			{
				// EQ
				// Bad variable name exception will propagate up.
				Variable v1 = vocab.getRequestVariable(breakdown[1]);
				Variable v2 = vocab.getRequestVariable(breakdown[2]);
				targetSet.add(MFormulaManager.makeEqAtom(v1, v2));
			}
			else
			{
				// IN
				Expression r = getRelationExpr(breakdown[0]);			
				targetSet.add(MFormulaManager.makeAtom(vocab.requestVariables.get(breakdown[1]), r));
			}
		}
		
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

	
	
	/**
	 * Returns a Margrave query representing the requests for which this policy and another 
	 * will disagree on the decision to be made.	 
	 * @param p2 The other policy to compare with this one
	 * @return A Margrave Query object
	 * @throws MGECannotDiff
	 * @throws MGEUnknownIdentifier
	 * @throws MGEArityMismatch
	 * @throws MGEBadQueryString
	 * @throws MGECombineVocabs
	 * @throws MGEBadIdentifierName
	 * @throws MGEUnsortedVariable 
	 * @throws MGEManagerException 
	 */
	public MQuery compareWithPolicy(MPolicy p2) throws MGECombineVocabs, MGEUnknownIdentifier, MGEArityMismatch, 
	MGEBadQueryString, MGEBadIdentifierName, MGEUnsortedVariable, MGEManagerException
	{
		// All query creation must go through MGQuery.queryThesePolicies. So construct a query string.
		String thequery = "";
		String reqvars = "";
		String suffix = "";
	
		// A problem with incompatible vocabularies will be handled below, by the queryThesePolicies method.
		
		for(Variable v : vocab.requestVectorOrder)
		{
			if(reqvars.length() < 1)
				reqvars = v.name();
			else
				reqvars = reqvars.concat(" "+v.name());
			
			Relation varsort = vocab.requestVarDomains.get(v.name());
			thequery = thequery.concat("(forSome "+v.name()+" "+varsort.name()+" ");
			suffix = suffix.concat(")");
		}
		
		thequery = thequery.concat(" (or ");
		suffix = suffix.concat(")");
		
		for(String decision : vocab.decisions)
		{			
			// If P1 ever decides this way, but P2 does not. (And vice versa.)
			thequery = thequery + " (and ("+this.name+":"+decision+" "+reqvars+") (not ("
			                              +p2.name+":"+decision+" "+reqvars+"))) "
			                              
			                     +"(and ("+p2.name+":"+decision+" "+reqvars+") (not ("
			                              +this.name+":"+decision+" "+reqvars+")))";
			
			// Note that since we check in both directions, the N/a <-> non-N/a possibilities are covered. 
		}

		thequery = thequery.concat(suffix);
		
		ArrayList<MIDBCollection> pollist = new ArrayList<MIDBCollection>();
		pollist.add(this);
		pollist.add(p2);
				
		MQuery result = MQuery.queryThesePolicies(thequery, pollist);
		return result;
		
	}
	
	
	/**
	 * Given a text query, create a query object that will run the requested query on this policy.
	 * @param prop The string describing the query
	 * @return MGQuery object
	 * @throws MGEBadQueryString
	 * @throws MGEUnknownIdentifier
	 * @throws MGEArityMismatch
	 * @throws MGECombineVocabs
	 * @throws MGEBadIdentifierName
	 * @throws MGEUnsortedVariable 
	 * @throws MGEManagerException 
	 */
	public MQuery queryPolicy(String prop) 
	throws MGEBadQueryString, MGEUnknownIdentifier, MGEArityMismatch, MGECombineVocabs, 
	MGEBadIdentifierName, MGEUnsortedVariable, MGEManagerException
	{
		// Convert the text query into a MGQuery object.
		ArrayList<MIDBCollection> temp = new ArrayList<MIDBCollection>();
		temp.add(this);
		
		return MQuery.queryThesePolicies(prop, temp);
	}
		
	
	
	/**
	 * Prints the IDBs of the policy in a (somewhat) readable manner.
	 * This is not trustworthy until initIDBs() is called.
	 * @see MPolicy.initIDBs
	 */
	public void prettyPrintIDBs()
	{
		MEnvironment.errorStream.println("IDBs in policy "+name+":");
		for(String n : idbs.keySet())
		{
			MEnvironment.errorStream.println(n+": "+idbs.get(n));
		}
		MEnvironment.errorStream.println("");
	}	
	
	public void prettyPrintEDBs()
	{
		MEnvironment.errorStream.println("Request Type EDBs: ");
		
		//boolean commaneeded = false;
		for(MSort t : vocab.sorts.values())
		{
			if(t.parent == null)
				MEnvironment.errorStream.println(t.name+" <-- base type");
			else
				MEnvironment.errorStream.println(t.name + "( "+ vocab.getUniverseSort(t).name +" )");
			
			/*if(commaneeded)
				MEnvironment.errorStream.print(", ");
			MEnvironment.errorStream.print(t.name);
			commaneeded = true;*/
		}
		
		MEnvironment.errorStream.println("State EDBs: ");
		for(String relname : vocab.predicates.keySet())
		{
			MEnvironment.errorStream.println(relname + "( "+ vocab.predtypes.get(relname) +" )");
			/*if(commaneeded)
				MEnvironment.errorStream.print(", ");
			MEnvironment.errorStream.print(relname);
			commaneeded = true;*/
		}

	}	
	
	

	public abstract void printPolicyInfo();
	
	// A leaf will just return the decision for a rule name, "" if no such rule.
	// A set will split by : and defer to the given child, "" if no such child
	public abstract String getDecisionForRuleIDBName(String idbname);
	
	public abstract List<String> ruleIDBsWithHigherPriorityThan(String rulename);
	
	public void addDisjointAssumption(String name1, String name2) 
	throws MGEUnknownIdentifier, MGEBadCombinator, MGEBadQueryString, MGEArityMismatch, MGEManagerException, MGEBadIdentifierName
	{
		// Allow the Scheme UI to say things like "A subject will never have role=faculty and role=student at the same time."
		// This is convenient, but hides potential policy flaws. Add such constraints at your own risk.
		// Now treated as a policy ASSUMPTION, not an axiom.
		
		assumptions.addConstraintDisjoint(name1, name2);	
		initIDBs();
	}
	
	public void addSingletonAssumption(String name)
	throws MGEUnknownIdentifier, MGEBadCombinator, MGEBadQueryString, MGEArityMismatch, MGEManagerException, MGEBadIdentifierName
	{
		// "There is precisely one action." See above for warning.
		assumptions.addConstraintSingleton(name);
		initIDBs();
	}
	
	public void addDisjointPrefixAssumption(String prefix)
	throws MGEUnknownIdentifier, MGEBadCombinator, MGEBadQueryString, MGEArityMismatch, MGEManagerException, MGEBadIdentifierName
	{
		// "All types with this name prefix are disjoint." This is used to say that certain XACML attributes cannot have >1 value...
		
		List<String> disj = new ArrayList<String>();
		// list of pairwise disjoint sorts
		
		for(MSort t : vocab.sorts.values())
			if(t.name.startsWith(prefix))
				disj.add(t.name);
		
		assumptions.addConstraintDisjoint(disj);
		initIDBs();
	}

	public void addSubsetAssumption(String child, String parent)
	throws MGEUnknownIdentifier, MGEBadCombinator, MGEBadQueryString, MGEArityMismatch, MGEManagerException, MGEBadIdentifierName
	{
		// "Something in child is always in parent as well."
		assumptions.addConstraintSubset(child, parent);
		initIDBs();
	}
		
	public String getExistentialRequestPrefix()
	{
		// Return a query string prefix coorisponding to the request vector
		// used in automated query generation

		String result = "";
		for(Variable v : vocab.requestVectorOrder)
		{			
			Relation varsort = vocab.requestVarDomains.get(v.name());
			result = result.concat("(forSome "+v.name()+" "+varsort.name()+" ");
		}

		return result;
	}

	public String getRequestPrefixClosing()
	{
		String result = "";
		for(Variable v : vocab.requestVectorOrder)
		{						
			result = result.concat(")");
		}

		return result;
	}

	public String getRequestVarVector()
	{
		String result = "";
		for(Variable v : vocab.requestVectorOrder)
		{			
			if(result.length() < 1)
				result = result.concat(v.name());
			else
				result = result.concat(" "+v.name());
		}

		return result;
	}
	
	public String getRequestVarVectorWithCommas()
	{
		String result = "";
		for(Variable v : vocab.requestVectorOrder)
		{			
			if(result.length() < 1)
				result = result.concat(v.name());
			else
				result = result.concat(", "+v.name());
		}

		return result;
	}

	
	public List<String> getIDBNameList()
	{
		// Order independent list
		ArrayList<String> result = new ArrayList<String>(idbs.size());		
		result.addAll(idbs.keySet());		
		return result;
	}

	public List<String> getQualifiedIDBNameList()
	{
		// Order independent list
		// Same as getIDBNameList, but include polname: prefix
		ArrayList<String> result = new ArrayList<String>(idbs.size());
		for(String idbname : idbs.keySet())
			result.add(name + ":" + idbname);			
		return result;
	}

	public List<String> getRequestVarList()
	{
		ArrayList<String> result = new ArrayList<String>(vocab.requestVariables.size());
		for(Variable var : vocab.requestVectorOrder)
			result.add(var.name());			
		return result;
	}
	
	public abstract void initIDBs()
	throws MGEBadCombinator, MGEUnknownIdentifier, MGEArityMismatch, 
	       MGEBadQueryString, MGEManagerException, MGEBadIdentifierName;
	  
	public void setName(String n)
	{
		name = n.toLowerCase();
	}
	
	void handlePolicyAssumptions() throws MGEBadQueryString, MGEArityMismatch, MGEUnknownIdentifier, MGEManagerException, MGEBadIdentifierName
	{
		// Predicate for "my assumptions have been violated in this model."
		Formula assumptionsok = MFormulaManager.makeConjunction(assumptions.getConstraintFormulas(this)); 
		
		// Don't bother if no assumptions
		if(assumptionsok.equals(Formula.TRUE))
			return;
		
		//MEnvironment.errorStream.println("Building assumption IDBs for policy: "+name);
		
		for(String dec : vocab.decisions)
		{
			// New decision to cover the policy saying "I'm gonna permit, but you told me this couldn't happen."
			idbs.put(dec+"-ec", MFormulaManager.makeAnd(idbs.get(dec), MFormulaManager.makeNegation(assumptionsok)));
			
			// Now the decision is split into EC and non-EC
			idbs.put(dec, MFormulaManager.makeAnd(idbs.get(dec), assumptionsok));
		}
	}
	
	
	public static String convertSeparators(String s)
	{
		// Strip off leading "file:" if it is there.
		// (SISC will prepend everything with it.)
		if(s.startsWith("file:"))
			s = s.substring(5);
		
		// Convert separator characters
		if(File.separator.equals("/"))
			s = s.replace("\\", "/");
		if(File.separator.equals("\\"))
			s = s.replace("/", "\\");
		
		return s;
	}
	
	// Amazon SQS (JSON)
	
	public static MPolicy loadSQS(String sFileName) 
	throws MGEBadIdentifierName, MGEUnknownIdentifier, MGEUnsupportedSQS, MGEManagerException, MGEBadCombinator, MGEArityMismatch, MGEBadQueryString
	{
		return SQSReader.loadSQS(sFileName);
	}
		
	
	// JUNOS Input
	public static void readJUNIS(String junosFileName)
	{
		// Invoke parser
		
		// TODO
		
		// void because --- what should it return? there are multiple "policies".
	}
	
	
	// XACML Input	
		
	public static MPolicy readXACML(String xacmlFileName, String xacml20SchemaFile) 
	throws MGEBadIdentifierName, MGEBadCombinator, MGEUnsupportedXACML, MGEUnknownIdentifier, 
	MGEArityMismatch, MGEBadQueryString, MGEManagerException
	{
		xacmlFileName = convertSeparators(xacmlFileName);
		xacml20SchemaFile = convertSeparators(xacml20SchemaFile);
		
		// Given an XACML file name, parse the policy file and return an MGPolicy object.
		MEnvironment.errorStream.println("* (XACML) Reading file: "+xacmlFileName);
		
		// Try to load as XACML 2.0, then fall back on 1.0 (Sun's implementation).
		try
		{
			File f = new File(xacmlFileName);			
						
			MPolicy result =  XACML20Reader.loadXACML20(xacmlFileName, xacml20SchemaFile, f.getParent());
			MEnvironment.errorStream.println("* XACML 2.0 succeeded in reading: "+result.name);
			return result;
		}
		catch(MGEUnsupportedXACML e)
		{
			// Continue with Sun's 1.0 implementation
			MEnvironment.errorStream.println("* File was not a valid XACML 2 policy or policy set.");
		}
		catch(SAXException e)
		{
			// Continue with Sun's 1.0 implementation
			MEnvironment.errorStream.println("* File was not a valid XACML 2 policy or policy set.");
		}
		
		MEnvironment.errorStream.println("* Attempting to treat file as XACML 1...");
		return readXACML10(xacmlFileName);
	}
	
	public static MPolicy readXACML10(String xacmlFileName)
	throws MGEBadIdentifierName, MGEBadCombinator, MGEUnsupportedXACML, 
	       MGEUnknownIdentifier, MGEArityMismatch, MGEBadQueryString,
	       MGEManagerException
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
        	throw new MGEUnsupportedXACML("XACML 1.0 implementation returned null when loading: "+xacmlFileName);
        
        // Instantiate MGPolicy object
        MPolicy thepol = recXACMLPolicy(p, voc);
        
        if(thepol == null)
        	throw new MGEUnsupportedXACML("XACML 1.0 converter returned null when creating policy: "+p.getId());
        
        // Trigger calculation of decision Formulas.        
        thepol.initIDBs();
        return thepol;
        
             
	}
	
	public static MVocab makeXACMLVocab(String filename) throws MGEBadIdentifierName, MGEUnknownIdentifier
	{
		MVocab env = new MVocab("VocabFor_"+filename);
		env.addSort("subject"); 
		env.addSort("action");
		env.addSort("resource");
		env.addSort("environment");
		
		env.addRequestVar("s", "subject");
		env.addRequestVar("a", "action");
		env.addRequestVar("r", "resource");
		env.addRequestVar("e", "environment");
		
		env.addDecision("permit");
		env.addDecision("deny");
		
		// Note that Margrave does not support any language features that could result in an
		// Indeterminate decision. (In particular, MustBePresent, among others.)
		return env;
	}

	
	private static Formula handleXACMLTargetMatch(TargetMatch tm, MVocab env, String varname, 
			String relname) 
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
		String newpredname = (relname+":"+((AttributeDesignator)ev).getId().getSchemeSpecificPart() + "="+tm.getMatchValue().encode()).toLowerCase(); 
		//String propprefix = (relname+":"+((AttributeDesignator)ev).getId().getSchemeSpecificPart() + "=").toLowerCase();
		
		if(!env.isSort(newpredname))
		{			
			// New predicate
			env.addSubSort(relname, newpredname);
			
			// Cannot infer a disjointness constraint between two values for the same attribute: 			
			// A request context may contain multiple values for the same attribute.
			// For instance, a request's subject may easily involve multiple roles.			
		}
	
		Relation r = env.getRelation(newpredname);
			
		return MFormulaManager.makeAtom(env.requestVariables.get(varname), r);
	}
	
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
		if(targ.getSubjects() != null)
			for(List<TargetMatch> subl : (List<List<TargetMatch>>)targ.getSubjects())
			{				
				Formula thissubject = Formula.TRUE; 
				for(TargetMatch tm : subl)
					thissubject = MFormulaManager.makeAnd(thissubject, handleXACMLTargetMatch(tm, env,  "s", "Subject"));
				subf.add(thissubject);
			}
		else
			subf.add(Formula.TRUE); // no restrictions to subjects
				
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
	
	private static String handleXACMLCombine(CombiningAlgorithm ca) throws MGEBadCombinator
	{
		if(ca instanceof DenyOverridesPolicyAlg || ca instanceof DenyOverridesRuleAlg)
			return "O deny permit";
		if(ca instanceof PermitOverridesPolicyAlg || ca instanceof PermitOverridesRuleAlg)
			return "O permit deny";
		if(ca instanceof FirstApplicablePolicyAlg || ca instanceof FirstApplicableRuleAlg)
			return "FAX"; // xacml FA, not intuitive FA
				
		throw new MGEBadCombinator("Unsupported combining algorithm: "+ca);		
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
		
		return result+")";		
	}
	
	private static void handleXACMLRuleCondition(MRule mr, Apply cond, MVocab env)
	throws MGEUnsupportedXACML, MGEManagerException, MGEBadIdentifierName
	{
		mr.condition = Formula.TRUE;		
		if(cond == null) return; // no condition!
							
		// Build a state predicate that governs whether or not this condition holds.
		// Predicate name will be this function's name (child names, possibly recursively derived)

		Set<String> involves = new HashSet<String>();		
		String newname = buildConditionPredicate(cond, involves);
		
		// TODO nullary predicates: We fixed this in XACML 2.0 parser
		// need to migrate fix over. (The problem is that KodKod doesn't allow nullary relations)
		if(involves.size() < 1)
			throw new MGEUnsupportedXACML("Condition involved no part of the request: "+newname);
		
		// assemble type construct string
		// order matters!
		List<String> involves_list = new ArrayList<String>(involves);
			
		try
		{			
			String construct = "";
			for(String s : involves_list)	
				construct = construct + env.requestVarDomains.get(s).name() + " ";
			
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
			for(String s : involves_list)
				varsList.add(s);			
			
			// New XACML library has an Expression type as well. Be specific.			
			kodkod.ast.Expression tuple = MFormulaManager.makeVarTuple(varsList);
			mr.condition = MFormulaManager.makeAtom(tuple, env.getRelation(newname)); 
		}
		catch(MGEUnknownIdentifier e)
		{
			throw new MGEUnsupportedXACML("Identifier problem with getVariable: "+e.getMessage());
		}
		

		// TODO -- assumption propagation -- does it matter?
		// (Do we want parent policies to *explicitly* carry over their children's assumptions?)
		
		
	}
	
	private static void addXACMLRule(MPolicyLeaf pol, Rule r, MVocab env)
	throws MGEUnsupportedXACML,MGEUnknownIdentifier, MGEBadIdentifierName, MGEManagerException
	{
		// A rule target in XACML contains a SUBJECTS element
		// A SUBJECTS element contains a *disjunctive* sequence of SUBJECT elements.
		// A SUBJECT element contains a *conjunctive* sequence of SubjectMatch elements.
		// (and similarly for resources and actions)
		
		// For the moment, 1 XACML rule -> 1 Margrave Rule. This means that Margrave rules may not be
		// simple conjunctive queries anymore. But we could factor out if we needed to, right?
		
		MRule mr = new MRule();
		mr.name = r.getId().toString().toLowerCase();
		
		// Decision
		if(r.getEffect() == Result.DECISION_PERMIT)
			mr.setDecision("permit");
		else
			mr.setDecision("deny");
	
		// target
		mr.target = handleXACMLTarget(r.getTarget(), env); //.accept(new SimplifyFormulaV());
		
		// Condition
		handleXACMLRuleCondition(mr, r.getCondition(), env);			
		
		mr.target_and_condition = MFormulaManager.makeAnd(mr.target, mr.condition);
		
		// Add the rule
		pol.rules.add(mr);
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
			
			return new MPolicyLeaf(p.getId().toString(), voc);
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

			// Combining Alg
			
			pol.pCombine = handleXACMLCombine(p.getCombiningAlg());	
		
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

			// Combining Alg
			pol.rCombine = handleXACMLCombine(p.getCombiningAlg()); 
			
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

//Used for tupling
class MInternalIDBCollection extends MIDBCollection
{
	
	protected MInternalIDBCollection(String n, MVocab voc)
	{
		idbs = new HashMap<String, Formula>();
		name = n;		
		vocab = voc;
	}

	protected void addIDB(String idbname, Formula idb)
	{
		idbs.put(idbname, idb);
	}
	
}
