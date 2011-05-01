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


package edu.wpi.margrave;

import java.lang.*;
import java.util.*;
import java.io.*;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;

import org.w3c.dom.DOMImplementation;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Text;

import kodkod.ast.*;
import kodkod.ast.operator.Multiplicity;
import kodkod.instance.Instance;
import kodkod.instance.Tuple;


class MVariableVectorAssertion
{
	boolean positive;
	Expression sortExpression;
	
	MVariableVectorAssertion(boolean positive, Expression rel)
	{
		this.positive = positive;
		this.sortExpression = rel;
	}
	
	public String toString()
	{
		return "("+positive + ") "+sortExpression;
	}
	
	public boolean equals(Object oth)
	{
		if(! (oth instanceof MVariableVectorAssertion))
			return false;
		MVariableVectorAssertion o = (MVariableVectorAssertion) oth;
		if(o.positive != positive)
			return false;
		if(! sortExpression.equals(o.sortExpression))
			return false;
		
		return true;
	}
	
	public int hashCode()
	{		
		if(positive)
			return sortExpression.hashCode();
		return sortExpression.hashCode() * -1;
	}
}


class MExploreCondition
{
	Formula fmla;
	Set<MIDBCollection> seenIDBs = new HashSet<MIDBCollection>();
	
	/* So we can detect bad EDB names
	when combining vocabs at very end, 
	look at this list and double-check.
	*/
	Set<Relation> madeEDBs = new HashSet<Relation>();
	
	// We attempt to infer the sort of variables based on information in the query itself.
	// * AtomicNecessary and Necessary assertions are true in all models of the query.
	// * Sufficient assertions are those whose opposite must be true in the query's negation.
	//     (This means that there must be some model of the query where the assertion holds:
	//      (\neg \sigma) \implies (\neg a)  is equivalent to
	//      a \implies \sigma
	//      So if a is satisfiable, there is a model of \sigma containing a.
	//      Each assertion is satisfiable in isolation as an atom. So QED.)
	
	// Examples:
	// (p or q) and (p or r) ===> p or (q and r) ===> p is SUFFICIENT.
	// (p or q) and (p or r) and (r or s) ===> nothing is necessarily SUFFICIENT.
	// (p and q) or (p and r) ===> p and (q or r) ===> p is NECESSARY.
	// (p and q) or (p and r) or (r and s) ===> nothing is necessarily necessary.
	
	// I am sorry for the terminological abomination in the last paragraph, but
	// it seems fairly clear from context. And it might possibly have been
	// necessary. Hopefully it is sufficient. -TN
	
	
	HashMap<List<Variable>, Set<MVariableVectorAssertion>> assertAtomicNecessary =
		new HashMap<List<Variable>, Set<MVariableVectorAssertion>>();
	HashMap<List<Variable>, Set<MVariableVectorAssertion>> assertNecessary =
		new HashMap<List<Variable>, Set<MVariableVectorAssertion>>();
	HashMap<List<Variable>, Set<MVariableVectorAssertion>> assertSufficient =
		new HashMap<List<Variable>, Set<MVariableVectorAssertion>>();
	
	// These are assertions that must hold for the formula to be well-formed, and thus
	// are not subject to propagation. 
	HashMap<List<Variable>, Set<MVariableVectorAssertion>> inferredSorts = 
		new HashMap<List<Variable>, Set<MVariableVectorAssertion>>();
	
	// Terms seen
	Set<MTerm> terms = new HashSet<MTerm>();
	
	// **************************************************************
	// **************************************************************
	
	/* Often want to build a query condition formula without first knowing 
	 * the vocabulary the query operates under. This can cause a problem
	 * for de-sugaring certain special syntax. 
	 * 
	 * Kodkod will not allow us to subclass Formula, Expression, or ComparisonFormula.
	 * Therefore, we leave "placeholders" in the condition formula for late evaluation
	 * when the vocabulary is known.
	 */ 
	Set<Formula> eqPlaceholders = new HashSet<Formula>();
	//Set<Expression> exprPlaceholders = new HashSet<Expression>();
	
	// Remember what we decided were true placeholders, so MQuery won't
	// think they are real free variables.
	Set<Variable> knownPlaceholders = new HashSet<Variable>();
	
	/*void resolvePlaceholders(MVocab vocab)
	throws MUserException, MGEManagerException
	{
		// When the vocabulary is known, turn the placeholders into real Nodes.
		
		Map<Node, Node> replacementMap = new HashMap<Node, Node>();
		
		for(Formula eqf : eqPlaceholders)
		{
			MCommunicator.writeToLog("\nHandling placeholder: "+eqf);
			
			ComparisonFormula comparison = (ComparisonFormula) eqf;
			Expression lhs = comparison.left();
			Expression rhs = comparison.right();
			
			// Validate
			if(!(lhs instanceof Variable))
				throw new MUserException("Could not understand the LHS of equality: "+comparison);			
			if(!(rhs instanceof Variable))
				throw new MUserException("Could not understand the RHS of equality: "+comparison);

			Variable lhv = (Variable) lhs;
			Variable rhv = (Variable) rhs;			
			MSort lsort = vocab.fastGetSort(lhv.name());
			MSort rsort = vocab.fastGetSort(rhv.name());
			
			// Neither is a sort ~~ variable equality:  X = Y
			if(lsort == null && rsort == null)
				continue;
			
			// Don't allow sort = sort			
			if(lsort != null && rsort != null)
				throw new MUserException("Both sides of the equality "+comparison + " were sort symbols. Could not resolve the equality.");

			MSort thesort;
			Variable thevar;
			Variable theplaceholder;
			
			if(lsort != null)
			{
				thesort = lsort;
				theplaceholder = lhv;
				thevar = rhv;
			}
			else // only remaining option is rsort != null
			{
				thesort = rsort;
				theplaceholder = rhv;
				thevar = lhv;
			}
			
			// Require at most one elements in the sort
			if(!vocab.axioms.setsAtMostOne.contains(thesort.name) &&
			   !vocab.axioms.setsSingleton.contains(thesort.name))
				throw new MUserException("Sort "+thesort.name+" was not constrained to be atmostone or singleton; cannot treat it like a constant.");
						
			// If we got this far, we know that this equality needs to be re-written to A(x)
			Formula newFormula = MFormulaManager.makeAtom(thevar, thesort.rel);

			MCommunicator.writeToLog("\n   "+eqf+ " should become: "+newFormula);
			
			knownPlaceholders.add(theplaceholder);
			replacementMap.put(eqf, newFormula);			
			
		} // end for each eqPlaceholder
		
		
		// Now perform the replacement(s)
		
		ReplaceComparisonFormulasV theVisitor = new ReplaceComparisonFormulasV(replacementMap);		
		fmla = fmla.accept(theVisitor);		
	}	*/
	
	/*public static void resolveMapPlaceholders(MVocab vocab,
			Map<String, Set<List<String>>> themap) throws MUserException
	{		
		if(themap == null)
			return;
		if(!themap.containsKey("="))
			return;
		
		MCommunicator.writeToLog("\nHandling placeholders in map: "+themap);
		
			for(List<String> varindexing : themap.get("="))
			{
				// Only deal with |2| indexings
				if(varindexing.size() == 2)
				{
					String left = varindexing.get(0);
					String right = varindexing.get(1);
					MSort lsort = vocab.fastGetSort(left);
					MSort rsort = vocab.fastGetSort(right);
					
					// Neither is a sort ~~ variable equality:  X = Y
					if(lsort == null && rsort == null)
						continue;
					
					// Don't allow sort = sort			
					if(lsort != null && rsort != null)
						throw new MUserException("Both sides of the equality "+left+"="+right+" were sort symbols. Could not resolve the equality.");

					MSort thesort;
					String thevar;
					
					if(lsort != null)
					{
						thesort = lsort;
						thevar = right;
					}
					else // only remaining option is rsort != null
					{
						thesort = rsort;
						thevar = left;
					}
					
					// Require at most one elements in the sort
					if(!vocab.axioms.setsAtMostOne.contains(thesort.name) &&
					   !vocab.axioms.setsSingleton.contains(thesort.name))
						throw new MUserException("Sort "+thesort.name+" was not constrained to be atmostone or singleton; cannot treat it like a constant.");
								
					// If we got this far, we know that this equality needs to be re-written to A(x)
					if(!themap.containsKey(thesort.name))
						themap.put(thesort.name, new HashSet<List<String>>());
					List<String> singletonlist = new ArrayList<String>(1);
					
					singletonlist.add(thevar);
					themap.get(thesort.name).add(singletonlist);
					
					MCommunicator.writeToLog("\n   ="+left+","+right+" changed to: "+thesort+"("+thevar+")");					
				}
				
			}
			
			// Remove the special entry
			themap.remove("=");
		
	}*/
	
	// **************************************************************	
	
	/*
	public void inferFromInclude(MVocab uber,
			Map<String, Set<List<String>>> includeMap) throws MGEUnknownIdentifier
	{
		for(String key : includeMap.keySet())
		{
			// Seeing a sort gives you nothing for sure
			if(uber.fastIsSort(key))
				continue;
			
			MCommunicator.writeToLog("\ninferFromInclude: "+key+" was not a sort.");
			
			boolean bIsIDB = false;
			Relation predrel = null;
			MIDBCollection pol = null;
			if(uber.predicates.containsKey(key))
			{
				bIsIDB = false;
				predrel = uber.predicates.get(key).rel;
				MCommunicator.writeToLog("\ninferFromInclude: "+key+" was a predicate: "+predrel);
			}
			else
			{
				bIsIDB = true;
				
				String collname; 
				
				// view or policy IDB?
				int colonpos = key.indexOf(":"); 
				if(colonpos < 0)
				{
					// view
					collname = key;
					pol = MEnvironment.getPolicyOrView(collname);
					MCommunicator.writeToLog("\ninferFromInclude: "+key+" was a saved query with name = "+collname);

				}
				else
				{
					// policy idb
					collname = key.substring(0, colonpos);
					pol = MEnvironment.getPolicyOrView(collname);
					MCommunicator.writeToLog("\ninferFromInclude: "+key+" was an IDB in collection: "+pol+"; coll name = "+collname);					
				}
				
				if(pol == null)
				{
					// Invalid collection name
					throw new MGEUnknownIdentifier("Could not find a policy or saved query named: "+collname);
				}
			}
			
			
			for(List<String> varnamevector : includeMap.get(key))
			{
				List<Variable> varvector = vectorize(varnamevector);
			
				// Initialize assertion pools for this vector IF NEEDED
				// (may have seen the vector already, after all)
				initAssertionsForVectorIfNeeded(varvector);
				
				if(!bIsIDB)
					inferredSorts.get(varvector).add(new MVariableVectorAssertion(true, predrel));
				else
				{
					// This constructor means a set of atomic necessary assertions:	
					int iIndex = 0;
					for(Variable v : varvector)
					{			
						List<Variable> thisVar = new ArrayList<Variable>();
						thisVar.add(v);	// lists with equal elements are equal.	
						
						initAssertionsForVectorIfNeeded(thisVar);
						
						Variable oldVar = pol.varOrdering.get(iIndex);
						iIndex ++;
						
						//MEnvironment.errorStream.println(oldVar);
						//MEnvironment.errorStream.println(pol.varSorts);
						//MEnvironment.errorStream.println(assertAtomicNecessary.get(thisVar));
						
						inferredSorts.get(thisVar).add(new MVariableVectorAssertion(true, pol.varSorts.get(oldVar)));	
					}
				}
			}
		}
	}
	*/

	
	void initAssertionsForVectorIfNeeded(List<Variable> thisVar)
	{
		if(!assertAtomicNecessary.containsKey(thisVar))
			assertAtomicNecessary.put(thisVar, new HashSet<MVariableVectorAssertion>());
		if(!assertNecessary.containsKey(thisVar))
			assertNecessary.put(thisVar, new HashSet<MVariableVectorAssertion>());
		if(!assertSufficient.containsKey(thisVar))
			assertSufficient.put(thisVar, new HashSet<MVariableVectorAssertion>());
		if(!inferredSorts.containsKey(thisVar))
			inferredSorts.put(thisVar, new HashSet<MVariableVectorAssertion>());
	}
	
	MExploreCondition(boolean b)
	{
		if(b)
			fmla = Formula.TRUE;
		else
			fmla = Formula.FALSE;
		
		// No assertions. No free variables. Just a constant formula
	}
	
	// Equality
	MExploreCondition(Formula f, MTerm t1, MTerm t2, boolean isPlaceholder)
	{
		fmla = f;				
		// No assertions, but make an entry so we know there's a free variable.
		
		//List<Variable> thisVarX = new ArrayList<Variable>();
		//thisVarX.add(x);	// lists with equal elements are equal.
		//List<Variable> thisVarY = new ArrayList<Variable>();
		//thisVarY.add(y);	// lists with equal elements are equal.
		
		//initAssertionsForVectorIfNeeded(thisVarX);
		//initAssertionsForVectorIfNeeded(thisVarY);
		
		//if(isPlaceholder)
		//	eqPlaceholders.add(fmla);
		
		terms.add(t1);
		terms.add(t2);
	}
	
	// Atomic EDB formulas: made(vec)
	/*MExploreCondition(Formula f, Relation made, List<String> varnamevector)
	{
		fmla = f;
		madeEDBs.add(made);
		
		List<Variable> varvector = vectorize(varnamevector);
	
		
		// Initialize assertion pools for this vector IF NEEDED
		// (may have seen the vector already, after all)
		initAssertionsForVectorIfNeeded(varvector);
		
		// This constructor means an atomic necessary assertion:
		assertAtomicNecessary.get(varvector).add(new MVariableVectorAssertion(true, made));				
	}*/
	
	MExploreCondition(Formula f, Relation made, List<MTerm> vec)
	{
		fmla = f;
		madeEDBs.add(made);
		
		for(MTerm t : vec)
			terms.add(t);
	}
	MExploreCondition(Formula f, MIDBCollection pol, String idbname, List<MTerm> vec)
	{
		fmla = f;
		seenIDBs.add(pol);
		
		for(MTerm t : vec)
			terms.add(t);
	}
	MExploreCondition(Formula f, Relation madeSort, Variable v)
	{
		fmla = f;
		madeEDBs.add(madeSort);
	}
	
	// Atomic IDB Formulas pol.idbname(vec)
	/*MExploreCondition(Formula f, MIDBCollection pol, String idbname, List<String> varnamevector)
	{
		fmla = f;
		seenIDBs.add(pol);
		
		List<Variable> varvector = vectorize(varnamevector);		
		
		// Initialize assertion pools for this vector
						
		// This constructor means a set of atomic necessary assertions:	
		int iIndex = 0;
		for(Variable v : varvector)
		{			
			List<Variable> thisVar = new ArrayList<Variable>();
			thisVar.add(v);	// lists with equal elements are equal.	
			
			initAssertionsForVectorIfNeeded(thisVar);
			
			Variable oldVar = pol.varOrderings.get(idbname).get(iIndex);
			iIndex ++;
			
			//MEnvironment.errorStream.println(oldVar);
			//MEnvironment.errorStream.println(pol.varSorts);
			//MEnvironment.errorStream.println(assertAtomicNecessary.get(thisVar));
			
			assertAtomicNecessary.get(thisVar).add(new MVariableVectorAssertion(true, pol.varSorts.get(oldVar)));	
		}
	}*/
	
	List<Variable> vectorize(List<String> namevector)
	{
		List<Variable> result = new ArrayList<Variable>(namevector.size());
		
		for(String s : namevector)
			result.add(MFormulaManager.makeVariable(s));
		
		return result;
	}
	
	MExploreCondition not()	
	{
		fmla = MFormulaManager.makeNegation(fmla);				
		
		doAssertNot();
		
		return this;
	}
	
	private void doAssertNot()
	{
		Set<List<Variable>> toDo = new HashSet<List<Variable>>(assertNecessary.keySet());
		for(List<Variable> varvector : toDo)
		{
			// De Morgan: negate inside
			negateAll(assertNecessary.get(varvector));
			negateAll(assertSufficient.get(varvector));			

			// De Morgan: and <-> or
			Set<MVariableVectorAssertion> forSwap = assertNecessary.get(varvector);
			assertNecessary.put(varvector, assertSufficient.get(varvector));
			assertSufficient.put(varvector, forSwap);
			
			// No and/or involved, just negate
			negateAll(assertAtomicNecessary.get(varvector));						
		}
	}
	
	private void negateAll(Set<MVariableVectorAssertion> asserts)
	{
		for(MVariableVectorAssertion ast : asserts)
			ast.positive = ! ast.positive;		
	}
	
	MExploreCondition and(MExploreCondition oth)	
	{		
		fmla = MFormulaManager.makeAnd(fmla, oth.fmla);						
		seenIDBs.addAll(oth.seenIDBs);
		madeEDBs.addAll(oth.madeEDBs);
		eqPlaceholders.addAll(oth.eqPlaceholders);
		
		doAssertAnd(oth);
				
		return this;
	}

	MExploreCondition exists(Variable theVar, Relation theSort)
	{
		Decl theDecls = MFormulaManager.makeOneOfDecl(theVar, theSort);		
		fmla = MFormulaManager.makeExists(this.fmla, theDecls);
		
		// Catch unknown sort
		madeEDBs.add(theSort);
		
		return this;
	}

	MExploreCondition forall(Variable theVar, Relation theSort)
	{
		Decl theDecls = MFormulaManager.makeOneOfDecl(theVar, theSort);		
		fmla = MFormulaManager.makeForAll(this.fmla, theDecls);
		
		// Catch unknown sort
		madeEDBs.add(theSort);
		
		return this;
	}

	
	MExploreCondition or(MExploreCondition oth)	
	{		
		fmla = MFormulaManager.makeOr(fmla, oth.fmla);			
		seenIDBs.addAll(oth.seenIDBs);
		madeEDBs.addAll(oth.madeEDBs);
		eqPlaceholders.addAll(oth.eqPlaceholders);
		
		doAssertOr(oth);
		
		return this;
	}

	void synchKeys(MExploreCondition oth)
	{
		// What if oth mentions variables that we know nothing about?
		
		for(List<Variable> thisVar : oth.assertNecessary.keySet())
		{
			initAssertionsForVectorIfNeeded(thisVar);
		}
	}
	
	private void doAssertOr(MExploreCondition oth)
	{
		synchKeys(oth);
		Set<List<Variable>> toDo = new HashSet<List<Variable>>(assertNecessary.keySet());
		
		for(List<Variable> varvector : toDo)
		{
			if(oth.assertSufficient.containsKey(varvector))
				assertSufficient.get(varvector).addAll(oth.assertSufficient.get(varvector));
			if(assertAtomicNecessary.containsKey(varvector))
				assertSufficient.get(varvector).addAll(assertAtomicNecessary.get(varvector));
			if(oth.assertAtomicNecessary.containsKey(varvector))
				assertSufficient.get(varvector).addAll(oth.assertAtomicNecessary.get(varvector));

			if(oth.assertNecessary.containsKey(varvector))
				assertNecessary.get(varvector).retainAll(oth.assertNecessary.get(varvector));
			
			// always preserved
			if(oth.inferredSorts.containsKey(varvector))
				inferredSorts.get(varvector).addAll(oth.inferredSorts.get(varvector));
			
			assertAtomicNecessary.get(varvector).clear();
		}
	}
	
	private void doAssertAnd(MExploreCondition oth)
	{
		synchKeys(oth);
		Set<List<Variable>> toDo = new HashSet<List<Variable>>(assertNecessary.keySet());		
						
		for(List<Variable> varvector : toDo)
		{
			if(oth.assertNecessary.containsKey(varvector))
				assertNecessary.get(varvector).addAll(oth.assertNecessary.get(varvector));
			if(assertAtomicNecessary.containsKey(varvector))
				assertNecessary.get(varvector).addAll(assertAtomicNecessary.get(varvector));
			if(oth.assertAtomicNecessary.containsKey(varvector))
				assertNecessary.get(varvector).addAll(oth.assertAtomicNecessary.get(varvector));

			if(oth.assertSufficient.containsKey(varvector))
				assertSufficient.get(varvector).retainAll(oth.assertSufficient.get(varvector));
			
			// always preserved
			if(oth.inferredSorts.containsKey(varvector))
				inferredSorts.get(varvector).addAll(oth.inferredSorts.get(varvector));

			
			assertAtomicNecessary.get(varvector).clear();
		}		
	}
	
	MExploreCondition implies(MExploreCondition oth)	
	{
		fmla = MFormulaManager.makeImplication(fmla, oth.fmla);
		seenIDBs.addAll(oth.seenIDBs);
		madeEDBs.addAll(oth.madeEDBs);
		eqPlaceholders.addAll(oth.eqPlaceholders);
		
		// a -> b
		// is equivalent to
		// !a or b
		doAssertNot();
		doAssertOr(oth);
		
		return this;
	}

	MExploreCondition iff(MExploreCondition oth)	
	{
		fmla = MFormulaManager.makeIFF(fmla, oth.fmla);
		seenIDBs.addAll(oth.seenIDBs);	
		madeEDBs.addAll(oth.madeEDBs);	
		eqPlaceholders.addAll(oth.eqPlaceholders);
		
		// No assertions survive.
		clearAssertions();
						
		return this;
	}

	private void clearAssertions()
	{
		Set<List<Variable>> toDo = new HashSet<List<Variable>>(assertNecessary.keySet());
		for(List<Variable> varvector : toDo)
		{
			assertNecessary.get(varvector).clear();
			assertSufficient.get(varvector).clear();			
			assertAtomicNecessary.get(varvector).clear();
		}
	}
		
	MExploreCondition addSeenIDBCollections(List<MIDBCollection> moreIDBCollections)
	{
		seenIDBs.addAll(moreIDBCollections);		
		return this;
	}	
}




/**
 * Represents a command-line and environment for Margrave
 * @author tn
 *
 */
public class MEnvironment 
{
	private static Map<String, MIDBCollection> envIDBCollections = new HashMap<String, MIDBCollection>();
	private static Map<String, MVocab> envVocabularies = new HashMap<String, MVocab>();
	
	private static Map<String, MQueryResult> envQueryResults = new HashMap<String, MQueryResult>();
	private static Map<String, MInstanceIterator> envIterators = new HashMap<String, MInstanceIterator>();
			
	// both streams will be packaged with the response XML
	
	// System.err should never be written to, because Racket
	// does not read from it, and it could cause a freeze.

	// System.out should never be written to except by
	// MCommunicator, since Racket expects well-formed XML.
	
	protected static StringWriter errorBuffer = new StringWriter();
	protected static PrintWriter errorWriter = new PrintWriter(errorBuffer);
	
	protected static StringWriter outBuffer = new StringWriter();
	protected static PrintWriter outWriter = new PrintWriter(outBuffer);
	
	public static String eol = System.getProperty("line.separator");
	public static String sNoIterator = "no iterator";
	public static String sFalse = String.valueOf(false);
	public static String sTrue = String.valueOf(true);
	public static String sUnknown = "unknown";
	public static String sNotExpected = "not expected type";
	public static String sVocabulary = "vocabulary";
	public static String sPolicy = "policy";
	public static String sPolicySet = "policyset";
	public static String sPolicyLeaf = "policyleaf";
	public static String sIdentifier = "identifier";
	public static String sIDBCollection = "idb collection";
	public static String sQuery = "query";
	public static String sResultID = "result ID";
	public static String sUsed = "used";
	public static String sReserved = "reserved";
	public static String sReqVector = "request vector";	
	public static String sNotDocument = "value not XML document";
	public static String sFailure = "failure";
	public static String sCommand = "command";
	public static String sConstraint = "constraint";
	
	public static String sShow = "show";
	public static String sNext = "next";
	public static String sGet = "get";
	public static String sFirst = "first";	
	
	public static String sTopQualifiedName = "MARGRAVE-RESPONSE";
	public static String sSuccess = "success";
	public static String sUnsat = "unsat";
	public static String sDebug = "debug";
	public static String sQuitMargrave = "quit";

	public static String tempVarPrefix = "_";
	
	// Used in exception output
	static String lastCommandReceived = "";

	// Functions to send immediately: don't run out of space. 
	// Writing too much without flushing seems to interfere 
	// with the XML protocol.
	// apr 2011: may no longer be needed
	static public void writeOutLine(Object str)
	{
		outWriter.println(str);
		outWriter.flush();
	}
	static public void writeOut(Object str)
	{
		outWriter.print(str);
		outWriter.flush();
	}
	
	static public void writeErrLine(Object obj)
	{					
		errorWriter.println(obj);
		errorWriter.flush();
	}
	static public void writeErr(Object obj)
	{
		errorWriter.print(obj);
		errorWriter.flush();
	}
	
	
	
	static MIDBCollection getPolicyOrView(String str)
	{		
		// Is str the name of an idb I know?
		if(envIDBCollections.containsKey(str))
			return envIDBCollections.get(str);
		
		return null;
	}
	
	static MVocab getVocab(String str)
	{
		if(envVocabularies.containsKey(str))
			return envVocabularies.get(str);		
		return null;
	}

	static MQueryResult getQueryResult(String id) throws MUserException
	{		
		if(envQueryResults.containsKey(id))
			return envQueryResults.get(id);		
		return null;
	}
		
	//If num is !- -1, returns num.
	//If num is == -1, return the last results id (lastresult)
/*	static int convertQueryNumber(int num) throws MUserException
	{
		if (num == -1) 
		{
			if(lastResult < 0) // do we HAVE a last result?
			{
				throw new MUserException("No prior EXPLORE statement to reference. Perhaps the last EXPLORE statement returned an error?");
			}
			return lastResult;
		}
		else {
			return num;
		}
	}
*/
	
	static Formula getIDB(String collname, String idbname)
	{
		MIDBCollection collection = getPolicyOrView(collname);
		if(collection == null)
			return null;
		
		if(collection.containsIDB(idbname))
			return collection.getIDB(idbname);
		return null;
	}
	
	static Formula getOnlyIDB(String collname)
	{
		// Used for view IDB collections, which must have only one IDB.
		MIDBCollection collection = getPolicyOrView(collname);
		if(collection == null)
			return null;
		
		if(collection.size() != 1)
			return null;
		
		for(String name : collection.idbKeys())
			return collection.getIDB(name);
		return null;
	}
	
	/*static public Document commandSilent(String cmd)
	{
		// Don't print anything
		return command(cmd, true);
	}
	
	static public Document command(String cmd)
	{
		// Default to non-silent
		return command(cmd, false);
	}*/
	
	static void printOut(Object out, boolean silent)
	{
		if(!silent)
			outWriter.println(out.toString());
	}
	
	static Document getNextModel(String id)
	throws MBaseException
	{		
		// Do we have a result for this num?
		if(!envQueryResults.containsKey(id))
			return errorResponse(sUnknown, sResultID, id);
		
		if(!envIterators.containsKey(id))
			return getFirstModel(id);
		else
		{
			// Return next model in the iterator.
			MQueryResult result = getQueryResult(id);
			try
			{
				return scenarioResponse(result, envIterators.get(id).next(), id);
			}
			catch(MGENoMoreSolutions e)
			{
				return noSolutionResponse(result, id);
			}
		}		
	}
	
	public static Document resetIterator(String id) 
	{
		// Do we have a result for this qry?
		if(!envQueryResults.containsKey(id))
			return errorResponse(sUnknown, sResultID, id);
		
		MQueryResult result = getQueryResult(id);
		
		// Reset the iterator
		MInstanceIterator it = result.getTotalIterator();
		envIterators.put(id, it);
		
		return successResponse();		
	}
	
	static Document getFirstModel(String id) 
	throws MBaseException
	{
		// Do we have a result for this qry?
		if(!envQueryResults.containsKey(id))
			return errorResponse(sUnknown, sResultID, id);

		MQueryResult result = getQueryResult(id);
		
		// Reset the iterator (or create it if new)
		MInstanceIterator it = result.getTotalIterator();
		envIterators.put(id, it);
		
		try
		{
			return scenarioResponse(result, it.next(), id);
		}
		catch(MGENoMoreSolutions e)
		{
			return noSolutionResponse(result, id);
		}
		
	}
	
	static public Document returnQueryResponse(MQuery qry, String XMLCommand)
	{
		try
		{
				// May return null if an internal error.
				if(qry == null)
				{
					//errorStream.println("-> Command failed.");
					return errorResponse(sFailure, sQuery, XMLCommand);
				}
				
				// Compile the query and get the result.
				MQueryResult qryResult = qry.runQuery();
				
				envQueryResults.put(qry.name, qryResult);
				return resultHandleResponse(0);
												
			}  // end: a query
		catch(Exception e)
		{
			// Unexpected! Bad exception			
			return exceptionResponse(e);
		}
	}
	
	
	static public Document printInfo(String id)
	{
		MIDBCollection coll = getPolicyOrView(id);
		if(coll != null)
			return getCollectionInfo(coll);
		else
		{
			MVocab voc = envVocabularies.get(id);
			if(voc != null)
			{
				return getVocabInfo(voc);
			}
		}
				
		return errorResponse(sUnknown, sIdentifier, id);
	}
	
	
	private static Document getCollectionInfo(MIDBCollection coll)
	{	
		// TODO This should be broken out into methods of each subclass.
			
		Document xmldoc = makeInitialResponse("collection-info");
		if(xmldoc == null) return null; // be safe (but bottle up exceptions)		
				
		if(coll instanceof MPolicyLeaf)
		{
			MPolicyLeaf pleaf = (MPolicyLeaf) coll;
			Element theElement = xmldoc.createElementNS(null, "POLICY-LEAF");
			theElement.setAttribute("name", pleaf.name);
			theElement.setAttribute("rule-combine", pleaf.printCombinators(pleaf.rCombineFA, pleaf.rCombineWhatOverrides));
			
			Element idbsElement = xmldoc.createElementNS(null, "IDBS");
			for(String key : coll.idbKeys())
			{
				Element idbElement = xmldoc.createElementNS(null, "IDB");
				idbElement.setAttribute("base-name", key);
				idbElement.appendChild(xmldoc.createTextNode(coll.name+":"+key));
				idbsElement.appendChild(idbElement);
			}
			theElement.appendChild(idbsElement);
			
			Element varsElement = xmldoc.createElementNS(null, "FREE-VARIABLES");
			for(String idbname : coll.varOrderings.keySet())
			{
				Element idbElement = xmldoc.createElementNS(null, "IDB");
				for(Variable v : coll.varOrderings.get(idbname))
				{
					Element varElement = xmldoc.createElementNS(null, "VARIABLE");
					varElement.appendChild(xmldoc.createTextNode(v.name()));
					varsElement.appendChild(varElement);
					idbElement.appendChild(varElement);
				}
				theElement.appendChild(idbElement);
			}
			
						
			xmldoc.getDocumentElement().appendChild(theElement);
		}
		else if(coll instanceof MPolicySet)
		{
			MPolicySet pset = (MPolicySet) coll;
			Element theElement = xmldoc.createElementNS(null, "POLICY-SET");
			theElement.setAttribute("name", pset.name);
			theElement.setAttribute("policy-combine", pset.printCombinators(pset.pCombineFA, pset.pCombineWhatOverrides));					
			
			Element idbsElement = xmldoc.createElementNS(null, "IDBS");
			for(String key : coll.idbKeys())
			{
				Element idbElement = xmldoc.createElementNS(null, "IDB");
				idbElement.setAttribute("base-name", key);
				idbElement.appendChild(xmldoc.createTextNode(coll.name+":"+key));
				
				Element varsElement = xmldoc.createElementNS(null, "FREE-VARIABLES");
				for(Variable v : coll.varOrderings.get(key))
				{
					Element varElement = xmldoc.createElementNS(null, "VARIABLE");
					varElement.appendChild(xmldoc.createTextNode(v.name()));
					varsElement.appendChild(varElement);
				}
				idbElement.appendChild(varsElement);
				
				idbsElement.appendChild(idbElement);
			}
			theElement.appendChild(idbsElement);
			
			Element childrenElement = xmldoc.createElementNS(null, "CHILDREN");
			for(MPolicy child : ((MPolicySet) coll).children)
			{
				Element cElement = xmldoc.createElementNS(null, "POLICY-IDENTIFIER");
				cElement.setAttribute("name", child.name);								
				childrenElement.appendChild(cElement);
			}
			theElement.appendChild(childrenElement);					
			
			xmldoc.getDocumentElement().appendChild(theElement);
		}
		else
		{
			MQuery qry = (MQuery) coll;
			Element theElement = xmldoc.createElementNS(null, "SAVED-QUERY");
			theElement.setAttribute("name", qry.name);

			Element idbsElement = xmldoc.createElementNS(null, "IDBS");
			for(String key : coll.idbKeys())
			{
				Element idbElement = xmldoc.createElementNS(null, "IDB");
				idbElement.setAttribute("base-name", key);
				idbElement.appendChild(xmldoc.createTextNode(coll.name+":"+key));
				
				Element varsElement = xmldoc.createElementNS(null, "FREE-VARIABLES");
				for(Variable v : coll.varOrderings.get(key))
				{
					Element varElement = xmldoc.createElementNS(null, "VARIABLE");
					varElement.appendChild(xmldoc.createTextNode(v.name()));
					varsElement.appendChild(varElement);
				}
				idbElement.appendChild(varsElement);
				
				idbsElement.appendChild(idbElement);
			}
			theElement.appendChild(idbsElement);
			
			
			xmldoc.getDocumentElement().appendChild(theElement);
		}
					
		
		return xmldoc;
	}
	
	static void writeToLog(String s) {
   	 try{
   		    // Create file 
   		    FileWriter fstream = new FileWriter("log.txt", true);
   		        BufferedWriter out = new BufferedWriter(fstream);
   		    out.write(s);
   		    //Close the output stream
   		    out.close();
   		    }catch (Exception e)
   		    {
   		      //Catch exception if any
   		    	MEnvironment.errorWriter.println("Error: " + e.getMessage());
   		    }
    }

	private static Document getVocabInfo(MVocab voc) 
	{
		Document xmldoc = makeInitialResponse("vocabulary-info");
		if(xmldoc == null) return null; // be safe (but bottle up exceptions)		
		Element theElement = xmldoc.createElementNS(null, "VOCABULARY");
		theElement.setAttribute("name", "");
		
		Element sortsElement = xmldoc.createElementNS(null, "SORTS");
		Element axiomsElement = xmldoc.createElementNS(null, "AXIOMS");
		
		// List each sort with its subsorts
		// Better to do it this way than tree-like, just in case 2 sorts have a common subsort.
		for(MSort s : voc.sorts.values())
		{
				Element sortElement = xmldoc.createElementNS(null, "SORT");
				sortElement.setAttribute("name", s.name);				
				sortsElement.appendChild(sortElement);
				
				for(MSort child : s.subsorts)
				{
					Element childElement = xmldoc.createElementNS(null, "SORT");
					childElement.setAttribute("name", child.name);
					sortElement.appendChild(childElement);
				}
		}
		
		// AXIOMS
		// TODO more axioms
		
		
		
		
		theElement.appendChild(sortsElement);
		theElement.appendChild(axiomsElement);
		xmldoc.getDocumentElement().appendChild(theElement);		
		return xmldoc;
	}

	// Returns true if overwriting existing identifier
	// Assume that the policy is already loaded FOR NOW. 
	// Later we will add a LOAD POLICY keyword.
	static public Document savePolicyAs(String ident, MPolicy pol)
	{		
		if("last".equals(ident))
		{						
			errorResponse(sReserved, sIDBCollection, ident);
		}
				
		return boolResponse(envIDBCollections.put(ident, pol) != null);			
	}
		
	static protected void setLast(MQuery qry)
	{
		envIDBCollections.put("last", qry);
	}
	
	static protected void setQueryForID(String id, MQuery qry)
	{
		envIDBCollections.put(id, qry);
	}
	
	static protected boolean isQueryIDUsed(String id)
	{
		return envIDBCollections.containsKey(id);
	}
	
	public static String foldConcatWithSpaces(List<String> idl)
	{
		StringBuffer result = new StringBuffer();
		
		boolean first = true;
		for(String s : idl)
		{
			if(!first)				
				result.append(" ");
			result.append(s);
			first = false;
		}
		
		return result.toString();		
	}
	
	
	protected static MInstanceIterator getIteratorFor(Integer id)
	{
		return envIterators.get(id);
	}
	
	public static Document showPopulated(String id,
			Map<String, Set<List<String>>> rlist,
			Map<String, Set<List<String>>> clist) throws MBaseException
	{
		MQueryResult aResult = getQueryResult(id);
		if(aResult == null)
			return errorResponse(sUnknown, sResultID, id);
				
		// XXX no more placeholders. to be removed. 
		// If tupled, will have indexing. Translate (using the original vocab)
		//if(aResult.forQuery.tupled)
		//{
		//	MExploreCondition.resolveMapPlaceholders(aResult.forQuery.internalTupledQuery.vocab, rlist);
		//	MExploreCondition.resolveMapPlaceholders(aResult.forQuery.internalTupledQuery.vocab, clist);
		//}
		
		Map<String, Set<String>> outsets;
		try
		{
			outsets = aResult.getPopulatedRelationFinder().getRealizedFormulas(rlist, clist);
			
			MCommunicator.writeToLog(rlist.toString());
			MCommunicator.writeToLog(clist.toString());
			MCommunicator.writeToLog(outsets.toString());
			
			if(outsets.size() == 1 && outsets.containsKey(""))				
				return setResponse(outsets.get(""));
			return mapResponse(outsets);
			
		}
		catch (MBaseException e)
		{
			return exceptionResponse(e);
		}
	}

	public static Document showNextCollapse(Integer id) 
	{
		// TODO No support for partial models yet
		return unsupportedResponse();
	}

	public static Document showPopulated(String id, Map<String, Set<List<String>>> rlist) throws MBaseException
	{
		return showPopulated(id, rlist,  new HashMap<String, Set<List<String>>>());
	}
	
	public static Document showUnpopulated(String id,
			Map<String, Set<List<String>>> rlist,
			Map<String, Set<List<String>>> clist) throws MUserException
	{
		MQueryResult aResult = getQueryResult(id);
		if(aResult == null)
			return errorResponse(sUnknown, sResultID, id);
				
		// XXX no more placeholders. to be removed. 
		// If tupled, will have indexing. Translate (using the original vocab)
		//if(aResult.forQuery.tupled)
		//{
		//	MExploreCondition.resolveMapPlaceholders(aResult.forQuery.internalTupledQuery.vocab, rlist);
		//	MExploreCondition.resolveMapPlaceholders(aResult.forQuery.internalTupledQuery.vocab, clist);
		//}
		
		Map<String, Set<String>> outsets;
		try
		{
			outsets = aResult.getPopulatedRelationFinder().getUnpopulatedRelations(rlist, clist);
			
			if(outsets.size() == 1 && outsets.containsKey(""))				
				return setResponse(outsets.get(""));
			return mapResponse(outsets);
			
		}
		catch (MBaseException e)
		{
			return exceptionResponse(e);
		}
	}
	
	public static Document showUnpopulated(String id, Map<String, Set<List<String>>> rlist) throws MUserException 
	{
		return showUnpopulated(id, rlist,  new HashMap<String, Set<List<String>>>());
	}
	
	public static Document countModels(String id, Integer n) throws MUserException
	{
		MQueryResult aResult = getQueryResult(id);
		if(aResult == null)
			return errorResponse(sUnknown, sResultID, id);
		return intResponse(aResult.countModelsAtSize(n));				
	}

	public static Document countModels(String id) throws MUserException
	{
		return countModels(id, -1); // -1 for overall total to ceiling
	}

	public static Document isGuar(String id) throws MUserException
	{
		// Is this solution complete? (Is the ceiling high enough?)
		MQueryResult aResult = getQueryResult(id);
		if(aResult == null)
			return errorResponse(sUnknown, sResultID, id);
		if(aResult.get_hu_ceiling() > aResult.get_universe_max())
			return boolResponse(false);
		return boolResponse(true);
	}

	public static Document isPoss(String id) throws MUserException 
	{	
		MQueryResult aResult = getQueryResult(id);
		if(aResult == null)
			return errorResponse(sUnknown, sResultID, id);
		try
		{
			return boolResponseWithStats(aResult, id, aResult.isSatisfiable());
		}
		catch(MBaseException e)
		{
			return exceptionResponse(e);
		}
	}

	public static Document showCeiling(String id) throws MUserException
	{
		MQueryResult aResult = getQueryResult(id);
		if(aResult == null)
			return errorResponse(sUnknown, sResultID, id);
		return intResponse(aResult.get_universe_max());
	}

	
	public static Document createPolicySet(String pname, String vname) 
	{
		// Create a new Policy Set with name pname, vocab vname...
		if(!envVocabularies.containsKey(vname))
			return errorResponse(sUnknown, sVocabulary, vname);
		if(envIDBCollections.containsKey(pname))
			return errorResponse(sUsed, sPolicy, pname);
		
		MVocab voc = envVocabularies.get(vname);
		MPolicySet pol = new MPolicySet(pname, voc);
		envIDBCollections.put(pname, pol);
		return successResponse();
	}
	
	
	public static Document createPolicyLeaf(String pname, String vname)
	{
		if(!envVocabularies.containsKey(vname))
			return errorResponse(sUnknown, sVocabulary, vname);
		if(envIDBCollections.containsKey(pname))
			return errorResponse(sUsed, sPolicy, pname);
		
		MVocab voc = envVocabularies.get(vname);
		MPolicyLeaf pol = new MPolicyLeaf(pname, voc);
		envIDBCollections.put(pname, pol);
		return successResponse();
	}

	public static Document createVocabulary(String vname)
	{
		if(envVocabularies.containsKey(vname))
			return errorResponse(sUsed, sVocabulary, vname);
		
		MVocab voc = new MVocab();
		envVocabularies.put(vname, voc);
		return successResponse();
	}
	
	public static Document deleteVocabulary(String vname)
	{
		if(!envVocabularies.containsKey(vname))
			return errorResponse(sUnknown, sVocabulary, vname);
		
		envVocabularies.remove(vname);
		return successResponse();
	}

	public static Document preparePolicy(String pname)
	{
		if(!envIDBCollections.containsKey(pname))
			return errorResponse(sUnknown, sPolicy, pname);
		MIDBCollection pol = envIDBCollections.get(pname);
		if(pol instanceof MPolicy)
		{
			try
			{
				((MPolicy)pol).initIDBs();
				return successResponse();
			}
			catch(MBaseException e)
			{
				return exceptionResponse(e);
			}
		}
		else
			return errorResponse(sNotExpected, sPolicy, pname);
	}

	public static Document loadXACML(String fname, String sfname) throws MUserException
	{
		MPolicy pol = MPolicy.readXACML(fname, sfname);
		
		if(envIDBCollections.containsKey(pol.name))
			return errorResponse(sUsed, sPolicy, pol.name);
		
		envIDBCollections.put(pol.name, pol);			
		return stringResponse(pol.name);
	}

	public static Document loadSQS(String fname)
	throws MUserException
	{
		MPolicy pol = MPolicy.loadSQS(fname);
		
		if(envIDBCollections.containsKey(pol.name))
			return errorResponse(sUsed, sPolicy, pol.name);
		
		envIDBCollections.put(pol.name, pol);			
		return stringResponse(pol.name);	
	}

	static MVocab makeNewVocabIfNeeded(String vname)
	{
		if(!envVocabularies.containsKey(vname))
		{
			MVocab voc = new MVocab();
			envVocabularies.put(vname, voc);
			return voc;
		}
		else
			return envVocabularies.get(vname);
	}
	
	public static Document addSubsort(String vname, String parent, String child)
	{
	
		MVocab voc = makeNewVocabIfNeeded(vname);
		try 
		{
			voc.addSubSort(parent, child);
			return successResponse();
		} 
		catch (MBaseException e)
		{
			return exceptionResponse(e);
		}
	}

	public static Document addSort(String vname, String sname)
	{

		MVocab voc = makeNewVocabIfNeeded(vname);
		try 
		{
			voc.addSort(sname);
			return successResponse();
		} 
		catch (MBaseException e)
		{
			return exceptionResponse(e);
		}
	}

	public static Document addSortWithSubs(String vname, String sname, List<String> subs)
	{

		MVocab voc = makeNewVocabIfNeeded(vname);
		try 
		{
			for(String childname : subs)
				voc.addSubSort(sname, childname);
			
			return successResponse();
		} 
		catch (MBaseException e)
		{
			return exceptionResponse(e);
		}
	}
	
	public static Document addConstraintAbstract(String vname, String s)
	{
		MVocab voc = makeNewVocabIfNeeded(vname);
		try 
		{
			voc.axioms.addConstraintAbstract(s);
			return successResponse();
		} 
		catch (MBaseException e)
		{
			return exceptionResponse(e);
		}
	}

	public static Document addConstraintAbstractAll(String vname, String s)
	{
		MVocab voc = makeNewVocabIfNeeded(vname);
		try 
		{
			voc.axioms.addConstraintAbstractAll(s);
			return successResponse();
		} 
		catch (MBaseException e)
		{
			return exceptionResponse(e);
		}
	}

	public static Document addConstraintTotalFunction(String vname, String s) 
	{

		MVocab voc = makeNewVocabIfNeeded(vname);
		try 
		{
			voc.axioms.addConstraintTotalFunction(s);
			return successResponse();
		} 
		catch (MBaseException e)
		{
			return exceptionResponse(e);
		}
	}

	public static Document addConstraintTotalRelation(String vname, String s) 
	{
		MVocab voc = makeNewVocabIfNeeded(vname);
		try 
		{
			voc.axioms.addConstraintTotalRelation(s);
			return successResponse();
		} 
		catch (MBaseException e)
		{
			return exceptionResponse(e);
		}
	}
	
	public static Document addConstraintPartialFunction(String vname, String s) 
	{

		MVocab voc = makeNewVocabIfNeeded(vname);
		try 
		{
			voc.axioms.addConstraintPartialFunction(s);
			return successResponse();
		} 
		catch (MBaseException e)
		{
			return exceptionResponse(e);
		}
	}

	public static Document addConstraintNonemptyAll(String vname, String s)
	{

		MVocab voc = makeNewVocabIfNeeded(vname);
		try 
		{
			voc.axioms.addConstraintNonemptyAll(s);
			return successResponse();
		} 
		catch (MBaseException e)
		{
			return exceptionResponse(e);
		}
	}

	public static Document addConstraintNonempty(String vname, String s)
	{

		MVocab voc = makeNewVocabIfNeeded(vname);
		try 
		{
			voc.axioms.addConstraintNonempty(s);
			return successResponse();
		} 
		catch (MBaseException e)
		{
			return exceptionResponse(e);
		}
	}

	public static Document addConstraintAtMostOneAll(String vname, String s)
	{

		MVocab voc = makeNewVocabIfNeeded(vname);
		try 
		{
			voc.axioms.addConstraintAtMostOneAll(s);
			return successResponse();
		} 
		catch (MBaseException e)
		{
			return exceptionResponse(e);
		}
	}

	public static Document addConstraintAtMostOne(String vname, String s) 
	{

		MVocab voc = makeNewVocabIfNeeded(vname);
		try 
		{
			voc.axioms.addConstraintAtMostOne(s);
			return successResponse();
		} 
		catch (MBaseException e)
		{
			return exceptionResponse(e);
		}
	}

	public static Document addConstraintSingletonAll(String vname, String s)
	{

		MVocab voc = makeNewVocabIfNeeded(vname);
		try 
		{
			voc.axioms.addConstraintSingletonAll(s);
			return successResponse();
		} 
		catch (MBaseException e)
		{
			return exceptionResponse(e);
		}
	}

	public static Document addConstraintSingleton(String vname, String s)
	{

		MVocab voc = makeNewVocabIfNeeded(vname);
		try 
		{
			voc.axioms.addConstraintSingleton(s);
			return successResponse();
		} 
		catch (MBaseException e)
		{
			return exceptionResponse(e);
		}
	}

	/*public static Document addDecision(String vname, String decname)
	{

		MVocab voc = makeNewVocabIfNeeded(vname);
		try 
		{
			voc.addDecision(decname);
			return successResponse();
		} 
		catch (MBaseException e)
		{
			return exceptionResponse(e);
		}
	}*/

	public static Document addConstant(String vname, String sname, String typename)
	{
		MVocab voc = makeNewVocabIfNeeded(vname);
		try 
		{
			MSort theSort = voc.getSort(typename);
			Relation rel = MFormulaManager.makeRelation(sname, 1);
			MConstant theConst = new MConstant(sname, rel, theSort);
			voc.constants.put(sname, theConst);
			return successResponse();
		} 
		catch (MBaseException e)
		{
			return exceptionResponse(e);
		}
	}
	
	public static Document addFunction(String vname, String sname, List<String> constr)
	{
		MVocab voc = makeNewVocabIfNeeded(vname);
		try 
		{			
			List<MSort> theAritySorts = new ArrayList<MSort>();
			for(String typename : constr)
				theAritySorts.add(voc.getSort(typename));
			MSort theResultSort = theAritySorts.get(theAritySorts.size()-1);
			theAritySorts.remove(theAritySorts.size()-1);
			
			Relation rel = MFormulaManager.makeRelation(sname, constr.size());
			MFunction theFunc = new MFunction(sname, rel, theAritySorts, theResultSort);
			voc.functions.put(sname, theFunc);
			return successResponse();
		} 
		catch (MBaseException e)
		{
			return exceptionResponse(e);
		}
	}
	
	public static Document addPredicate(String vname, String sname, List<String> constr)
	{

		MVocab voc = makeNewVocabIfNeeded(vname);
		try 
		{
			String constructstr = foldConcatWithSpaces(constr);
			voc.addPredicate(sname, constructstr);
			return successResponse();
		} 
		catch (MBaseException e)
		{
			return exceptionResponse(e);
		}
	}

	public static Document addRule(String pname, String rname, String decision, List<String> varOrdering, Formula target, Formula condition)
	{
		if(!envIDBCollections.containsKey(pname))
			return errorResponse(sUnknown, sPolicy, pname);			
		MIDBCollection coll = envIDBCollections.get(pname);
		if(!(coll instanceof MPolicyLeaf))
			return errorResponse(sNotExpected, sPolicyLeaf, pname);
		MPolicyLeaf pol = (MPolicyLeaf) coll;
		
		// No longer just a list of literal fmlas in a single conjunction.
		// Now we have an actual formula, potentially with quantifiers!
		// (But still need to separate target/condition for XACML.)
		
		try
		{
			pol.addRule(rname, decision, varOrdering, target, condition);
		}
		catch(MBaseException e)
		{
			return exceptionResponse(e);
		}
		return successResponse();
	}
	
	public static Document addVarDec(String pname, String varname, String typename)
	{
		if(!envIDBCollections.containsKey(pname))
			return errorResponse(sUnknown, sPolicy, pname);			
		MIDBCollection coll = envIDBCollections.get(pname);
		if(!(coll instanceof MPolicyLeaf))
			return errorResponse(sNotExpected, sPolicyLeaf, pname);
		MPolicyLeaf pol = (MPolicyLeaf) coll;
		
		try
		{
			pol.varSorts.put(MFormulaManager.makeVariable(varname), pol.vocab.getSort(typename).rel);
		}
		catch(MBaseException e)
		{
			return exceptionResponse(e);
		}
		return successResponse();
	}
	
	public static Document setPolicyTarget(String pname, List<String> cc)
	{
		if(!envIDBCollections.containsKey(pname))
			return errorResponse(sUnknown, sPolicy, pname);;			
		MIDBCollection coll = envIDBCollections.get(pname);
		if(!(coll instanceof MPolicy))
			return errorResponse(sNotExpected, sPolicy, pname);
		MPolicy pol = (MPolicy) coll;
		try
		{
			pol.setTarget(cc);
		}
		catch(MBaseException e)
		{
			return exceptionResponse(e);
		}
		return successResponse();
	}

	public static Document setRCombine(String pname, Set<String> facs, Map<String, Set<String>> overrides)
	{
		if(!envIDBCollections.containsKey(pname))
			return errorResponse(sUnknown, sPolicy, pname);;			
		MIDBCollection coll = envIDBCollections.get(pname);
		if(!(coll instanceof MPolicyLeaf))
			return errorResponse(sNotExpected, sPolicyLeaf, pname);
		MPolicyLeaf pol = (MPolicyLeaf) coll;
		pol.rCombineFA = new HashSet<String>(facs);
		pol.rCombineWhatOverrides = new HashMap<String, Set<String>>(overrides);
		
		return successResponse();
	}

	public static Document setPCombine(String pname, Set<String> facs, Map<String, Set<String>> overrides) 
	{
		if(!envIDBCollections.containsKey(pname))
			return errorResponse(sUnknown, sPolicy, pname);;			
		MIDBCollection coll = envIDBCollections.get(pname);
		if(!(coll instanceof MPolicySet))
			return errorResponse(sNotExpected, sPolicySet, pname);
		MPolicySet pol = (MPolicySet) coll;
		pol.pCombineFA = new HashSet<String>(facs);
		pol.pCombineWhatOverrides = new HashMap<String, Set<String>>(overrides);
		
		return successResponse();
	}

	public static Document getDecisionFor(String pname, String rname)
	{
		if(!envIDBCollections.containsKey(pname))
			return errorResponse(sUnknown, sPolicy, pname);;			
		MIDBCollection coll = envIDBCollections.get(pname);
		if(!(coll instanceof MPolicyLeaf))
			return errorResponse(sNotExpected, sPolicyLeaf, pname);
		MPolicyLeaf pol = (MPolicyLeaf) coll;
		
		return stringResponse(pol.getDecisionForRuleIDBName(rname));		
	}

	public static Document getHigherPriorityThan(String pname, String rname)
	{
		if(!envIDBCollections.containsKey(pname))
			return errorResponse(sUnknown, sPolicy, pname);;			
		MIDBCollection coll = envIDBCollections.get(pname);
		if(!(coll instanceof MPolicyLeaf))
			return errorResponse(sNotExpected, sPolicyLeaf, pname);
		MPolicyLeaf pol = (MPolicyLeaf) coll;
		
		List<String> res = pol.ruleIDBsWithHigherPriorityThan(rname);
		
		// order matters
		return listResponse(res);
	}

	public static Document getRulesIn(String pname, boolean b, String decname)
	{
		if(!envIDBCollections.containsKey(pname))
			return errorResponse(sUnknown, sPolicy, pname);;			
		MIDBCollection coll = envIDBCollections.get(pname);
		if(!(coll instanceof MPolicyLeaf))
			return errorResponse(sNotExpected, sPolicyLeaf, pname);
		MPolicyLeaf pol = (MPolicyLeaf) coll;
		
		List<String> res;
		res = pol.getRulesList(decname, b);
		
		// order matters
		return listResponse(res);
	}

	public static Document addChild(String parent, String child)
	{
		MIDBCollection pcoll = envIDBCollections.get(parent);
		MIDBCollection ccoll = envIDBCollections.get(child);
		if(pcoll == null) 
			return errorResponse(sUnknown, sPolicy, parent);
		if(ccoll == null)
			return errorResponse(sUnknown, sPolicy, child);
		
		if(!(pcoll instanceof MPolicySet)) 
			return errorResponse(sNotExpected, sPolicySet, parent);
		if(!(ccoll instanceof MPolicy))
			return errorResponse(sNotExpected, sPolicy, child);
		
		MPolicySet polparent = (MPolicySet) pcoll;
		MPolicy polchild = (MPolicy) ccoll;
		polparent.addChild(polchild);			
		return successResponse();
	}


	/* Response functions: encode response as XML document for streaming back to Racket. */
	
	
	public static Document printSystemInfo()
	{				
		Document xmldoc = makeInitialResponse("sysinfo");
		if(xmldoc == null) return null; // be safe (but bottle up exceptions)
	
		Element statsElement = MFormulaManager.getStatisticsNode(xmldoc);
		
		// TODO give more detail here later. These elements will be more than just counters.
		// (will want to list vocabs/collections saved by name.)
		Element vocabElement = xmldoc.createElementNS(null, "VOCABULARIES");
		vocabElement.setAttribute("count", String.valueOf(envVocabularies.size()));		
		
		Element idbCollElement = xmldoc.createElementNS(null, "COLLECTIONS");
		idbCollElement.setAttribute("count", String.valueOf(envIDBCollections.size()));
		
		Element cachedElement = xmldoc.createElementNS(null, "CACHED-RESULTS");
		cachedElement.setAttribute("count", String.valueOf(envQueryResults.size()));

		xmldoc.getDocumentElement().appendChild(statsElement);
		xmldoc.getDocumentElement().appendChild(vocabElement);
		xmldoc.getDocumentElement().appendChild(idbCollElement);
		xmldoc.getDocumentElement().appendChild(cachedElement);
					
		return xmldoc;
	}
	
	
	static Document errorResponse(String errorType, String errorSubtype, String desc) 
	{	
		Document xmldoc = makeInitialResponse("error");
		if(xmldoc == null) return null; // be safe (but bottle up exceptions)
		
		Element errorElement = xmldoc.createElementNS(null, "ERROR");
		
		errorElement.setAttribute("type", errorType);
		errorElement.setAttribute("subtype", errorSubtype);
		if(desc != null && desc.length() > 0)
		{
			errorElement.appendChild(xmldoc.createTextNode(desc));
		}
		xmldoc.getDocumentElement().appendChild(errorElement);
				
		
		return xmldoc;
	}
	static Document errorResponse(String errorType, String errorSubtype, Integer desc) 
	{
		return errorResponse(errorType, errorSubtype, desc.toString());
	}

	
	static Document exceptionResponse(Throwable e)
	{
		
		Document xmldoc = makeInitialResponse("exception");
		if(xmldoc == null) return null; // be safe (but bottle up exceptions)
		
		Element errorElement = xmldoc.createElementNS(null, "EXCEPTION");
		
		errorElement.setAttribute("class", e.getClass().getCanonicalName());
		errorElement.setAttribute("stack-trace", Arrays.toString(e.getStackTrace()));

		Element msgElement = xmldoc.createElementNS(null, "MESSAGE");
		if(e.getLocalizedMessage() != null && e.getLocalizedMessage().length() > 0 )
			msgElement.appendChild(xmldoc.createTextNode(e.getLocalizedMessage()));
		else
			msgElement.appendChild(xmldoc.createTextNode("(No message)"));
		
		errorElement.appendChild(msgElement);
		
		/*if(e instanceof MParserException)
		{
			MParserException ex = (MParserException) e;
			Element placeElement = xmldoc.createElementNS(null, "LOCATION");
			placeElement.setAttribute("row", String.valueOf(ex.row));
			placeElement.setAttribute("col", String.valueOf(ex.col));
			if(ex.errorValue.toString().length() > 0)
				placeElement.appendChild(xmldoc.createTextNode(ex.errorValue.toString()));
			errorElement.appendChild(placeElement);
		}
		else if(e instanceof MLexerException)
		{
			MLexerException ex = (MLexerException) e;
			Element placeElement = xmldoc.createElementNS(null, "LOCATION");
			placeElement.setAttribute("row", String.valueOf(ex.row));
			placeElement.setAttribute("col", String.valueOf(ex.col));
			if(ex.at.length() > 0)
				placeElement.appendChild(xmldoc.createTextNode(ex.at));
			errorElement.appendChild(placeElement);
		}
		else */
		/*if(e instanceof MUserException)
		{
			MUserException ex = (MUserException) e;
			Element placeElement = xmldoc.createElementNS(null, "LOCATION");
			//placeElement.setAttribute("row", String.valueOf(ex.row));
			//placeElement.setAttribute("col", String.valueOf(ex.col));
			placeElement.setAttribute("problem", String.valueOf(ex.problem));
			
			//if(String.valueOf(ex.errorValue).length() > 0)
			//	placeElement.appendChild(xmldoc.createTextNode(String.valueOf(ex.errorValue)));
			errorElement.appendChild(placeElement);
		}*/
		
		// Include the query that caused the problem.
		Element queryElement = xmldoc.createElementNS(null, "COMMAND");
		if(lastCommandReceived.length() > 0)
			queryElement.appendChild(xmldoc.createTextNode(lastCommandReceived));
		errorElement.appendChild(queryElement);
		
		xmldoc.getDocumentElement().appendChild(errorElement);
		
		return xmldoc;
	}
	
	
	private static Document noSolutionResponse(MQueryResult theResult, String id)
	{
		Document xmldoc = makeInitialResponse(sUnsat);
		if(xmldoc == null) return null; // be safe (but bottle up exceptions)
				
		Element statsElement = makeStatisticsElement(xmldoc, theResult, id);		
		xmldoc.getDocumentElement().appendChild(statsElement);								
		return xmldoc;
	}

	private static Element makeStatisticsElement(Document xmldoc, MQueryResult theResult, String id)
	{
		Element statsElement = xmldoc.createElementNS(null, "STATISTICS");;		
		
		// Report the size ceiling (calculated and user-provided) so a warning
		// can be given if need be.
		statsElement.setAttribute("max-size", String.valueOf(theResult.maxSize));
		statsElement.setAttribute("user-max-size", String.valueOf(theResult.forQuery.userSizeCeiling));
		statsElement.setAttribute("computed-max-size", String.valueOf(theResult.sufficientMaxSize));
		statsElement.setAttribute("result-id", String.valueOf(id));
		return statsElement;
	}
	
	private static Document successResponse()
	{
		// <MARGRAVE-RESPONSE>success</MARGRAVE-RESPONSE>
	
		Document xmldoc = makeInitialResponse(sSuccess);
		if(xmldoc == null) return null; // be safe (but bottle up exceptions)
		return xmldoc;
	}
	
	private static Document makeInitialResponse(String type) 
	{
		DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
		try
		{
			DocumentBuilder builder = factory.newDocumentBuilder();
			DOMImplementation impl = builder.getDOMImplementation();
		
			Document thedoc = impl.createDocument(null, sTopQualifiedName, null);
			thedoc.getDocumentElement().setAttribute("type", type);
			return thedoc;
		}
		catch(Exception e)
		{
			return null;
		}
	}

	private static Document unsupportedResponse()
	{
		return errorResponse("unsupported operation", "", "");
	}
	
	private static Document resultHandleResponse(Integer id)
	{			
		Document xmldoc = makeInitialResponse("explore-result");
		if(xmldoc == null) return null; // be safe (but bottle up exceptions)		
		Element handleElement = xmldoc.createElementNS(null, "RESULT-HANDLE");
		handleElement.appendChild(xmldoc.createTextNode(id.toString()));
		xmldoc.getDocumentElement().appendChild(handleElement);		
		return xmldoc;
	}
	
	private static Document boolResponseWithStats(MQueryResult aResult, String id, boolean b)
	{
		Document xmldoc = makeInitialResponse("boolean");
		if(xmldoc == null) return null; // be safe (but bottle up exceptions)
		
		Element booleanElement = xmldoc.createElementNS(null, "BOOLEAN");
		booleanElement.setAttribute("value", String.valueOf(b));		
		xmldoc.getDocumentElement().appendChild(booleanElement);
		
		xmldoc.getDocumentElement().appendChild(makeStatisticsElement(xmldoc, aResult, id));
		return xmldoc;
	}
	
	private static Document boolResponse(boolean b)
	{		
		Document xmldoc = makeInitialResponse("boolean");
		if(xmldoc == null) return null; // be safe (but bottle up exceptions)
		
		Element booleanElement = xmldoc.createElementNS(null, "BOOLEAN");
		booleanElement.setAttribute("value", String.valueOf(b));		
		xmldoc.getDocumentElement().appendChild(booleanElement);
		
		return xmldoc;
	}
	private static Document intResponse(int theInt)
	{
		return stringResponse(String.valueOf(theInt));
	}
	
	private static Document stringResponse(String str)
	{		
		Document xmldoc = makeInitialResponse("string");
		if(xmldoc == null) return null; // be safe (but bottle up exceptions)
		
		Element stringElement = xmldoc.createElementNS(null, "STRING");
		if(str != null)
		{					
			stringElement.setAttribute("value", str);
		}
		
		xmldoc.getDocumentElement().appendChild(stringElement);
		return xmldoc;
	}

	protected static void flushBuffers(String arg)
	{
		Document xmldoc = makeInitialResponse(arg);
		if(xmldoc != null)
		{
			Text val = xmldoc.createTextNode(sDebug);
			xmldoc.getDocumentElement().appendChild(val);
			
			// Explicitly add the buffers (since this func will be called from outside the main loop)
			MCommunicator.addBuffers(xmldoc);
			
			byte[] theBytes = MCommunicator.transformXML(xmldoc);
			try
			{
				MCommunicator.out.write(theBytes);
			} catch (IOException e)
			{										
			}
			MCommunicator.out.flush();
		}
	}
	
	public static Document quitMargrave()
	{
		Document xmldoc = makeInitialResponse("quit");
		if(xmldoc != null)
		{
			Text val = xmldoc.createTextNode(sQuitMargrave);
			xmldoc.getDocumentElement().appendChild(val);	
			byte[] theBytes = MCommunicator.transformXML(xmldoc);
			try
			{
				MCommunicator.out.write(theBytes);
			} catch (IOException e)
			{										
			}
			MCommunicator.out.flush();
		}
		System.exit(0);
		return null;
	}

	private static Document scenarioResponse(MQueryResult mQueryResult,
			MSolutionInstance nextPreTup, String id)
	{
		MSolutionInstance next;
		MVocab theVocab;
		
		// If this query was tupled, don't forget to convert back to the original signature.		
		if(mQueryResult.forQuery.tupled) 
		{
			next = mQueryResult.forQuery.processTupledSolutionForThis(nextPreTup);
			theVocab = mQueryResult.forQuery.internalTupledQuery.vocab;
			MCommunicator.writeToLog("\nscenarioResponse: query was TUPLED.");
			MCommunicator.writeToLog("\ntupled model = "+nextPreTup.getFacts());
			MCommunicator.writeToLog("\nconverted model = "+next.getFacts());			
			MCommunicator.writeToLog("\nAnnotations: ");
			MCommunicator.writeToLog(next.getAnnotations().toString());
		}
		else
		{
			next = nextPreTup;
			theVocab = mQueryResult.forQuery.vocab;
			MCommunicator.writeToLog("\nscenarioResponse: query was NOT tupled.");
			
		}
		
		//writeToLog("\nORIG INSTANCE: "+nextPreTup.getFacts());
		//writeToLog("\nPOST CONVERSION INSTANCE: "+next.getFacts());
		
		Document xmldoc = makeInitialResponse("model");
		if(xmldoc == null) return null; // be safe (but bottle up exceptions)		
		Element modelElement = xmldoc.createElementNS(null, "MODEL");
		
		Instance facts = next.getFacts();
		List<String> annotations = next.getAnnotations();
		
		modelElement.setAttribute("size", String.valueOf(facts.universe().size()));
				
		// What is the universe?
		Element universeElement = xmldoc.createElementNS(null, "UNIVERSE");
		for(Object o : facts.universe())
		{
			Element atomElement = xmldoc.createElementNS(null, "ATOM");			
			atomElement.appendChild(xmldoc.createTextNode(o.toString()));	
			universeElement.appendChild(atomElement);
		}
		modelElement.appendChild(universeElement);
		
		
		// For each relation, what's in it?
		for(Relation r : facts.relations())
		{
			Element relationElement = xmldoc.createElementNS(null, "RELATION");
			relationElement.setAttribute("name", r.name());
			relationElement.setAttribute("arity", String.valueOf(r.arity()));
			
			// Is this relation a sort or a predicate?
			MSort theSort = null;
			if(theVocab.isSort(r.name()))
			{
				relationElement.setAttribute("type", "sort");
				theSort = theVocab.fastGetSort(r.name());
			}
			else
			{
				relationElement.setAttribute("type", "predicate");
			}
			
			for(Tuple t : facts.relationTuples().get(r))
			{
				Element tupleElement = xmldoc.createElementNS(null, "TUPLE");
				
				// If this is a sort, need to say whether there's a child sort
				// having this tuple in it as well (used for pretty printing)
				if(theSort != null)
				{
					boolean bInSubsort = MQuery.someChildContainsTuple(facts, theSort, t);
					tupleElement.setAttribute("not-in-subsort", String.valueOf(!bInSubsort));	
				}				
				
				for(int ii = 0; ii<t.arity();ii++)
				{
					Element atomElement = xmldoc.createElementNS(null, "ATOM");
					Object theAtom = t.atom(ii);
					atomElement.appendChild(xmldoc.createTextNode(theAtom.toString()));	
					tupleElement.appendChild(atomElement);
				}				
				
				relationElement.appendChild(tupleElement);
			}
			
			modelElement.appendChild(relationElement);
		}
		
		// !!! TODO: s is a string here, e.g.  "Rel is true of: [x, y]".
		// Should be structured so Racket can display however it wants.
		for(String s : annotations)
		{
			Element annElement = xmldoc.createElementNS(null, "ANNOTATION");
			annElement.appendChild(xmldoc.createTextNode(s));				
			modelElement.appendChild(annElement);
		}
					
		xmldoc.getDocumentElement().appendChild(modelElement);
		
		Element statsElement = makeStatisticsElement(xmldoc, mQueryResult, id);		
		xmldoc.getDocumentElement().appendChild(statsElement);		
		
		return xmldoc;
	}
	
	private static Document listResponse(List<?> res)
	{
		Document xmldoc = makeInitialResponse("list");
		if(xmldoc == null) return null; // be safe (but bottle up exceptions)		
		Element listElement = xmldoc.createElementNS(null, "LIST");
		listElement.setAttribute("size", String.valueOf(res.size()));
		
		int iOrder = 1;
		for(Object obj : res)
		{
			Element objElement = xmldoc.createElementNS(null, "ITEM");
			objElement.setAttribute("type", obj.getClass().getCanonicalName());
			objElement.setAttribute("order", String.valueOf(iOrder));
			
			// WORRY potential info loss in the toString call here. 
			objElement.appendChild(xmldoc.createTextNode(obj.toString()));
			listElement.appendChild(objElement);
			iOrder++;
		}
				
		xmldoc.getDocumentElement().appendChild(listElement);		
		return xmldoc;
	}
	
	private static Document setResponse(Set<String> set)
	{
		Document xmldoc = makeInitialResponse("set");
		if(xmldoc == null) return null; // be safe (but bottle up exceptions)		
		Element setElement = xmldoc.createElementNS(null, "SET");
		setElement.setAttribute("size", String.valueOf(set.size()));
		
		for(Object obj : set)
		{
			Element objElement = xmldoc.createElementNS(null, "ITEM");
			objElement.setAttribute("type", obj.getClass().getCanonicalName());
			
			// WORRY potential info loss in the toString call here. 
			objElement.appendChild(xmldoc.createTextNode(obj.toString()));
			setElement.appendChild(objElement);			
		}
		
		xmldoc.getDocumentElement().appendChild(setElement);		
		return xmldoc;
	}

	private static Document mapResponse(Map<String, Set<String>> outsets)
	{
		Document xmldoc = makeInitialResponse("map");
		if(xmldoc == null) return null; // be safe (but bottle up exceptions)		
		Element mapElement = xmldoc.createElementNS(null, "MAP");
				
		for(Object key : outsets.keySet())
		{
			Element entryElement = xmldoc.createElementNS(null, "ENTRY");
			entryElement.setAttribute("key-type", key.getClass().getCanonicalName());
			entryElement.setAttribute("key", key.toString());
			
			// WORRY some info lost in the toString calls here.
			for(Object obj : outsets.get(key))
			{
				Element valueElement = xmldoc.createElementNS(null, "VALUE");	
				valueElement.setAttribute("type", obj.getClass().getCanonicalName());
				valueElement.appendChild(xmldoc.createTextNode(obj.toString()));
				entryElement.appendChild(valueElement);
			}
			
			mapElement.appendChild(entryElement);			
		}
		
		
		xmldoc.getDocumentElement().appendChild(mapElement);		
		return xmldoc;
	}
	
	public static void clearLastQuery()
	{
		envIDBCollections.remove("last");
		envQueryResults.remove("last");
		envIterators.remove("last");		
	}
	public static void debug() 
	{
		for(String v : envVocabularies.keySet())
		{
			MEnvironment.errorWriter.println("Vocabulary: "+v);
			MVocab voc = envVocabularies.get(v);
			MEnvironment.errorWriter.println(voc.sorts);
			MEnvironment.errorWriter.println(voc.predicates);
			MEnvironment.errorWriter.println(voc.constants);
			MEnvironment.errorWriter.println(voc.functions);
		}
		
		for(String p : envIDBCollections.keySet())
		{
			MEnvironment.errorWriter.println("IDB Collection: "+p);
			MIDBCollection coll = envIDBCollections.get(p);
			
			MEnvironment.errorWriter.println(coll.idbsAsString());
			MEnvironment.errorWriter.println(coll.varOrderings);
			MEnvironment.errorWriter.println(coll.varSorts);
			
			if(coll instanceof MPolicyLeaf)
			{
				MPolicyLeaf pleaf = (MPolicyLeaf) coll;
				MEnvironment.errorWriter.println("(Policy Leaf)");
				pleaf.prettyPrintRules();
				MEnvironment.errorWriter.println(pleaf.rCombineFA);
				MEnvironment.errorWriter.println(pleaf.rCombineWhatOverrides);
			}
			if(coll instanceof MQuery)
			{
				MQuery qry = (MQuery) coll;
				MEnvironment.errorWriter.println("(Saved Query)");
				MQueryResult res = qry.runQuery();
				MTotalInstanceIterator it = res.getTotalIterator();
				try
				{
					MSolutionInstance sol = it.next();
					MEnvironment.errorWriter.println(sol.getFacts());
				}
				catch(MGENoMoreSolutions e)
				{
					MEnvironment.errorWriter.println("No more solutions.");
				}
				MEnvironment.errorWriter.println(res.sufficientMaxSize);
			}
			
			MEnvironment.errorWriter.println();
		}
		
		
	}
	
	/*
	public static Document returnCompareQuery(String originalXMLText,
			String polname1, String polname2,
			Boolean tupling, Integer debugLevel,
			Integer ceilingLevel)
	throws MUserException
	{
		
		// Get the policies
		MIDBCollection coll1 = envIDBCollections.get(polname1);
		MIDBCollection coll2 = envIDBCollections.get(polname2);		
		if(coll1 == null)
			return errorResponse(sUnknown, sPolicy, polname1);
		if(coll2 == null)
			return errorResponse(sUnknown, sPolicy, polname2);

		// should be a different error message
		if(!(coll1 instanceof MPolicy))
			return errorResponse(sUnknown, sPolicy, polname1);
		if(!(coll2 instanceof MPolicy))
			return errorResponse(sUnknown, sPolicy, polname2);
				
		MPolicy pol1 = (MPolicy)coll1;
		MPolicy pol2 = (MPolicy)coll2;
		
		// Create exploreCondition
		MQuery theQuery = pol1.compareWithPolicy(pol2);
							
		theQuery.debug_verbosity = debugLevel;
		theQuery.userSizeCeiling = ceilingLevel;
		theQuery.doTupling = tupling;
		
		return returnQueryResponse(theQuery, originalXMLText);
	}*/

	
	
}
