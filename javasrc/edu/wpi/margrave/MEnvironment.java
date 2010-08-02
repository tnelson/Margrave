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
import javax.xml.parsers.ParserConfigurationException;

import org.w3c.dom.DOMImplementation;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Text;

import kodkod.ast.*;
import kodkod.instance.Instance;
import kodkod.instance.Tuple;

import java_cup.runtime.Symbol;


class MSemanticException extends IOException
{	
	String problem;

	MSemanticException(String problem)
	{
		super("Margrave could not understand..."); // don't have a null message
		this.problem = problem;
	}
}

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
	// necessary. -TN
	
	
	HashMap<List<Variable>, Set<MVariableVectorAssertion>> assertAtomicNecessary =
		new HashMap<List<Variable>, Set<MVariableVectorAssertion>>();
	HashMap<List<Variable>, Set<MVariableVectorAssertion>> assertNecessary =
		new HashMap<List<Variable>, Set<MVariableVectorAssertion>>();
	HashMap<List<Variable>, Set<MVariableVectorAssertion>> assertSufficient =
		new HashMap<List<Variable>, Set<MVariableVectorAssertion>>();
	
	
	
	MExploreCondition(Formula f, Variable x, Variable y)
	{
		fmla = f;		
		// No assertions, but make an entry so we know there's a free variable.
		
		List<Variable> thisVarX = new ArrayList<Variable>();
		thisVarX.add(x);	// lists with equal elements are equal.
		List<Variable> thisVarY = new ArrayList<Variable>();
		thisVarY.add(y);	// lists with equal elements are equal.
		
		assertAtomicNecessary.put(thisVarX, new HashSet<MVariableVectorAssertion>());
		assertNecessary.put(thisVarX, new HashSet<MVariableVectorAssertion>());
		assertSufficient.put(thisVarX, new HashSet<MVariableVectorAssertion>());

		assertAtomicNecessary.put(thisVarY, new HashSet<MVariableVectorAssertion>());
		assertNecessary.put(thisVarY, new HashSet<MVariableVectorAssertion>());
		assertSufficient.put(thisVarY, new HashSet<MVariableVectorAssertion>());
	}
	
	MExploreCondition(Formula f, Relation made, List<String> varnamevector)
	{
		fmla = f;
		madeEDBs.add(made);
		
		List<Variable> varvector = vectorize(varnamevector);
	
		
		// Initialize assertion pools for this vector
		assertAtomicNecessary.put(varvector, new HashSet<MVariableVectorAssertion>());
		assertNecessary.put(varvector, new HashSet<MVariableVectorAssertion>());
		assertSufficient.put(varvector, new HashSet<MVariableVectorAssertion>());
		
		// This constructor means an atomic necessary assertion:
		assertAtomicNecessary.get(varvector).add(new MVariableVectorAssertion(true, made));				
	}
	
	MExploreCondition(Formula f, MIDBCollection pol, List<String> varnamevector)
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
			
			if(!assertAtomicNecessary.containsKey(thisVar))
				assertAtomicNecessary.put(thisVar, new HashSet<MVariableVectorAssertion>());
			if(!assertNecessary.containsKey(thisVar))
				assertNecessary.put(thisVar, new HashSet<MVariableVectorAssertion>());
			if(!assertSufficient.containsKey(thisVar))
				assertSufficient.put(thisVar, new HashSet<MVariableVectorAssertion>());
			
			Variable oldVar = pol.varOrdering.get(iIndex);
			iIndex ++;
			
			//System.out.println(oldVar);
			//System.out.println(pol.varSorts);
			//System.out.println(assertAtomicNecessary.get(thisVar));
			
			assertAtomicNecessary.get(thisVar).add(new MVariableVectorAssertion(true, pol.varSorts.get(oldVar)));	
		}
	}
	
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
					
		doAssertAnd(oth);
				
		return this;
	}

	MExploreCondition or(MExploreCondition oth)	
	{
		fmla = MFormulaManager.makeOr(fmla, oth.fmla);
		seenIDBs.addAll(oth.seenIDBs);
		madeEDBs.addAll(oth.madeEDBs);
		
		doAssertOr(oth);
		
		return this;
	}

	void synchKeys(MExploreCondition oth)
	{
		// What if oth mentions variables that we know nothing about?
		
		for(List<Variable> thisVar : oth.assertNecessary.keySet())
		{
			if(!assertAtomicNecessary.containsKey(thisVar))
				assertAtomicNecessary.put(thisVar, new HashSet<MVariableVectorAssertion>());
			if(!assertNecessary.containsKey(thisVar))
				assertNecessary.put(thisVar, new HashSet<MVariableVectorAssertion>());
			if(!assertSufficient.containsKey(thisVar))
				assertSufficient.put(thisVar, new HashSet<MVariableVectorAssertion>());
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
			
			assertAtomicNecessary.get(varvector).clear();
		}		
	}
	
	MExploreCondition implies(MExploreCondition oth)	
	{
		fmla = MFormulaManager.makeImplication(fmla, oth.fmla);
		seenIDBs.addAll(oth.seenIDBs);
		madeEDBs.addAll(oth.madeEDBs);
		
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
	
	private static Map<Integer, MQueryResult> envQueryResults = new HashMap<Integer, MQueryResult>();
	private static Map<Integer, MInstanceIterator> envIterators = new HashMap<Integer, MInstanceIterator>();
	
	// BOTH are System.err because out is reserved for XML communication.
	protected static PrintStream errorStream = System.err;
	protected static PrintStream outStream = System.err;
	
	public static boolean debugParser = false;
	
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
	public static String sQuitMargrave = "quit";

	public static String tempVarPrefix = "_";
	
	// Used in exception output
	static String lastCommandReceived = "";

		
	static MIDBCollection getPolicyOrView(String str)
	{
		
		
		str = str.toLowerCase();
		
		// Is str the name of an idb I know?
		if(envIDBCollections.containsKey(str))
			return envIDBCollections.get(str);
		
		return null;
	}
	
	static MVocab getVocab(String str)
	{
		str = str.toLowerCase();	
		if(envVocabularies.containsKey(str))
			return envVocabularies.get(str);		
		return null;
	}

	static MQueryResult getQueryResult(int num)
	{		
		if(envQueryResults.containsKey(num))
			return envQueryResults.get(num);		
		return null;
	}

	
	static Formula getIDB(String collname, String idbname)
	{
		collname = collname.toLowerCase();
		
		MIDBCollection collection = getPolicyOrView(collname);
		if(collection == null)
			return null;
		
		if(collection.idbs.containsKey(idbname))
			return collection.idbs.get(idbname);
		return null;
	}
	
	static Formula getOnlyIDB(String collname)
	{
		// Used for view IDB collections, which must have only one IDB.
		MIDBCollection collection = getPolicyOrView(collname);
		if(collection == null)
			return null;
		
		if(collection.idbs.size() != 1)
			return null;
		
		for(String name : collection.idbs.keySet())
			return collection.idbs.get(name);
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
			outStream.println(out.toString());
	}
	
	static Document getNextModel(int numResult)
	throws MGEUnknownIdentifier, MGEUnsortedVariable, MGEManagerException, MGEBadIdentifierName
	{
		// Do we have a result for this num?
		if(!envQueryResults.containsKey(numResult))
			return errorResponse(sUnknown, sResultID, numResult);
		
		if(!envIterators.containsKey(numResult))
			return getFirstModel(numResult);
		else
		{
			// Return next model in the iterator.
			MQueryResult result = envQueryResults.get(numResult);
			try
			{
				return scenarioResponse(result, envIterators.get(numResult).next(), numResult);
			}
			catch(MGENoMoreSolutions e)
			{
				return noSolutionResponse(result, numResult);
			}
		}		
	}
				
	static Document getFirstModel(int numResult) 
	throws MGEUnknownIdentifier, MGEUnsortedVariable, MGEManagerException, MGEBadIdentifierName
	{
		// Do we have a result for this num?
		if(!envQueryResults.containsKey(numResult))
			return errorResponse(sUnknown, sResultID, numResult);

		MQueryResult result = envQueryResults.get(numResult);
		
		// Reset the iterator (or create it if new)
		MInstanceIterator it = result.getTotalIterator();
		envIterators.put(numResult, it);
		
		try
		{
			return scenarioResponse(result, it.next(), numResult);
		}
		catch(MGENoMoreSolutions e)
		{
			return noSolutionResponse(result, numResult);
		}
		
	}
	
	static public Document returnQueryResponse(MQuery qry, String XMLCommand)
	{
		lastCommandReceived = XMLCommand.trim();	
		
		try
		{
				// May return null if an internal error.
				if(qry == null)
				{
					//errorStream.println("-> Command failed.");
					return errorResponse(sFailure, sQuery, XMLCommand);
				}
				
				// Compile the query and get the result handle.
				MQueryResult qryResult = qry.runQuery();
				
				// TODO Don't store more than one for now.
				envQueryResults.put(0, qryResult);
				return resultHandleResponse(0);
												
			}  // end: a query
		catch(Exception e)
		{
			// Unexpected! Bad exception			
			return exceptionResponse(e);
		}
	}
	

/*	static public Document command(String cmd, boolean silent)
	{
		lastCommandReceived = cmd.trim();
		Reader reader = new StringReader(cmd);
		MCommandLexer theLexer = new MCommandLexer(reader);
		MCommandParser theParser = new MCommandParser(theLexer);			
		
		try
		{
			Symbol result;
			if(debugParser)
				result = theParser.debug_parse();
			else
				result = theParser.parse();	
			
			// May not be a query. May be something else (RENAME or INFO command)							
			
			// EXPLORE
			// or 
			// COMPARE
			if (result.value instanceof MQuery)
			{			
				MQuery qry = (MQuery) result.value;
			
				// May return null if an internal error.
				if(qry == null)
				{
					errorStream.println("-> Command failed.");
					return errorResponse(sFailure, sQuery, cmd);
				}
				
				// Compile the query and get the result handle.
				MQueryResult qryResult = qry.runQuery();
				
				// TODO Don't store more than one for now.
				envQueryResults.put(0, qryResult);
				return resultHandleResponse(0);
												
			}  // end: a query
			
			// Else, it's an INFO command etc.
			else
			{
				Document resDoc;
				if(result.value instanceof Document)
					resDoc = (Document) result.value;
				else
					resDoc = errorResponse(sNotDocument, "", "");

				
				return resDoc;
			}

		}
		catch(MParserException e)
		{
			return exceptionResponse(e);		
		}
		catch(MLexerException e)
		{
			return exceptionResponse(e);
		}		
		catch(MSemanticException e)
		{
			return exceptionResponse(e);
		}
		catch(Exception e)
		{
			// Unexpected! Bad exception			
			return exceptionResponse(e);
		}
		
	}
	*/
	
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
		// TODO should also give more info
			
		Document xmldoc = makeInitialResponse("collection-info");
		if(xmldoc == null) return null; // be safe (but bottle up exceptions)		
				
		if(coll instanceof MPolicyLeaf)
		{
			MPolicyLeaf pleaf = (MPolicyLeaf) coll;
			Element theElement = xmldoc.createElementNS(null, "POLICY-LEAF");
			theElement.setAttribute("name", pleaf.name);
			theElement.setAttribute("rule-combine", pleaf.rCombine);
			
			Element idbsElement = xmldoc.createElementNS(null, "IDBS");
			for(String key : coll.idbs.keySet())
			{
				Element idbElement = xmldoc.createElementNS(null, "IDB");
				idbElement.setAttribute("base-name", key);
				idbElement.appendChild(xmldoc.createTextNode(coll.name+":"+key));
				idbsElement.appendChild(idbElement);
			}
			theElement.appendChild(idbsElement);
			
			Element varsElement = xmldoc.createElementNS(null, "FREE-VARIABLES");
			for(Variable v : coll.varOrdering)
			{
				Element varElement = xmldoc.createElementNS(null, "VARIABLE");
				varElement.appendChild(xmldoc.createTextNode(v.name()));
				varsElement.appendChild(varElement);
			}
			theElement.appendChild(varsElement);
						
			xmldoc.getDocumentElement().appendChild(theElement);
		}
		else if(coll instanceof MPolicySet)
		{
			MPolicySet pset = (MPolicySet) coll;
			Element theElement = xmldoc.createElementNS(null, "POLICY-SET");
			theElement.setAttribute("name", pset.name);
			theElement.setAttribute("policy-combine", pset.pCombine);
			
			Element idbsElement = xmldoc.createElementNS(null, "IDBS");
			for(String key : coll.idbs.keySet())
			{
				Element idbElement = xmldoc.createElementNS(null, "IDB");
				idbElement.setAttribute("base-name", key);
				idbElement.appendChild(xmldoc.createTextNode(coll.name+":"+key));
				idbsElement.appendChild(idbElement);
			}
			theElement.appendChild(idbsElement);
			
			Element varsElement = xmldoc.createElementNS(null, "FREE-VARIABLES");
			for(Variable v : coll.varOrdering)
			{
				Element varElement = xmldoc.createElementNS(null, "VARIABLE");
				varElement.appendChild(xmldoc.createTextNode(v.name()));
				varsElement.appendChild(varElement);
			}
			theElement.appendChild(varsElement);
			
			xmldoc.getDocumentElement().appendChild(theElement);
		}
		else
		{
			MQuery qry = (MQuery) coll;
			Element theElement = xmldoc.createElementNS(null, "SAVED-QUERY");
			theElement.setAttribute("name", qry.name);

			Element idbsElement = xmldoc.createElementNS(null, "IDBS");
			for(String key : coll.idbs.keySet())
			{
				Element idbElement = xmldoc.createElementNS(null, "IDB");
				idbElement.setAttribute("base-name", key);
				idbElement.appendChild(xmldoc.createTextNode(coll.name+":"+key));
				idbsElement.appendChild(idbElement);
			}
			theElement.appendChild(idbsElement);
			
			Element varsElement = xmldoc.createElementNS(null, "FREE-VARIABLES");
			for(Variable v : coll.varOrdering)
			{
				Element varElement = xmldoc.createElementNS(null, "VARIABLE");
				varElement.appendChild(xmldoc.createTextNode(v.name()));
				varsElement.appendChild(varElement);
			}
			theElement.appendChild(varsElement);
			
			xmldoc.getDocumentElement().appendChild(theElement);
		}
					
		
		return xmldoc;
	}

	private static Document getVocabInfo(MVocab voc) 
	{
		Document xmldoc = makeInitialResponse("vocabulary-info");
		if(xmldoc == null) return null; // be safe (but bottle up exceptions)		
		Element theElement = xmldoc.createElementNS(null, "VOCABULARY");
		theElement.setAttribute("name", voc.vocab_name);
		
		Element sortsElement = xmldoc.createElementNS(null, "SORTS");
		Element reqElement = xmldoc.createElementNS(null, "REQ-VECTOR");
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
		
		int iOrder = 1;
		for(Variable var : voc.requestVectorOrder)
		{
			Element varElement = xmldoc.createElement("VARIABLE");
			varElement.setAttribute("order", String.valueOf(iOrder));
			varElement.appendChild(xmldoc.createTextNode(var.name()));
			reqElement.appendChild(varElement);	
			iOrder++;
		}
		
		// AXIOMS
		// TODO more axioms
		
		
		Element disjElement = xmldoc.createElementNS(null, "DISJOINT");
		
		
		for(MSort s : voc.axioms.axiomDisjoints.keySet())
		{
			if(voc.axioms.axiomDisjoints.get(s).size() < 1)
				continue;
			
			Element firstElement = xmldoc.createElementNS(null, "SORT");
			firstElement.setAttribute("name", s.name);
			
			for(MSort t : voc.axioms.axiomDisjoints.get(s))
			{
				Element secondElement = xmldoc.createElementNS(null, "SORT");
				secondElement.setAttribute("name", t.name);
				firstElement.appendChild(secondElement);
			}
			
			disjElement.appendChild(firstElement);
		}		
		axiomsElement.appendChild(disjElement);
		
		theElement.appendChild(sortsElement);
		theElement.appendChild(reqElement);
		theElement.appendChild(axiomsElement);
		xmldoc.getDocumentElement().appendChild(theElement);		
		return xmldoc;
	}

	// Returns true if overwriting existing identifier
	// Assume that the policy is already loaded FOR NOW. 
	// Later we will add a LOAD POLICY keyword.
	static public Document savePolicyAs(String ident, MPolicy pol)
	{
		ident = ident.toLowerCase();
		
		if("last".equals(ident))
		{						
			errorResponse(sReserved, sIDBCollection, ident);
		}
				
		return boolResponse(envIDBCollections.put(ident, pol) != null);			
	}
	
	static public Document renameIDBCollection(String identold, String identnew)
	{
		identold = identold.toLowerCase();
		identnew = identnew.toLowerCase();
		
		if("last".equals(identnew))
		{						
			return errorResponse(sReserved, sIDBCollection, identnew);
		}
		
		if(!envIDBCollections.containsKey(identold))
		{			
			return errorResponse(sUnknown, sIDBCollection, identold);		
		}
		
		MIDBCollection collToMove = envIDBCollections.get(identold);
		
		// Important: Change the "name" field of the IDB Collection, too.
		// (Some internal code depends on this.)
		// TODO -- remove the name field entirely and add a reverse-lookup in MEnvironment
		collToMove.name = identnew;
		
		boolean overwrote = envIDBCollections.put(identnew, collToMove) != null;
		envIDBCollections.remove(identold);
		return boolResponse(overwrote);		
	}
	
	/*static public Document doCompare(String pol1, String pol2, Map<String, Set<List<String>>> idbOutputMap,
			Boolean tupling, Integer debuglevel, Integer sizeceiling)
	{
		// Pol1 and Pol2 are policies to compare.
		// If their vocabs are incompatible the query will throw an exception.
		
		MIDBCollection thePol1 = getPolicyOrView(pol1);
		MIDBCollection thePol2 = getPolicyOrView(pol2);
		
		// Construct the request vector
		// Better be the same variable ordering!
		String reqvars = "";
		for(Variable v : thePol1.varOrdering)
		{
			if(reqvars.length() < 1)
				reqvars = v.name();
			else
				reqvars = reqvars.concat(", "+v.name());			
		}
		
		String theCmd = "";
		
		// If P1 ever decides this way, but P2 does not. (And vice versa.)
		// Note that since we check in both directions, the N/a <-> non-N/a possibilities are covered. 
		for(String d : thePol1.vocab.decisions)
		{
			if(theCmd.length() < 1)
				theCmd = "EXPLORE ";
			else
				theCmd += " OR ";
			
			// Check forward
			theCmd += "(" + 
			pol1+":"+d+"("+reqvars+")"+
			" AND NOT "+
			pol2+":"+d+"("+reqvars+")"+			
			") OR (" +
			
			// Check backward
			pol2+":"+d+"("+reqvars+")"+
			" AND NOT "+
			pol1+":"+d+"("+reqvars+")"+	
			")\n";
		}
		
		// Add stock publish statement 
		theCmd += " PUBLISH "+reqvars;							
		
		if(tupling)
			theCmd += " TUPLING ";
		
		if(debuglevel > 0)
			theCmd += " DEBUG "+debuglevel+" ";
		
		theCmd += " CEILING "+sizeceiling+" ";

		//System.out.println("---- ");
		//System.out.println(thePol1.varOrdering);
		//System.out.println(theCmd);
				
		return command(theCmd);		
	}*/
	
	
	static protected void setLast(MQuery qry)
	{
		envIDBCollections.put("last", qry);
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
	

	/*public static Document showNextModel(Integer id)
	{
		MQueryResult aResult = getResultObject(id);
		MInstanceIterator anIterator = getIteratorFor(id);
		if(aResult == null)
			return errorResponse(sUnknown, sResultID, id);
		if(anIterator == null)
			return errorResponse(sNoIterator, "", id);
		
		try
		{
			MSolutionInstance sol = anIterator.next();
			String str = aResult.getPrettySolution(sol, anIterator);
			return stringResponse(str);
		}
		catch(MGENoMoreSolutions e)
		{
			return noSolutionResponse(id);
		}

	}*/
	
	/*public static Document showFirstModel(Integer id)
	{
		MQueryResult aResult = getResultObject(id);		
		if(aResult == null)
			return errorResponse(sUnknown, sResultID, id);
		
		try
		{	
			// create new iterator
			MInstanceIterator anIterator = aResult.getTotalIterator();
			
			// store it 			
			envIterators.put(Integer.valueOf(id), anIterator);
			
			// return the first model
			MSolutionInstance sol = anIterator.next();
			String str = aResult.getPrettySolution(sol, anIterator);			
			return stringResponse(str);
		}
		catch(MGENoMoreSolutions e)
		{
			return noSolutionResponse(id);
		}
		catch(MGException e)
		{
			return exceptionResponse(e);
		}
		 
	}*/

	protected static MInstanceIterator getIteratorFor(Integer id)
	{
		return envIterators.get(id);
	}
	
	public static Document showPopulated(Integer id,
			Map<String, Set<List<String>>> rlist,
			Map<String, Set<List<String>>> clist) 
	{
		MQueryResult aResult = getResultObject(id);
		if(aResult == null)
			return errorResponse(sUnknown, sResultID, id);
				
		Map<String, Set<String>> outsets;
		try
		{
			outsets = aResult.getPopulatedRelationFinder().getPopulatedRelations(rlist, clist);
			
			if(outsets.size() == 1 && outsets.containsKey(""))				
				return setResponse(outsets.get(""));
			return mapResponse(outsets);
			
		}
		catch (MGException e)
		{
			return exceptionResponse(e);
		}
		catch (IOException e)
		{
			return exceptionResponse(e);
		}
	}

	public static Document showNextCollapse(Integer id) 
	{
		// TODO No support for partial models yet
		return unsupportedResponse();
	}

	public static Document showPopulated(Integer id, Map<String, Set<List<String>>> rlist)
	{
		return showPopulated(id, rlist,  new HashMap<String, Set<List<String>>>());
	}
	
	public static Document showUnpopulated(Integer id,
			Map<String, Set<List<String>>> rlist,
			Map<String, Set<List<String>>> clist) throws MSemanticException
	{
		MQueryResult aResult = getResultObject(id);
		if(aResult == null)
			return errorResponse(sUnknown, sResultID, id);
				
		Map<String, Set<String>> outsets;
		try
		{
			outsets = aResult.getPopulatedRelationFinder().getUnpopulatedRelations(rlist, clist);
			
			if(outsets.size() == 1 && outsets.containsKey(""))				
				return setResponse(outsets.get(""));
			return mapResponse(outsets);
			
		}
		catch (MGException e)
		{
			return exceptionResponse(e);
		}
	}
	
	public static Document showUnpopulated(Integer id, Map<String, Set<List<String>>> rlist) throws MSemanticException 
	{
		return showUnpopulated(id, rlist,  new HashMap<String, Set<List<String>>>());
	}
	
	public static Document countModels(Integer id, Integer n)
	{
		MQueryResult aResult = getResultObject(id);
		if(aResult == null)
			return errorResponse(sUnknown, sResultID, id);
		return intResponse(aResult.countModelsAtSize(n));				
	}

	public static Document countModels(Integer id)
	{
		return countModels(id, -1); // -1 for overall total to ceiling
	}

	public static Document isGuar(Integer id)
	{
		// Is this solution complete? (Is the ceiling high enough?)
		MQueryResult aResult = getResultObject(id);
		if(aResult == null)
			return errorResponse(sUnknown, sResultID, id);
		if(aResult.get_hu_ceiling() > aResult.get_universe_max())
			return boolResponse(false);
		return boolResponse(true);
	}

	public static Document isPoss(Integer id) 
	{	
		MQueryResult aResult = getResultObject(id);
		if(aResult == null)
			return errorResponse(sUnknown, sResultID, id);
		try
		{
			return boolResponseWithStats(aResult, id, aResult.isSatisfiable());
		}
		catch(MGException e)
		{
			return exceptionResponse(e);
		}
	}

	public static Document showCeiling(Integer id)
	{
		MQueryResult aResult = getResultObject(id);
		if(aResult == null)
			return errorResponse(sUnknown, sResultID, id);
		return intResponse(aResult.get_universe_max());
	}



	private static MQueryResult getResultObject(Integer id)
	{
		return envQueryResults.get(id);		  	
	}
	
	public static Document createPolicySet(String pname, String vname) 
	{
		// Create a new Policy Set with name pname, vocab vname...
		if(!envVocabularies.containsKey(vname))
			return errorResponse(sUnknown, sVocabulary, vname);
		if(envIDBCollections.containsKey(pname))
			return errorResponse(sUsed, sPolicy, vname);
		
		MVocab voc = envVocabularies.get(vname);
		MPolicySet pol = new MPolicySet(pname, voc);
		envIDBCollections.put(pname, pol);
		return successResponse();
	}

	static Document getRequestVector(String vname)
	{
		if(!envVocabularies.containsKey(vname))
			return errorResponse(sUnknown, sVocabulary, vname);
		MVocab voc = envVocabularies.get(vname);
		
		return stringResponse(voc.getExpectedRequestVarOrder());
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
		
		MVocab voc = new MVocab(vname);
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
			catch(MGException e)
			{
				return exceptionResponse(e);
			}
		}
		else
			return errorResponse(sNotExpected, sPolicy, pname);
	}

	public static Document loadXACML(String fname, String sfname)
	{
		//MPolicy pol = MPolicy.readXACML(fname, sfname);
	
		// TODO
		// Get name from where? Filename? Can't use internal name since its often duplicated in our tests
		
		return unsupportedResponse();
	}

	public static Document loadSQS(String fname)
	{
		// TODO Auto-generated method stub
		return unsupportedResponse();
	}

	public static Document addSubsort(String vname, String parent, String child)
	{
		if(!envVocabularies.containsKey(vname))
			return errorResponse(sUnknown, sVocabulary, vname);
		MVocab voc = envVocabularies.get(vname);
		try 
		{
			voc.addSubSort(parent, child);
			return successResponse();
		} 
		catch (MGException e)
		{
			return exceptionResponse(e);
		}
	}

	public static Document addSort(String vname, String sname) {
		if(!envVocabularies.containsKey(vname))
			return errorResponse(sUnknown, sVocabulary, vname);
		MVocab voc = envVocabularies.get(vname);
		try 
		{
			voc.addSort(sname);
			return successResponse();
		} 
		catch (MGException e)
		{
			return exceptionResponse(e);
		}
	}

	public static Document addConstraintAbstract(String vname, String s)
	{
		if(!envVocabularies.containsKey(vname))
			return errorResponse(sUnknown, sVocabulary, vname);
		MVocab voc = envVocabularies.get(vname);
		try 
		{
			voc.axioms.addConstraintAbstract(s);
			return successResponse();
		} 
		catch (MGException e)
		{
			return exceptionResponse(e);
		}
	}

	public static Document addConstraintAbstractAll(String vname, String s)
	{
		if(!envVocabularies.containsKey(vname))
			return errorResponse(sUnknown, sVocabulary, vname);
		MVocab voc = envVocabularies.get(vname);
		try 
		{
			voc.axioms.addConstraintAbstractAll(s);
			return successResponse();
		} 
		catch (MGException e)
		{
			return exceptionResponse(e);
		}
	}

	public static Document addConstraintTotalFunction(String vname, String s) 
	{
		if(!envVocabularies.containsKey(vname))
			return errorResponse(sUnknown, sVocabulary, vname);
		MVocab voc = envVocabularies.get(vname);
		try 
		{
			voc.axioms.addConstraintTotalFunction(s);
			return successResponse();
		} 
		catch (MGException e)
		{
			return exceptionResponse(e);
		}
	}

	public static Document addConstraintPartialFunction(String vname, String s) 
	{
		if(!envVocabularies.containsKey(vname))
			return errorResponse(sUnknown, sVocabulary, vname);
		MVocab voc = envVocabularies.get(vname);
		try 
		{
			voc.axioms.addConstraintPartialFunction(s);
			return successResponse();
		} 
		catch (MGException e)
		{
			return exceptionResponse(e);
		}
	}

	public static Document addConstraintSubset(String vname, String parent, String child)
	{
		if(!envVocabularies.containsKey(vname))
			return errorResponse(sUnknown, sVocabulary, vname);
		MVocab voc = envVocabularies.get(vname);
		try 
		{
			voc.axioms.addConstraintSubset(child, parent); // reverse of how it usually is?
			return successResponse();
		} 
		catch (MGException e)
		{
			return exceptionResponse(e);
		}
	}

	public static Document addConstraintNonemptyAll(String vname, String s)
	{
		if(!envVocabularies.containsKey(vname))
			return errorResponse(sUnknown, sVocabulary, vname);
		MVocab voc = envVocabularies.get(vname);
		try 
		{
			voc.axioms.addConstraintNonemptyAll(s);
			return successResponse();
		} 
		catch (MGException e)
		{
			return exceptionResponse(e);
		}
	}

	public static Document addConstraintNonempty(String vname, String s)
	{
		if(!envVocabularies.containsKey(vname))
			return errorResponse(sUnknown, sVocabulary, vname);
		MVocab voc = envVocabularies.get(vname);
		try 
		{
			voc.axioms.addConstraintNonempty(s);
			return successResponse();
		} 
		catch (MGException e)
		{
			return exceptionResponse(e);
		}
	}

	public static Document addConstraintAtMostOneAll(String vname, String s)
	{
		if(!envVocabularies.containsKey(vname))
			return errorResponse(sUnknown, sVocabulary, vname);
		MVocab voc = envVocabularies.get(vname);
		try 
		{
			voc.axioms.addConstraintAtMostOneAll(s);
			return successResponse();
		} 
		catch (MGException e)
		{
			return exceptionResponse(e);
		}
	}

	public static Document addConstraintAtMostOne(String vname, String s) 
	{
		if(!envVocabularies.containsKey(vname))
			return errorResponse(sUnknown, sVocabulary, vname);
		MVocab voc = envVocabularies.get(vname);
		try 
		{
			voc.axioms.addConstraintAtMostOne(s);
			return successResponse();
		} 
		catch (MGException e)
		{
			return exceptionResponse(e);
		}
	}

	public static Document addConstraintSingletonAll(String vname, String s)
	{
		if(!envVocabularies.containsKey(vname))
			return errorResponse(sUnknown, sVocabulary, vname);
		MVocab voc = envVocabularies.get(vname);
		try 
		{
			voc.axioms.addConstraintSingletonAll(s);
			return successResponse();
		} 
		catch (MGException e)
		{
			return exceptionResponse(e);
		}
	}

	public static Document addConstraintSingleton(String vname, String s)
	{
		if(!envVocabularies.containsKey(vname))
			return errorResponse(sUnknown, sVocabulary, vname);
		MVocab voc = envVocabularies.get(vname);
		try 
		{
			voc.axioms.addConstraintSingleton(s);
			return successResponse();
		} 
		catch (MGException e)
		{
			return exceptionResponse(e);
		}
	}

	public static Document addConstraintDisjointAll(String vname, String s)
	{
		MCommunicator.writeToLog("In addConstraintDisjointAll (env): "+s);
		if(!envVocabularies.containsKey(vname))
			return errorResponse(sUnknown, sVocabulary, vname);
		MVocab voc = envVocabularies.get(vname);
		try 
		{
			MCommunicator.writeToLog("Calling voc.axioms.add...: "+s);
			voc.axioms.addConstraintDisjointAll(s);
			MCommunicator.writeToLog("Called voc");
			return successResponse();
		} 
		catch (MGException e)
		{
			return exceptionResponse(e);
		}
	}

	public static Document addConstraintDisjoint(String vname, String s1, String s2)
	{
		if(!envVocabularies.containsKey(vname))
			return errorResponse(sUnknown, sVocabulary, vname);
		MVocab voc = envVocabularies.get(vname);
		try 
		{
			voc.axioms.addConstraintDisjoint(s1, s2);
			return successResponse();
		} 
		catch (MGException e)
		{
			return exceptionResponse(e);
		}
	}

	public static Document addDecision(String vname, String decname)
	{
		if(!envVocabularies.containsKey(vname))
			return errorResponse(sUnknown, sVocabulary, vname);
		MVocab voc = envVocabularies.get(vname);
		try 
		{
			voc.addDecision(decname);
			return successResponse();
		} 
		catch (MGException e)
		{
			return exceptionResponse(e);
		}
	}

	public static Document addOtherVariable(String vname, String varname, String domainsort)
	{
		if(!envVocabularies.containsKey(vname))
			return errorResponse(sUnknown, sVocabulary, vname);
		MVocab voc = envVocabularies.get(vname);
		try 
		{
			voc.addOtherVar(varname, domainsort);
			return successResponse();
		} 
		catch (MGException e)
		{
			return exceptionResponse(e);
		}
	}

	public static Document addRequestVariable(String vname, String varname, String domainsort)
	{
		if(!envVocabularies.containsKey(vname))
			return errorResponse(sUnknown, sVocabulary, vname);
		MVocab voc = envVocabularies.get(vname);
		try 
		{
			voc.addRequestVar(varname, domainsort);
			return successResponse();
		} 
		catch (MGException e)
		{
			return exceptionResponse(e);
		}
	}

	public static Document addPredicate(String vname, String sname, List<String> constr)
	{
		if(!envVocabularies.containsKey(vname))
			return errorResponse(sUnknown, sVocabulary, vname);
		MVocab voc = envVocabularies.get(vname);
		try 
		{
			String constructstr = foldConcatWithSpaces(constr);
			voc.addPredicate(sname, constructstr);
			return successResponse();
		} 
		catch (MGException e)
		{
			return exceptionResponse(e);
		}
	}

	public static Document addRule(String pname, String rname, String decision, List<String> cc)
	{
		if(!envIDBCollections.containsKey(pname))
			return errorResponse(sUnknown, sPolicy, pname);			
		MIDBCollection coll = envIDBCollections.get(pname);
		if(!(coll instanceof MPolicyLeaf))
			return errorResponse(sNotExpected, sPolicyLeaf, pname);
		MPolicyLeaf pol = (MPolicyLeaf) coll;
		try
		{
			pol.addRule(rname, decision, cc);
		}
		catch(MGException e)
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
		catch(MGException e)
		{
			return exceptionResponse(e);
		}
		return successResponse();
	}

	public static Document setRCombine(String pname, List<String> idl)
	{
		if(!envIDBCollections.containsKey(pname))
			return errorResponse(sUnknown, sPolicy, pname);;			
		MIDBCollection coll = envIDBCollections.get(pname);
		if(!(coll instanceof MPolicyLeaf))
			return errorResponse(sNotExpected, sPolicyLeaf, pname);
		MPolicyLeaf pol = (MPolicyLeaf) coll;
		pol.rCombine = foldConcatWithSpaces(idl);
		return successResponse();
	}

	public static Document setPCombine(String pname, List<String> idl) 
	{
		if(!envIDBCollections.containsKey(pname))
			return errorResponse(sUnknown, sPolicy, pname);;			
		MIDBCollection coll = envIDBCollections.get(pname);
		if(!(coll instanceof MPolicySet))
			return errorResponse(sNotExpected, sPolicySet, pname);
		MPolicySet pol = (MPolicySet) coll;
		pol.pCombine = foldConcatWithSpaces(idl);
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
		return listResponse(res);
	}

	public static Document getRulesIn(String pname, boolean b)
	{
		if(!envIDBCollections.containsKey(pname))
			return errorResponse(sUnknown, sPolicy, pname);;			
		MIDBCollection coll = envIDBCollections.get(pname);
		if(!(coll instanceof MPolicyLeaf))
			return errorResponse(sNotExpected, sPolicyLeaf, pname);
		MPolicyLeaf pol = (MPolicyLeaf) coll;
		
		List<String> res;
		if(!b)
			res = pol.getIDBNameList();
		else
			res = pol.getQualifiedIDBNameList();
		
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
		if(desc.length() > 0)
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

	
	static Document exceptionResponse(Exception e)
	{
		
		Document xmldoc = makeInitialResponse("exception");
		if(xmldoc == null) return null; // be safe (but bottle up exceptions)
		
		Element errorElement = xmldoc.createElementNS(null, "EXCEPTION");
		
		errorElement.setAttribute("class", e.getClass().getCanonicalName());
		errorElement.setAttribute("stack-trace", Arrays.toString(e.getStackTrace()));

		Element msgElement = xmldoc.createElementNS(null, "MESSAGE");
		if(e.getLocalizedMessage() != null && e.getLocalizedMessage().length() > 0 )
			msgElement.appendChild(xmldoc.createTextNode(e.getLocalizedMessage()));
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
		if(e instanceof MSemanticException)
		{
			MSemanticException ex = (MSemanticException) e;
			Element placeElement = xmldoc.createElementNS(null, "LOCATION");
			//placeElement.setAttribute("row", String.valueOf(ex.row));
			//placeElement.setAttribute("col", String.valueOf(ex.col));
			placeElement.setAttribute("problem", String.valueOf(ex.problem));
			
			//if(String.valueOf(ex.errorValue).length() > 0)
			//	placeElement.appendChild(xmldoc.createTextNode(String.valueOf(ex.errorValue)));
			errorElement.appendChild(placeElement);
		}
		
		// Include the query that caused the problem.
		Element queryElement = xmldoc.createElementNS(null, "COMMAND");
		if(lastCommandReceived.length() > 0)
			queryElement.appendChild(xmldoc.createTextNode(lastCommandReceived));
		errorElement.appendChild(queryElement);
		
		xmldoc.getDocumentElement().appendChild(errorElement);
		
		return xmldoc;
	}
	
	
	private static Document noSolutionResponse(MQueryResult theResult, Integer id)
	{
		Document xmldoc = makeInitialResponse(sUnsat);
		if(xmldoc == null) return null; // be safe (but bottle up exceptions)
				
		Element statsElement = makeStatisticsElement(xmldoc, theResult, id);		
		xmldoc.getDocumentElement().appendChild(statsElement);								
		return xmldoc;
	}

	private static Element makeStatisticsElement(Document xmldoc, MQueryResult theResult, Integer id)
	{
		Element statsElement = xmldoc.createElementNS(null, "STATISTICS");;		
		
		// Report the size ceiling (calculated and user-provided) so a warning
		// can be given if need be.
		statsElement.setAttribute("max-size", String.valueOf(theResult.maxSize));
		statsElement.setAttribute("user-max-size", String.valueOf(theResult.forQuery.sizeCeiling));
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
	
	private static Document boolResponseWithStats(MQueryResult aResult, int id, boolean b)
	{
		Document xmldoc = makeInitialResponse("boolean");
		if(xmldoc == null) return null; // be safe (but bottle up exceptions)
		
		Text val = xmldoc.createTextNode(String.valueOf(b));
		xmldoc.getDocumentElement().appendChild(val);
		xmldoc.getDocumentElement().appendChild(makeStatisticsElement(xmldoc, aResult, id));
		return xmldoc;
	}
	
	private static Document boolResponse(boolean b)
	{		
		Document xmldoc = makeInitialResponse("boolean");
		if(xmldoc == null) return null; // be safe (but bottle up exceptions)
		
		Text val = xmldoc.createTextNode(String.valueOf(b));
		xmldoc.getDocumentElement().appendChild(val);
		return xmldoc;
	}
	private static Document intResponse(int theInt)
	{
		return stringResponse(String.valueOf(theInt));
	}
	
	private static Document stringResponse(String str)
	{
		// <MARGRAVE-RESPONSE>Hello</MARGRAVE-RESPONSE>
		
		Document xmldoc = makeInitialResponse("string");
		if(xmldoc == null) return null; // be safe (but bottle up exceptions)
		if(str.length() > 0)
		{
			// Empty text nodes WILL cause problems in XML transformation
			Text val = xmldoc.createTextNode(str);
			xmldoc.getDocumentElement().appendChild(val);
		}
		return xmldoc;
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
			MSolutionInstance nextPreTup, int id)
	{
		MSolutionInstance next;
		
		// If this query was tupled, don't forget to convert back to the original signature.		
		if(mQueryResult.forQuery.tupled) 
		{
			next = mQueryResult.forQuery.internalTupledQuery.processTupledSolutionForThis(nextPreTup);
			MCommunicator.writeToLog("scenarioResponse: query was TUPLED.");
			MCommunicator.writeToLog("Annotations:");
			MCommunicator.writeToLog(next.getAnnotations().toString());
		}
		else
		{
			next = nextPreTup;
			MCommunicator.writeToLog("scenarioResponse: query was NOT tupled.");
			MCommunicator.writeToLog("just-in-case, tupling = "+mQueryResult.forQuery.doTupling);
			MCommunicator.writeToLog("just-in-case, internal = "+mQueryResult.forQuery.internalTupledQuery);
		}
						
		Document xmldoc = makeInitialResponse("model");
		if(xmldoc == null) return null; // be safe (but bottle up exceptions)		
		Element modelElement = xmldoc.createElementNS(null, "MODEL");
		
		Instance facts = next.getFacts();
		List<String> annotations = next.getAnnotations();
		
		modelElement.setAttribute("size", String.valueOf(facts.universe().size()));
				
		// For each relation, what's in it?
		for(Relation r : facts.relations())
		{
			Element relationElement = xmldoc.createElementNS(null, "RELATION");
			relationElement.setAttribute("name", r.name());
			relationElement.setAttribute("arity", String.valueOf(r.arity()));
			
			for(Tuple t : facts.relationTuples().get(r))
			{
				Element tupleElement = xmldoc.createElementNS(null, "TUPLE");
								
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
			
			// TODO potential info loss in the toString call here. 
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
			
			// TODO potential info loss in the toString call here. 
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
			
			// TODO some info lost in the toString calls here.
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

}

