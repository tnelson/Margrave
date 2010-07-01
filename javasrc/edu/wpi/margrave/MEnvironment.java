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

import kodkod.ast.*;
import kodkod.instance.Instance;
import kodkod.instance.Tuple;

import java_cup.runtime.Symbol;


class MLexerException extends IOException
{
	int into;
	int row;
	int col;
	String at;
	
	MLexerException(String msg, int col, int line, int into, String at)
	{
		super(msg);
		this.row = line;
		this.col = col;
		this.into = into;
		this.at = at;
	}
}

class MParserException extends IOException
{	
	int row;
	int col;
	Object errorValue;
		
	MParserException(int row, int col, Object val)
	{
		super("Margrave encountered an error while parsing...");
		this.row = row;
		this.col = col;
		errorValue = val;
	}
}

class MSemanticException extends IOException
{	
	int row = -1;
	int col = -1;
	Object errorValue = null;
	String problem;

	MSemanticException(String problem)
	{
		this.problem = problem;
	}
	
	MSemanticException(String problem, int row, int col, Object val)
	{
		super("Margrave could not understand...");
		this.row = row;
		this.col = col;
		errorValue = val;
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

class MQueryOutputSpec
{
	public enum DefaultOutputType {outAll, outOne, outSat, outSilent, outPopulated, outUnpopulated };
	public enum DefaultIteratorType {outIterateTotal, outIteratePartial};
	
	public Map<String, Set<List<String>>> populatedCandidates = new HashMap<String, Set<List<String>>>();
	
	// FOR CASES is _disjunctive_. Candidates will be evaluated with respect to each separately.
	// In future, may allow a richer set of atom cases. (Negation, conjunction)
	public Map<String, Set<List<String>>> forCasesOr = new HashMap<String, Set<List<String>>>();
	
	
	public DefaultOutputType otDefault = DefaultOutputType.outAll;
	public DefaultIteratorType itDefault = DefaultIteratorType.outIterateTotal;	
	
	MQueryOutputSpec()
	{
		
	}
	
	MQueryOutputSpec(String arg)
	{
		if("all".equals(arg))
		{
			otDefault = DefaultOutputType.outAll;
			itDefault = DefaultIteratorType.outIterateTotal;			
		}
		else if("one".equals(arg))
		{
			otDefault = DefaultOutputType.outOne;
			itDefault = DefaultIteratorType.outIterateTotal;	
		}
		else if("collapse one".equals(arg))
		{
			otDefault = DefaultOutputType.outOne;
			itDefault = DefaultIteratorType.outIteratePartial;
		}
		else if("collapse all".equals(arg))
		{
			otDefault = DefaultOutputType.outAll;
			itDefault = DefaultIteratorType.outIteratePartial;

		}
		else if("populated".equals(arg))
		{
			otDefault = DefaultOutputType.outPopulated;
		}
		else if("unpopulated".equals(arg))
		{
			otDefault = DefaultOutputType.outUnpopulated;			
		}
		else if("?".equals(arg))
		{
			otDefault = DefaultOutputType.outSat;			
		}
		else
		{
			otDefault = DefaultOutputType.outAll;
			itDefault = DefaultIteratorType.outIterateTotal;	
		}

	}
	
	public String toString()
	{
		if(otDefault == DefaultOutputType.outSilent)
			return " SILENT ";
		if(otDefault == DefaultOutputType.outSat)
			return " IS POSSIBLE? ";
		
		
		// Don't need this yet
		/*if(otDefault == DefaultOutputType.outPopulated)
			return " POPULATED ";
		if(otDefault == DefaultOutputType.outUnpopulated)
			return " UNPOPULATED ";*/
		
		
		if(otDefault == DefaultOutputType.outAll && itDefault == DefaultIteratorType.outIteratePartial)
			return " SHOW ALL COLLAPSE ";
		if(otDefault == DefaultOutputType.outAll && itDefault == DefaultIteratorType.outIterateTotal)
			return " SHOW ALL";
		if(otDefault == DefaultOutputType.outOne && itDefault == DefaultIteratorType.outIteratePartial)
			return " SHOW ONE COLLAPSE ";
		if(otDefault == DefaultOutputType.outOne && itDefault == DefaultIteratorType.outIteratePartial)
			return " SHOW ONE ";
		
		return " SHOW ALL ";
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
	
	protected static PrintStream errorStream = System.err;
	protected static PrintStream outStream = System.out;
	
	public static boolean debugParser = false;
	
	public static String eol = System.getProperty("line.separator");
	public static String sUnknownIdMessage = "Unknown result ID.";
	public static String sNoIterator = "Cannot get NEXT model before getting a first ONE.";
	public static String sFalse = String.valueOf(false);
	public static String sTrue = String.valueOf(true);
	public static String sUnknownVocabMessage = "Unknown vocabulary.";		
	public static String sIdentifierUsedMessage = "Identifier was already in use.";
	public static String sUnknownPolicyMessage = "Unknown policy.";
	public static String sNotAPolicyLeafMessage = "Policy was not a leaf.";
	public static String sNotAPolicySetMessage = "Policy was not a set.";
	public static String sNotAPolicyMessage = "Not a policy identifier.";
	public static String sUnsupportedCommand = "Command is not yet supported.";
	public static String sNoMoreSolutions = sFalse;
	
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
	
	static public String commandSilent(String cmd)
	{
		// Don't print anything
		return command(cmd, true);
	}
	
	static public String command(String cmd)
	{
		// Default to non-silent
		return command(cmd, false);
	}
	
	static void printOut(Object out, boolean silent)
	{
		if(!silent)
			outStream.println(out.toString());
	}
	
	static String getNextModel(int numResult)
	throws MGEUnknownIdentifier, MGEUnsortedVariable, MGEManagerException, MGEBadIdentifierName
	{
		// Do we have a result for this num?
		if(!envQueryResults.containsKey(numResult))
			return "";
		
		if(!envIterators.containsKey(numResult))
			return getFirstModel(numResult);
		else
		{
			// Return next model in the iterator.
			try
			{
				return convertSolutionToSexp(envQueryResults.get(numResult), envIterators.get(numResult).next());
			}
			catch(MGENoMoreSolutions e)
			{
				return sFalse;
			}
		}		
	}
				
	static String getFirstModel(int numResult) 
	throws MGEUnknownIdentifier, MGEUnsortedVariable, MGEManagerException, MGEBadIdentifierName
	{
		// Do we have a result for this num?
		if(!envQueryResults.containsKey(numResult))
			return "";

		// Reset the iterator (or create it if new)
		MInstanceIterator it = envQueryResults.get(numResult).getTotalIterator();
		envIterators.put(numResult, it);
		
		try
		{
			return convertSolutionToSexp(envQueryResults.get(numResult), it.next());
		}
		catch(MGENoMoreSolutions e)
		{
			return sFalse;
		}
		
	}
	
	static public String command(String cmd, boolean silent)
	{
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
					return "";
				}
				
				// Compile the query and get the result handle.
				MQueryResult qryResult = qry.runQuery();
				
				// TODO Don't store more than one for now.
				envQueryResults.put(0, qryResult);
				return "0";
												
			}  // end: a query
			
			// Else, it's an INFO command etc.
			else
			{
				String resStr = result.value.toString();
				//System.err.println(resStr);
				
				return resStr;
			}

		}
		catch(MParserException e)
		{
			// TODO When real repl, SHOW the error with underlining
			perror("Error when parsing command. Row: "+e.row+", Column: "+e.col);
			return e.getLocalizedMessage();			
		}
		catch(MLexerException e)
		{
			// TODO When real repl, SHOW the error with underlining
			perror("Error when lexing command. Row: "+e.row+", Column: "+e.col+" At: "+e.at);		
			return e.getLocalizedMessage();
		}		
		catch(MSemanticException e)
		{
			// TODO When real repl, SHOW the error with underlining
			if(e.errorValue != null)
				perror(e.problem+" at Row: "+e.row+", Column: "+e.col+". This was around: "+((Symbol)e.errorValue).value);
			else
			{
				// MSemanticException can also be used to indicate a problem without including a row/col.
				perror(e.problem);
			}
			return e.getLocalizedMessage();
		}
		catch(Exception e)
		{
			// Unexpected! Bad exception
			perror("Unexpected exception when reading command: "+e.getClass()+"\n------------------------------\n");			
			e.printStackTrace(errorStream);
			perror("------------------------------");
			return e.getLocalizedMessage();
		}
		
	}
	
	static public String printInfo(String id)
	{
		MIDBCollection coll = getPolicyOrView(id);
		if(coll != null)
			return coll.getInfo();
		else
		{
			MVocab voc = envVocabularies.get(id);
			if(voc != null)
				return voc.getInfo();
		}
		
		return "Unknown identifier";
	}
	
	
	// Returns true if overwriting existing identifier
	// Assume that the policy is already loaded FOR NOW. 
	// Later we will add a LOAD POLICY keyword.
	static public boolean savePolicyAs(String ident, MPolicy pol)
	{
		ident = ident.toLowerCase();
		
		if("last".equals(ident))
		{						
			errorStream.println("The keyword LAST is reserved. Could not load policy and name it `LAST'.");
			return false;
		}
				
		return envIDBCollections.put(ident, pol) != null;			
	}
	
	static public String renameIDBCollection(String identold, String identnew)
	{
		identold = identold.toLowerCase();
		identnew = identnew.toLowerCase();
		
		if("last".equals(identnew))
		{						
			perror("The keyword LAST is reserved. Could not rename to `LAST'.");
			return "ERROR: The keyword LAST is reserved. Could not rename to `LAST'.";
		}
		
		if(!envIDBCollections.containsKey(identold))
		{
			perror("Could not find a collection with identifier: "+identold);
			return "ERROR: Could not find a collection with identifier: "+identold;		
		}
		
		
		boolean overwrote = envIDBCollections.put(identnew, envIDBCollections.get(identold)) != null;
		envIDBCollections.remove(identold);
		return String.valueOf(overwrote);		
	}
	
	static public Object doCompare(String pol1, String pol2, Map<String, Set<List<String>>> idbOutputMap,
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
		theCmd += "PUBLISH "+reqvars;							
		
		if(tupling)
			theCmd += " TUPLING ";
		
		if(debuglevel > 0)
			theCmd += " DEBUG "+debuglevel+" ";
		
		theCmd += "CEILING "+sizeceiling+" ";

		//System.out.println("---- ");
		//System.out.println(thePol1.varOrdering);
		//System.out.println(theCmd);
				
		return command(theCmd);		
	}
	
	
	static protected void setLast(MQuery qry)
	{
		envIDBCollections.put("last", qry);
	}
	
	
	static private void perror(String msg)
	{
		errorStream.println("\nMARGRAVE ERROR: \n"+msg+"\n");
		
		outStream.flush();
		if(outStream != errorStream)
			errorStream.flush();	
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
	
	static public String convertSetToSexp(Set<?> theSet)
	{
		// Gives a warning, but won't let me instantiate ArrayList<?> (which is only to be expected)
		return convertListToSexp(new ArrayList(theSet));
	}
	
	static public String convertListToSexp(List<?> theList)
	{		
		StringBuffer buf = new StringBuffer();		
		buf.append("( ");
				
		for(Object o : theList)
		{			
			buf.append(o.toString());		
			buf.append(" ");
		}
		
		buf.append(")");
		return buf.toString();
	}
	
	static public String convertMapToSexp(Map<?, ?> theMap)
	{
		StringBuffer buf = new StringBuffer();		
		buf.append("( ");
				
		for(Object o : theMap.keySet())
		{
			Object v = theMap.get(o);
			
			buf.append(" ( ");
			buf.append(o.toString());
			buf.append(" ");
			
			if(v instanceof List<?>)
				buf.append(convertListToSexp((List<?>)v));
			else
				buf.append(v.toString());
			
			buf.append(" ) ");
		}
		
		buf.append(")");
		return buf.toString();
	}
	
	public static String printSystemInfo()
	{		
		StringBuffer theResult = new StringBuffer();
		theResult.append(MFormulaManager.getStatisticsString());
		
		theResult.append(eol+eol);
		theResult.append("Number of vocabularies: "+envVocabularies.size()); theResult.append(eol);
		theResult.append("Number of IDB Collections (policies + saved queries): "+envIDBCollections.size()); theResult.append(eol);
		theResult.append("Number of cached query results: "+envQueryResults.size()); theResult.append(eol);
		return theResult.toString();
	}
	
	static public String convertSolutionToSexp(MQueryResult forResult, MSolutionInstance sol)
	{
		StringBuffer result = new StringBuffer();
		
		result.append("(model ");
		Instance facts = sol.getFacts();
		result.append(facts.universe().size());
		result.append(" ");
		
		// For each relation, what's in it?
		for(Relation r : facts.relations())
		{
			result.append("("+r.name()+" "+r.arity()+" (");
			
			for(Tuple t : facts.relationTuples().get(r))
			{
				result.append(t.toString());			
			}
			
			result.append(")) ");
		}
		
		result.append(")");
		return result.toString();
	}

	public static String showNextModel(Integer id)
	{
		MQueryResult aResult = getResultObject(id);
		MInstanceIterator anIterator = getIteratorFor(id);
		if(aResult == null)
			return sUnknownIdMessage;
		if(anIterator == null)
			return sNoIterator;
		
		try
		{
			MSolutionInstance sol = anIterator.next();
			String str = aResult.getPrettySolution(sol, anIterator);
			return str;
		}
		catch(MGENoMoreSolutions e)
		{
			return sNoMoreSolutions;
		}

	}

	public static String getNextModel(String id) 
	{
		// TODO Auto-generated method stub
		return sUnsupportedCommand;
	}

	public static String getFirstModel(String id)
	{
		
		// TODO only SHOW for now 
		return sUnsupportedCommand;
		
	}
	
	public static String showFirstModel(Integer id)
	{
		MQueryResult aResult = getResultObject(id);		
		if(aResult == null)
			return sUnknownIdMessage;
		
		try
		{	
			// create new iterator
			MInstanceIterator anIterator = aResult.getTotalIterator();
			
			// store it 			
			envIterators.put(Integer.valueOf(id), anIterator);
			
			// return the first model
			MSolutionInstance sol = anIterator.next();
			String str = aResult.getPrettySolution(sol, anIterator);
			return str;
		}
		catch(MGENoMoreSolutions e)
		{
			return sNoMoreSolutions;
		}
		catch(MGException e)
		{
			return e.getLocalizedMessage();
		}
		 
	}

	protected static MInstanceIterator getIteratorFor(Integer id)
	{
		return envIterators.get(id);
	}
	
	public static String showPopulated(Integer id,
			Map<String, Set<List<String>>> rlist,
			Map<String, Set<List<String>>> clist) 
	{
		MQueryResult aResult = getResultObject(id);
		if(aResult == null)
			return sUnknownIdMessage;
				
		Map<String, Set<String>> outsets;
		try
		{
			outsets = aResult.getPopulatedRelationFinder().getPopulatedRelations(rlist, clist);
			
			if(outsets.size() == 1 && outsets.containsKey(""))				
				return convertSetToSexp(outsets.get(""));
			return convertMapToSexp(outsets);
			
		}
		catch (MGException e)
		{
			return e.getLocalizedMessage();
		}
	}

	public static String showNextCollapse(Integer id) 
	{
		// TODO No support for partial models yet
		return sUnsupportedCommand;
	}

	public static String showPopulated(Integer id, Map<String, Set<List<String>>> rlist)
	{
		return showPopulated(id, rlist,  new HashMap<String, Set<List<String>>>());
	}
	
	public static String showUnpopulated(Integer id,
			Map<String, Set<List<String>>> rlist,
			Map<String, Set<List<String>>> clist)
	{
		MQueryResult aResult = getResultObject(id);
		if(aResult == null)
			return sUnknownIdMessage;
				
		Map<String, Set<String>> outsets;
		try
		{
			outsets = aResult.getPopulatedRelationFinder().getUnpopulatedRelations(rlist, clist);
			
			if(outsets.size() == 1 && outsets.containsKey(""))				
				return convertSetToSexp(outsets.get(""));
			return convertMapToSexp(outsets);
			
		}
		catch (MGException e)
		{
			return e.getLocalizedMessage();
		}
	}
	
	public static String showUnpopulated(Integer id, Map<String, Set<List<String>>> rlist) 
	{
		return showUnpopulated(id, rlist,  new HashMap<String, Set<List<String>>>());
	}
	
	public static String countModels(Integer id, Integer n)
	{
		MQueryResult aResult = getResultObject(id);
		if(aResult == null)
			return sUnknownIdMessage;
		return String.valueOf(aResult.countModelsAtSize(n));				
	}

	public static String countModels(Integer id)
	{
		return countModels(id, -1); // -1 for overall total to ceiling
	}

	public static String isGuar(Integer id)
	{
		// Is this solution complete? (Is the ceiling high enough?)
		MQueryResult aResult = getResultObject(id);
		if(aResult == null)
			return sUnknownIdMessage;
		if(aResult.get_hu_ceiling() > aResult.get_universe_max())
			return sFalse;
		return sTrue;
	}

	public static String isPoss(Integer id) 
	{	
		MQueryResult aResult = getResultObject(id);
		if(aResult == null)
			return sUnknownIdMessage;
		try
		{
			return String.valueOf(aResult.isSatisfiable());
		}
		catch(MGException e)
		{
			return e.getLocalizedMessage();
		}
	}

	public static String showCeiling(Integer id)
	{
		MQueryResult aResult = getResultObject(id);
		if(aResult == null)
			return sUnknownIdMessage;
		return String.valueOf(aResult.get_universe_max());
	}

	private static MQueryResult getResultObject(Integer id)
	{
		return envQueryResults.get(id);		  	
	}
	
	public static String createPolicySet(String pname, String vname) 
	{
		// Create a new Policy Set with name pname, vocab vname...
		if(!envVocabularies.containsKey(vname))
			return sUnknownVocabMessage;
		if(envIDBCollections.containsKey(pname))
			return sIdentifierUsedMessage;
		
		MVocab voc = envVocabularies.get(vname);
		MPolicySet pol = new MPolicySet(pname, voc);
		envIDBCollections.put(pname, pol);
		return sTrue;
	}

	static String getRequestVector(String vname)
	{
		if(!envVocabularies.containsKey(vname))
			return sUnknownVocabMessage;
		MVocab voc = envVocabularies.get(vname);
		
		return voc.getExpectedRequestVarOrder();
	}

	
	public static String createPolicyLeaf(String pname, String vname)
	{
		if(!envVocabularies.containsKey(vname))
			return sUnknownVocabMessage;
		if(envIDBCollections.containsKey(pname))
			return sIdentifierUsedMessage;
		
		MVocab voc = envVocabularies.get(vname);
		MPolicyLeaf pol = new MPolicyLeaf(pname, voc);
		envIDBCollections.put(pname, pol);
		return sTrue;
	}

	public static String createVocabulary(String vname)
	{
		if(envVocabularies.containsKey(vname))
			return sIdentifierUsedMessage;
		
		MVocab voc = new MVocab(vname);
		envVocabularies.put(vname, voc);
		return sTrue;
	}

	public static String preparePolicy(String pname)
	{
		if(!envIDBCollections.containsKey(pname))
			return sUnknownPolicyMessage;
		MIDBCollection pol = envIDBCollections.get(pname);
		if(pol instanceof MPolicy)
		{
			try
			{
				((MPolicy)pol).initIDBs();
				return sTrue;
			}
			catch(MGException e)
			{
				return e.getLocalizedMessage();
			}
		}
		else
			return sNotAPolicyMessage;
	}

	public static String loadXACML(String fname, String sfname)
	{
		//MPolicy pol = MPolicy.readXACML(fname, sfname);
	
		// TODO
		// Get name from where? Filename? Can't use internal name since its often duplicated in our tests
		
		return sUnsupportedCommand;
	}

	public static String loadSQS(String fname) {
		// TODO Auto-generated method stub
		return sUnsupportedCommand;
	}

	public static String addSubsort(String vname, String parent, String child)
	{
		if(!envVocabularies.containsKey(vname))
			return sUnknownVocabMessage;
		MVocab voc = envVocabularies.get(vname);
		try 
		{
			voc.addSubSort(parent, child);
			return sTrue;
		} 
		catch (MGException e)
		{
			return e.getLocalizedMessage();
		}
	}

	public static String addSort(String vname, String sname) {
		if(!envVocabularies.containsKey(vname))
			return sUnknownVocabMessage;
		MVocab voc = envVocabularies.get(vname);
		try 
		{
			voc.addSort(sname);
			return sTrue;
		} 
		catch (MGException e)
		{
			return e.getLocalizedMessage();
		}
	}

	public static String addConstraintAbstract(String vname, String s)
	{
		if(!envVocabularies.containsKey(vname))
			return sUnknownVocabMessage;
		MVocab voc = envVocabularies.get(vname);
		try 
		{
			voc.axioms.addConstraintAbstract(s);
			return sTrue;
		} 
		catch (MGException e)
		{
			return e.getLocalizedMessage();
		}
	}

	public static String addConstraintAbstractAll(String vname, String s) 
	{
		if(!envVocabularies.containsKey(vname))
			return sUnknownVocabMessage;
		MVocab voc = envVocabularies.get(vname);
		try 
		{
			voc.axioms.addConstraintAbstractAll(s);
			return sTrue;
		} 
		catch (MGException e)
		{
			return e.getLocalizedMessage();
		}
	}

	public static String addConstraintTotalFunction(String vname, String s) 
	{
		if(!envVocabularies.containsKey(vname))
			return sUnknownVocabMessage;
		MVocab voc = envVocabularies.get(vname);
		try 
		{
			voc.axioms.addConstraintTotalFunction(s);
			return sTrue;
		} 
		catch (MGException e)
		{
			return e.getLocalizedMessage();
		}
	}

	public static String addConstraintPartialFunction(String vname, String s) 
	{
		if(!envVocabularies.containsKey(vname))
			return sUnknownVocabMessage;
		MVocab voc = envVocabularies.get(vname);
		try 
		{
			voc.axioms.addConstraintPartialFunction(s);
			return sTrue;
		} 
		catch (MGException e)
		{
			return e.getLocalizedMessage();
		}
	}

	public static String addConstraintSubset(String vname, String parent, String child)
	{
		if(!envVocabularies.containsKey(vname))
			return sUnknownVocabMessage;
		MVocab voc = envVocabularies.get(vname);
		try 
		{
			voc.axioms.addConstraintSubset(child, parent); // reverse of how it usually is?
			return sTrue;
		} 
		catch (MGException e)
		{
			return e.getLocalizedMessage();
		}
	}

	public static String addConstraintNonemptyAll(String vname, String s)
	{
		if(!envVocabularies.containsKey(vname))
			return sUnknownVocabMessage;
		MVocab voc = envVocabularies.get(vname);
		try 
		{
			voc.axioms.addConstraintNonemptyAll(s);
			return sTrue;
		} 
		catch (MGException e)
		{
			return e.getLocalizedMessage();
		}
	}

	public static String addConstraintNonempty(String vname, String s)
	{
		if(!envVocabularies.containsKey(vname))
			return sUnknownVocabMessage;
		MVocab voc = envVocabularies.get(vname);
		try 
		{
			voc.axioms.addConstraintNonempty(s);
			return sTrue;
		} 
		catch (MGException e)
		{
			return e.getLocalizedMessage();
		}
	}

	public static String addConstraintAtMostOneAll(String vname, String s)
	{
		if(!envVocabularies.containsKey(vname))
			return sUnknownVocabMessage;
		MVocab voc = envVocabularies.get(vname);
		try 
		{
			voc.axioms.addConstraintAtMostOneAll(s);
			return sTrue;
		} 
		catch (MGException e)
		{
			return e.getLocalizedMessage();
		}
	}

	public static String addConstraintAtMostOne(String vname, String s) 
	{
		if(!envVocabularies.containsKey(vname))
			return sUnknownVocabMessage;
		MVocab voc = envVocabularies.get(vname);
		try 
		{
			voc.axioms.addConstraintAtMostOne(s);
			return sTrue;
		} 
		catch (MGException e)
		{
			return e.getLocalizedMessage();
		}
	}

	public static String addConstraintSingletonAll(String vname, String s)
	{
		if(!envVocabularies.containsKey(vname))
			return sUnknownVocabMessage;
		MVocab voc = envVocabularies.get(vname);
		try 
		{
			voc.axioms.addConstraintSingletonAll(s);
			return sTrue;
		} 
		catch (MGException e)
		{
			return e.getLocalizedMessage();
		}
	}

	public static String addConstraintSingleton(String vname, String s)
	{
		if(!envVocabularies.containsKey(vname))
			return sUnknownVocabMessage;
		MVocab voc = envVocabularies.get(vname);
		try 
		{
			voc.axioms.addConstraintSingleton(s);
			return sTrue;
		} 
		catch (MGException e)
		{
			return e.getLocalizedMessage();
		}
	}

	public static String addConstraintDisjointAll(String vname, String s)
	{
		if(!envVocabularies.containsKey(vname))
			return sUnknownVocabMessage;
		MVocab voc = envVocabularies.get(vname);
		try 
		{
			voc.axioms.addConstraintDisjointAll(s);
			return sTrue;
		} 
		catch (MGException e)
		{
			return e.getLocalizedMessage();
		}
	}

	public static String addConstraintDisjoint(String vname, String s1, String s2)
	{
		if(!envVocabularies.containsKey(vname))
			return sUnknownVocabMessage;
		MVocab voc = envVocabularies.get(vname);
		try 
		{
			voc.axioms.addConstraintDisjoint(s1, s2);
			return sTrue;
		} 
		catch (MGException e)
		{
			return e.getLocalizedMessage();
		}
	}

	public static String addDecision(String vname, String decname)
	{
		if(!envVocabularies.containsKey(vname))
			return sUnknownVocabMessage;
		MVocab voc = envVocabularies.get(vname);
		try 
		{
			voc.addDecision(decname);
			return sTrue;
		} 
		catch (MGException e)
		{
			return e.getLocalizedMessage();
		}
	}

	public static String addOtherVariable(String vname, String varname, String domainsort)
	{
		if(!envVocabularies.containsKey(vname))
			return sUnknownVocabMessage;
		MVocab voc = envVocabularies.get(vname);
		try 
		{
			voc.addOtherVar(varname, domainsort);
			return sTrue;
		} 
		catch (MGException e)
		{
			return e.getLocalizedMessage();
		}
	}

	public static String addRequestVariable(String vname, String varname, String domainsort)
	{
		if(!envVocabularies.containsKey(vname))
			return sUnknownVocabMessage;
		MVocab voc = envVocabularies.get(vname);
		try 
		{
			voc.addRequestVar(varname, domainsort);
			return sTrue;
		} 
		catch (MGException e)
		{
			return e.getLocalizedMessage();
		}
	}

	public static String addPredicate(String vname, String sname, List<String> constr)
	{
		if(!envVocabularies.containsKey(vname))
			return sUnknownVocabMessage;
		MVocab voc = envVocabularies.get(vname);
		try 
		{
			String constructstr = foldConcatWithSpaces(constr);
			voc.addPredicate(sname, constructstr);
			return sTrue;
		} 
		catch (MGException e)
		{
			return e.getLocalizedMessage();
		}
	}


	public static String addRule(String pname, String rname, String decision, List<String> cc)
	{
		if(!envIDBCollections.containsKey(pname))
			return sUnknownPolicyMessage;;			
		MIDBCollection coll = envIDBCollections.get(pname);
		if(!(coll instanceof MPolicyLeaf))
			return sNotAPolicyLeafMessage;
		MPolicyLeaf pol = (MPolicyLeaf) coll;
		try
		{
			pol.addRule(rname, decision, cc);
		}
		catch(MGException e)
		{
			return e.getLocalizedMessage();
		}
		return sTrue;
	}
	
	public static String setPolicyTarget(String pname, List<String> cc)
	{
		if(!envIDBCollections.containsKey(pname))
			return sUnknownPolicyMessage;;			
		MIDBCollection coll = envIDBCollections.get(pname);
		if(!(coll instanceof MPolicy))
			return sNotAPolicyMessage;
		MPolicy pol = (MPolicy) coll;
		try
		{
			pol.setTarget(cc);
		}
		catch(MGException e)
		{
			return e.getLocalizedMessage();
		}
		return sTrue;
	}

	public static String setRCombine(String pname, List<String> idl)
	{
		if(!envIDBCollections.containsKey(pname))
			return sUnknownPolicyMessage;;			
		MIDBCollection coll = envIDBCollections.get(pname);
		if(!(coll instanceof MPolicyLeaf))
			return sNotAPolicyLeafMessage;
		MPolicyLeaf pol = (MPolicyLeaf) coll;
		pol.rCombine = foldConcatWithSpaces(idl);
		return sTrue;
	}

	public static String setPCombine(String pname, List<String> idl) 
	{
		if(!envIDBCollections.containsKey(pname))
			return sUnknownPolicyMessage;;			
		MIDBCollection coll = envIDBCollections.get(pname);
		if(!(coll instanceof MPolicySet))
			return sNotAPolicySetMessage;
		MPolicySet pol = (MPolicySet) coll;
		pol.pCombine = foldConcatWithSpaces(idl);
		return sTrue;
	}

	public static String getDecisionFor(String pname, String rname)
	{
		if(!envIDBCollections.containsKey(pname))
			return sUnknownPolicyMessage;;			
		MIDBCollection coll = envIDBCollections.get(pname);
		if(!(coll instanceof MPolicyLeaf))
			return sNotAPolicyLeafMessage;
		MPolicyLeaf pol = (MPolicyLeaf) coll;
		
		return pol.getDecisionForRuleIDBName(rname);		
	}

	public static String getHigherPriorityThan(String pname, String rname)
	{
		if(!envIDBCollections.containsKey(pname))
			return sUnknownPolicyMessage;;			
		MIDBCollection coll = envIDBCollections.get(pname);
		if(!(coll instanceof MPolicyLeaf))
			return sNotAPolicyLeafMessage;
		MPolicyLeaf pol = (MPolicyLeaf) coll;
		
		List<String> res = pol.ruleIDBsWithHigherPriorityThan(rname);
		return convertListToSexp(res);
	}

	public static String getRulesIn(String pname, boolean b)
	{
		if(!envIDBCollections.containsKey(pname))
			return sUnknownPolicyMessage;;			
		MIDBCollection coll = envIDBCollections.get(pname);
		if(!(coll instanceof MPolicyLeaf))
			return sNotAPolicyLeafMessage;
		MPolicyLeaf pol = (MPolicyLeaf) coll;
		
		List<String> res;
		if(!b)
			res = pol.getIDBNameList();
		else
			res = pol.getQualifiedIDBNameList();
		
		return convertListToSexp(res);
	}

	public static String addChild(String parent, String child)
	{
		MIDBCollection pcoll = envIDBCollections.get(parent);
		MIDBCollection ccoll = envIDBCollections.get(child);
		if(pcoll == null || ccoll == null)
			return sUnknownPolicyMessage;
		if(!(pcoll instanceof MPolicySet) || !(ccoll instanceof MPolicy))
			return sNotAPolicySetMessage;
		
		MPolicySet polparent = (MPolicySet) pcoll;
		MPolicy polchild = (MPolicy) ccoll;
		polparent.addChild(polchild);			
		return sTrue;
	}
	
}
