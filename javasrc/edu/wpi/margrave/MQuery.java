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

import java.lang.management.*;
import kodkod.ast.*;
import kodkod.engine.satlab.SATFactory;
import kodkod.instance.*;
import java.util.*;

/**
 * The MGQuery class encapsulates a query against a policy or policies. It
 * contains the KodKod formula which is being tested, as well as the quantifiers
 * for that formula. The quantifiers are kept separate in order to facilitate
 * easy composition of queries.
 *
 * @author Tim Nelson
 * @author tn@cs.wpi.edu
 */

public class MQuery extends MIDBCollection
{

	/**
	 * KodKod Formula object which encodes the query. This formula is NOT
	 * quantifier-free. As of 8/09, this formula no longer includes axioms: just
	 * the user query.
	 */
	protected Formula myQueryFormula;

	/**
	 * This map contains the policies and views this query runs with respect to.
	 * We make an assumption that all policies on this list use the same MGVocab
	 * object (or they wouldn't be on the list together.)
	 *
	 * The string element is the policy name.
	 */
	HashMap<String, MIDBCollection> myIDBCollections;

	/**
	 * Indicates which SAT Solver application KodKod should use. Default is
	 * MiniSAT.
	 */
	public SATFactory mySATFactory;

	/**
	 * Integer value which tells KodKod how heavy-handed to be with symmetry
	 * breaking. Default is 20. Must be using a symmetric SAT Solver (Such as
	 * MiniSAT) to use this.
	 *
	 * @see mySATFactory
	 */
	public int mySB;
	
	/**
	 * 0: No debug output 1: Display statistics 2: Display statistics and query
	 * execution stages 3: Display statistics, query execution stages, and
	 * formulas. (Warning: level 3 runs .toString() on Formula objects, and so
	 * may be unwise to use for large policies or large queries.)
	 *
	 * Defaults to 0.
	 */
	public int debug_verbosity = 0;


	// For tupled queries. Need to know which tuple indexing to use for a given
	// IDB. Setting function will confirm that it is a valid list of numbers, and
	// that the arity matches the desired IDB name.
	protected HashMap<String, Set<List<String>>> idbsToAddInFormula = new HashMap<String, Set<List<String>>>();

	// time spent before query creation
	protected long msPreprocessingTime = -1;

	// The default default is SAT4j, for compatibility.
	// For performance (or if using large queries, for which SAT4j can run out
	// of heap space)
	// switch to MiniSAT.
	static SATFactory defaultSATFactory = SATFactory.DefaultSAT4J;

	public void printSettings()
	{
		// Print out the current settings for this query.

		if (mySATFactory.equals(SATFactory.MiniSat))
			MEnvironment.writeOutLine("SATFactory: MiniSAT");
		else
			MEnvironment.writeOutLine("SATFactory: SAT4j (Default)");

		MEnvironment.writeOutLine("SB: " + mySB);
	}

	protected void init(Formula nFormula) {
		myQueryFormula = nFormula;
		mySATFactory = defaultSATFactory;
	}

	// constructors
	/*private MQuery(Formula nFormula, MPolicy initialPolicy)
			throws  MGEUnknownIdentifier,
			MGEBadQueryString, MGEArityMismatch {
		vocab = initialPolicy.vocab;
		init(nFormula);

		myIDBCollections = new HashMap<String, MIDBCollection>();
		myIDBCollections.put(initialPolicy.name, initialPolicy);

		// Don't need to combine or replace -- only one source vocab.

	}*/

	protected MQuery(MVocab uber, Formula nFormula,
			Set<MIDBCollection> idbcollections) {
		vocab = uber;
		init(nFormula);

		myIDBCollections = new HashMap<String, MIDBCollection>();
		for (MIDBCollection p : idbcollections)
			myIDBCollections.put(p.name, p);
	}

	protected MQuery(Formula nFormula, MVocab voc) 
			throws MGEUnknownIdentifier, MGEBadQueryString, MGEArityMismatch {
		vocab = voc;
		init(nFormula);
		myIDBCollections = new HashMap<String, MIDBCollection>();
	}
	
	MQuery(MQuery previous) 
	throws MGEUnknownIdentifier, MGEBadQueryString, MGEArityMismatch 
	{
		vocab = previous.vocab;
		init(previous.myQueryFormula);

		debug_verbosity = previous.debug_verbosity;
		
		// "Deep enough" copy
		for(Map.Entry<String, Set<List<String>>> e : previous.idbsToAddInFormula.entrySet())
			idbsToAddInFormula.put(e.getKey(), new HashSet<List<String>>(e.getValue()));
				
		myIDBCollections = new HashMap<String, MIDBCollection>(previous.myIDBCollections);
	}

	public Set<String> getIDBNamesToOutput()
	{
		return idbsToAddInFormula.keySet();
	}

	public void useMiniSAT() {
		mySATFactory = SATFactory.MiniSat;
	}

	public void useSAT4j() {
		mySATFactory = SATFactory.DefaultSAT4J;
	}

	public static void useMiniSATAsDefault() {
		defaultSATFactory = SATFactory.MiniSat;
	}

	public static void useSAT4jAsDefault() {
		defaultSATFactory = SATFactory.DefaultSAT4J;
	}

	protected static int measureFullFormulaSize(Formula f) {
		FormulaFullMeasurementV fullmeasure = new FormulaFullMeasurementV();
		f.accept(fullmeasure);
		return fullmeasure.counter;
	}

	protected static int measureFormulaReferences(Formula f) {
		FormulaMeasurementV measure = new FormulaMeasurementV();
		f.accept(measure);
		// MREPL.outStream.println("bin refs: "+measure.iBinaryFormula);
		// MREPL.outStream.println("nary refs: "+measure.iNaryFormula);
		// MREPL.outStream.println("not refs: "+measure.iNotFormula);
		// MREPL.outStream.println("quant refs: "+measure.iQuantifiedFormula);
		// MREPL.outStream.println("comparison refs: "+measure.iComparisonFormula);
		return measure.counter;
	}

	/*
	 * private static Set<FuncStruct> walkASTForFunctions(Formula origfmla,
	 * MGVocab context) throws MGEUnknownIdentifier { // Convert to NNF (nearly)
	 * and return the result of walking the AST. WalkASTForFunctionsV walker =
	 * new WalkASTForFunctionsV(context);
	 *
	 * Formula nnf_formula = origfmla.accept(new NNFConverterV());
	 *
	 *
	 * //MREPL.outStream.println("Traversing both formulas and measuring their length..."
	 * );
	 * //MREPL.outStream.println("Length of pre-nnf: "+measureFormulaSize(origfmla)+
	 * ". Length of post-nnf: "+ // measureFormulaSize(nnf_formula));
	 *
	 * Set<FuncStruct> results = nnf_formula.accept(walker);
	 *
	 * if(walker.error) throw new
	 * MGEUnknownIdentifier("AST Walker returned error: "
	 * +walker.error_condition); return results; }
	 */

	protected void print_query_formula() {
		MEnvironment.writeOutLine(myQueryFormula.toString());
	}

	/*
	 * private boolean funcIsCoveredByOther(FuncStruct f, HashMap<Expression,
	 * Set<FuncStruct>> coveredby) throws MGEUnknownIdentifier,
	 * MGEBadIdentifierName { // We know that that f covers its own type, so
	 * check to see if there's some other in the list.
	 * if(coveredby.get(f.resulttype).size() > 1) return true;
	 *
	 * // Assert that this is a simple MgType, not an intersection (or etc.)
	 * thereof. // Search for covered subtypes of f's type. List<MGSort> tocheck
	 * = new LinkedList<MGSort>(); for(MGSort c :
	 * myVocab.getSortForExpression(f.resulttype).subsorts) tocheck.add(c);
	 *
	 * while(tocheck.size() > 0) { MGSort current = tocheck.get(0);
	 * if(coveredby.containsKey(current.rel))
	 * if(coveredby.get(current.rel).size() > 0) return true;
	 *
	 * // continue to check deeper in the hierarchy for(MGSort c :
	 * current.subsorts) tocheck.add(c); tocheck.remove(0); }
	 *
	 * if(debug_verbosity >= 2) MREPL.outStream.println("DEBUG: "+f +
	 * " was not multi-covered by "+coveredby); return false; }
	 */

	protected FormulaSigInfo getHerbrandUniverseCeilingFor(
			Formula queryAndQueryAxioms,
			boolean prenexExistential) throws MGEUnknownIdentifier,
			MGEBadIdentifierName
	{
		if(queryAndQueryAxioms.accept(new FindClosureUseV()))
			return new InvalidFormulaSigInfo(1);
		
		ThreadMXBean mxBean = ManagementFactory.getThreadMXBean();
		long startTime = mxBean.getCurrentThreadCpuTime();

		if (debug_verbosity >= 2)
			MEnvironment.writeOutLine("DEBUG: Time (ms) to check for closure: "
					+ (mxBean.getCurrentThreadCpuTime() - startTime) / 1000000);
		startTime = mxBean.getCurrentThreadCpuTime();

		// Use LeafExpression rather than Relation, because we may need to
		// represent Expression.UNIV
		// if sort inference fails.

		// Build a FormulaSigInfo object and get its totals.
		// TODO Now that FormulaSigInfo is separate, a better internal data structure would be nice.
		Set<LeafExpression> sorts = new HashSet<LeafExpression>();
		Map<LeafExpression, Set<LeafExpression>> supersorts = new HashMap<LeafExpression, Set<LeafExpression>>();
		Map<LeafExpression, List<LeafExpression>> predicates = new HashMap<LeafExpression, List<LeafExpression>>();
		Map<LeafExpression, Set<LeafExpression>> disjointness = new HashMap<LeafExpression, Set<LeafExpression>>();
		
		Map<Expression, LeafExpression> termTypes = new HashMap<Expression, LeafExpression>();
		
		// All terms seen by the vocabulary so far
		for(Expression expr : vocab.exprToTerm.keySet())
		{
			MTerm t = vocab.exprToTerm.get(expr);
			
			if(t instanceof MFunctionTerm)
			{
				MFunction theFunc = vocab.functions.get(((MFunctionTerm) t).funcName);
				termTypes.put(expr, theFunc.result.rel);
			}
			else if(t instanceof MConstantTerm)
			{
				MConstant theConst = vocab.constants.get(((MConstantTerm) t).constName);
				termTypes.put(expr, theConst.type.get(0).rel);
			}
		}
		
		// And constants (which may not have been mentioned, but will be!)
		for(MConstant c : vocab.constants.values())
		{
			termTypes.put(c.rel, c.type.get(0).rel);
		}
		
		for (MSort s : vocab.sorts.values()) {
			// s.rel is a sort.
			sorts.add(s.rel);

			////////////////////////////
			
			// What are the supersorts of s.rel? (parents + supersort
			// constraints)
			Set<LeafExpression> supers = new HashSet<LeafExpression>();

			Set<MSort> mgsups = vocab.buildSuperSortSet(s); // this is transitive
			for (MSort sup : mgsups)
				supers.add(sup.rel);

			supersorts.put(s.rel, supers);
			
			////////////////////////////
									
			// What is axiomatically disjoint from s.rel?
			Set<LeafExpression> theDisjs = new HashSet<LeafExpression>();
			for(MSort aDisj : vocab.getConciseDisjointSorts(s))
				theDisjs.add(aDisj.rel);
			disjointness.put(s.rel, theDisjs);
		}
		
		for (String pname : vocab.predicates.keySet())
		{
			// What sort is this predicate?
			List<LeafExpression> predArity = new ArrayList<LeafExpression>();			

			MPredicate thePred = vocab.predicates.get(pname);
			for (MSort argSort : thePred.type)
				predArity.add(argSort.rel);

			predicates.put(thePred.rel, predArity);
		}

		if (debug_verbosity >= 2)
			MEnvironment.writeOutLine("DEBUG: Time (ms) to populate maps for FormulaSigInfo: "
							+ (mxBean.getCurrentThreadCpuTime() - startTime)
							/ 1000000);
		startTime = mxBean.getCurrentThreadCpuTime();

		Formula nnf_formula = queryAndQueryAxioms; 

		if (debug_verbosity >= 2)
			MEnvironment.writeOutLine("DEBUG: Time (ms) to convert to NNF: "
					+ (mxBean.getCurrentThreadCpuTime() - startTime) / 1000000);
		startTime = mxBean.getCurrentThreadCpuTime();

		try {
			FormulaSigInfo.EnumSAPHandling sap = FormulaSigInfo.EnumSAPHandling.sapKeep;
			if (prenexExistential)
				sap = FormulaSigInfo.EnumSAPHandling.sapIgnore;

			FormulaSigInfo info = new FormulaSigInfo(sorts, supersorts,
					predicates, new HashSet<SigFunction>(),
					new HashSet<SigFunction>(), nnf_formula, sap, disjointness, termTypes);

			if (debug_verbosity >= 2) {
				MEnvironment.writeOutLine("DEBUG: Generating a ceiling on necessary model size.");
			}

			if (debug_verbosity >= 2) {
				if (info.getTermCount() < 0)
					MEnvironment.writeOutLine("\n DEBUG: Counting terms... infinitely many terms. Detail:");
				else
					MEnvironment.writeOutLine("\n DEBUG: Counting terms... " + info.getTermCount()
							+ " terms generated. Detail:");

				info.printInfo();
			}

			return info;
		} catch (MUnsupportedFormulaException E) {
			// unsupported
			return new InvalidFormulaSigInfo(2);
		} catch (MNotASortException E) {
			// unsupported
			return new InvalidFormulaSigInfo(3);
		}

	}
	
	public MPreparedQueryContext runQuery() throws MBaseException
	{				
		if (debug_verbosity >= 2)
			MEnvironment.writeOutLine("DEBUG: Beginning to execute query (runQuery) ");
		
		ThreadMXBean mxBean = ManagementFactory.getThreadMXBean();
		long totalStartTime = mxBean.getCurrentThreadCpuTime();
		long localStartTime = mxBean.getCurrentThreadCpuTime();

		// Formulas for fixed and query axioms.
		// Query axioms affect Herbrand Universe size!
		Formula myFixedAxioms = vocab.getAxiomFormulaNoEffectOnSize();
		Set<Formula> myQueryAxioms = vocab.getAxiomFormulasThatMayAffectSize();
		Formula queryAxiomsConjunction = MFormulaManager.makeConjunction(myQueryAxioms);

		// Build the actual query formula: The user query, the user axioms, and
		// the fixed sig axioms
		Formula queryWithAxioms = MFormulaManager.makeAnd(myQueryFormula,
				queryAxiomsConjunction);
		queryWithAxioms = MFormulaManager.makeAnd(queryWithAxioms,
				myFixedAxioms);

		if (debug_verbosity >= 2)
			MEnvironment.writeOutLine("DEBUG: Time (ms) to get and build axiom formulas: "
							+ (mxBean.getCurrentThreadCpuTime() - localStartTime)
							/ 1000000);
		localStartTime = mxBean.getCurrentThreadCpuTime();
		if (debug_verbosity >= 2)
			MEnvironment.writeOutLine("DEBUG: Getting HU Ceiling. ");
			
		///////////////////////////////////////
		PrenexCheckV pren = new PrenexCheckV();
		boolean prenexExistential = myQueryFormula.accept(pren);			
		// Run FormulaSigInfo, counting sort ceilings
		FormulaSigInfo herbrandBounds = getHerbrandUniverseCeilingFor(
				MFormulaManager.makeAnd(myQueryFormula, queryAxiomsConjunction),
				prenexExistential);				
		///////////////////////////////////////
		
		if (debug_verbosity >= 2)
			MEnvironment.writeOutLine("DEBUG: Time (ms) in getHerbrandUniverseCeilingFor block: "
							+ (mxBean.getCurrentThreadCpuTime() - localStartTime)
							/ 1000000);
		//startTime = mxBean.getCurrentThreadCpuTime();

		//////////////////////////////////////////////////////////////////
		//////////////////////////////////////////////////////////////////
				
		if (debug_verbosity >= 2)
		{
			MEnvironment.writeOutLine("DEBUG: Preparing Bounds and passing query to Kodkod.");			
		}

		if (debug_verbosity >= 3) {
			// Very very slow to print all this out (especially in IDE) if
			// formulas are anywhere near a decent size

			//MEnvironment.writeOutLine("Query Formula: " + myQueryFormula);
			MEnvironment.writeOutLine("Query Formula: ");
			myQueryFormula.accept(new FormulaIndentPrintV());
			MEnvironment.writeOutLine("Fixed Axioms Formula: " + myFixedAxioms);
			MEnvironment.writeOutLine("Query Axioms Formula: " + queryAxiomsConjunction);
			// MREPL.outStream.println("Complete Formula: "+queryWithAxioms);
		}

		// result of cputime is nanosecond scale
		long cputime = (mxBean.getCurrentThreadCpuTime() - totalStartTime) / 1000000;
		
		if (!mxBean.isCurrentThreadCpuTimeSupported())
			cputime = -1;

		if (debug_verbosity >= 1) {
			//MEnvironment.writeOutLine("DEBUG: Manager statistics: ");
			//MFormulaManager.printStatistics();
			MEnvironment.writeOutLine("Returning Solutions object; time so far:");
			MEnvironment.writeOutLine("Preprocessing: " + msPreprocessingTime
					+ "ms, pre-Kodkod query processing time: " + cputime
					+ "ms.");
		}
		
		return new MPreparedQueryContext(this, queryWithAxioms, herbrandBounds,
				 msPreprocessingTime, cputime, 0);

	}
	
	boolean myIDBCollectionsContainWithDot(String k)
	{		
		// Dot is a special character; remember to escape it.
		String[] arr = k.split("\\.");
		
		if(arr.length < 2)
			return false;
		if(!myIDBCollections.containsKey(arr[0]))
			return false;
		return myIDBCollections.get(arr[0]).containsIDB(arr[1]);
	
	}
	
	String getmyIDBCollectionsIDBs()
	{
		String result = "";
		for(String cname : myIDBCollections.keySet())
		{
			MIDBCollection coll = myIDBCollections.get(cname);
			
			result += "Collection "+cname+": "+coll.idbKeys()+"\n";
		}
		
		return result;
	}
	
	int myIDBCollectionsHaveArityForWithDot(String k)
	{
		// Dot is a special character; remember to escape it.
		String[] arr = k.split("\\.");
		if(arr.length < 2)
			return -1;
		if(!myIDBCollections.containsKey(arr[0]))
			return -1;
		return myIDBCollections.get(arr[0]).varOrderings.get(arr[1]).size();
		
	}


	public void removeIDBOutputIndexing(String idbname, List<String> indexing)
	{
		if(idbsToAddInFormula.containsKey(idbname))
			idbsToAddInFormula.get(idbname).remove(indexing);
	}

	public void addIDBOutputIndexing(String idbname, List<String> indexing)
			throws MGEArityMismatch, MGEUnknownIdentifier
	{
		// CHECK: the indexing Strings must actually be integers
		for (String inlist : indexing) {
			try {
				Integer.parseInt(inlist);
			} catch (NumberFormatException e) {
				throw new MGEArityMismatch(e.toString());
			}
		}

		// CHECK: Valid idb name

		String[] split = idbname.split(MEnvironment.sIDBSeparatorRegExp);
		String polName = split[0].toLowerCase();

		if (split.length < 2)
			throw new MGEUnknownIdentifier(
					"IDB names for output must be of the form policyname:idbname.");

		String internal_idbname = idbname.substring(polName.length() + 1)
				.toLowerCase(); // include :

		if (!myIDBCollections.containsKey(polName))
			throw new MGEUnknownIdentifier("IDB collection appeared in IDBOUTPUT that was not used in condition or UNDER clause: "
					+ polName + MEnvironment.eol + " Collections declared were: "+myIDBCollections.keySet());


		MIDBCollection coll = myIDBCollections.get(polName);

		if (!coll.containsIDB(internal_idbname))
			throw new MGEUnknownIdentifier("Unknown IDB: " + internal_idbname
					+ " in collection: " + polName);

		List<Variable> idbArity = coll.varOrderings.get(internal_idbname);
		
		// CHECK: Arity matches between the indexing given and the idb.
		if (idbArity.size() != indexing.size())
			throw new MGEArityMismatch(
					"Given indexing did not match IDB arity: " + idbname
					+ ", " + indexing + "." + " Arity of IDB was: "
					+ idbArity.size());

		// Add to indexing map
		if(!idbsToAddInFormula.containsKey(idbname))
			idbsToAddInFormula.put(idbname, new HashSet<List<String>>());
		idbsToAddInFormula.get(idbname).add(indexing);
	}

/*	public boolean isQuerySatisfiable() throws MGException
	{
		// don't bother saving the result
		return runQuery().getTotalIterator().hasNext();
	}
*/
	/**
	 * Used in the test suite to check whether a query had the expected number
	 * of solutions.
	 *
	 * @param expected_size
	 * @param expected_sols
	 * @param expected_hbu
	 * @return Whether the test case passed.
	 * @throws MBaseException
	 */
	public boolean runTestCase(int expected_size, int expected_sols,
			int expected_hbu) throws MBaseException
	{
		MPreparedQueryContext res = runQuery();

		int count = res.countModelsAtSize(expected_size);

		return (expected_sols == count)
				&& (res.getCeilingUsed() == expected_hbu);
	}

	
	static boolean someChildContainsTuple(Instance sol, MSort thetype,
			Tuple theTuple) {
		// if a "grandchild" contains the tuple, a "child" must.
		for (MSort child : thetype.subsorts)
		{
			// empty may be null
			if (sol.relationTuples().containsKey(child.rel) &&
					sol.relationTuples().get(child.rel).contains(theTuple))
				return true;
		}
		return false;
	}

	

	static public void unitTest() throws MBaseException
	{
		MEnvironment.writeErrLine("----- Begin MQuery Tests (No messages is good.) -----");

		Variable x = MFormulaManager.makeVariable("x");
		Variable y = MFormulaManager.makeVariable("y");
		Variable z = MFormulaManager.makeVariable("z");
		Variable z2 = MFormulaManager.makeVariable("z2");
		Variable z3 = MFormulaManager.makeVariable("z3");

		// *********************************************************************************
		// Establish vocabularies. Test for HU ceiling correctness.

		// Test to make sure cycles are detected.
		MVocab env = new MVocab();
		env.addSort("Sort1");

		Expression sort1 = env.getRelation("Sort1");

		// Not a cycle because no ground term... but
		// unsure how to handle the case where HU = empty
		Formula f = x.eq(y).forSome(x.oneOf(sort1)).forAll(y.oneOf(sort1));
		MQuery test1 = new MQuery(f, env);
		test1.runTestCase(1, 2, -1);

		// This IS a cycle.
		f = x.eq(y).forSome(x.oneOf(sort1)).forAll(y.oneOf(sort1)).and(
				sort1.some());
		MQuery test2 = new MQuery(f, env);
		if (test2.runQuery().getCeilingComputed() != -1)
			MEnvironment.writeErrLine("Test 2a failed!");

		// Test for other multiplicities that induce existentials (one)
		f = x.eq(y).forSome(x.oneOf(sort1)).forAll(y.oneOf(sort1)).and(
				sort1.one());
		test2 = new MQuery(f, env);
		if (test2.runQuery().getCeilingComputed() != -1)
			MEnvironment.writeErrLine("Test 2b failed!");

		// Test for lone (doesn't induce an existential.)
		// LONE(sort1) and \forall (y:sort1) \exists(x:sort1) (x = y)
		// expect 1 skolem func from sort1 to sort1, but no constants 
		f = x.eq(y).forSome(x.oneOf(sort1)).forAll(y.oneOf(sort1)).and(
				sort1.lone());
		test2 = new MQuery(f, env);
		if (test2.runQuery().getCeilingComputed() != 0)
			MEnvironment.writeErrLine("Test 2c failed!");


		// Test for no (doesn't induce an existential.)
		f = x.eq(y).forSome(x.oneOf(sort1)).forAll(y.oneOf(sort1)).and(
				sort1.no());
		test2 = new MQuery(f, env);
		if (test2.runQuery().getCeilingComputed() != 0)
			MEnvironment.writeErrLine("Test 2d failed!");

		// So is this (explicit quantifier)
		// (Formula.TRUE here was causing an exception, whereas something like
		// z.in(sort1) did not.
		// Leave it in to test resolution of bug.
		f = x.eq(y).forSome(x.oneOf(sort1)).forAll(y.oneOf(sort1)).and(
				Formula.TRUE.forSome(z.oneOf(sort1)));
		MQuery test3 = new MQuery(f, env);
		if (test3.runQuery().getCeilingComputed() != -1)
			MEnvironment.writeErrLine("Test 3 failed!");

		// Test multiple ground terms
		f = Formula.TRUE.forSome(z.oneOf(sort1)).and(sort1.one());
		MQuery test4 = new MQuery(f, env);
		if (test4.runQuery().getCeilingComputed() != 1) // should filter out the
													// .one
			MEnvironment.writeErrLine("Test 4a failed!");

		f = Formula.TRUE.forSome(z.oneOf(sort1)).and(sort1.some()).or(
				Formula.TRUE.forSome(x.oneOf(sort1)));
		test4 = new MQuery(f, env);
		if (test4.runQuery().getCeilingComputed() != 2) // prunes out the .some, but
													// still two exists
			MEnvironment.writeErrLine("Test 4b failed!");

		// *********************************************************************************
		// *********************************************************************************
		// Cannot check function term generation in that vocab because there is
		// only one sort.
		// 2nd vocab:

		env = new MVocab();

		// 2 top level types
		env.addSort("Sort1");
		env.addSort("Sort2");

		// Each top level sort has 2 child sorts.
		env.addSubSort("Sort1", "Sort1A");
		env.addSubSort("Sort1", "Sort1B");
		env.addSubSort("Sort2", "Sort2A");
		env.addSubSort("Sort2", "Sort2B");

		// Sort 1's children are disjoint constrained, sort 2's are not.
		//env.axioms.addConstraintDisjointAll("Sort1");

		sort1 = env.getRelation("Sort1");
		Expression sort2 = env.getRelation("Sort2");

		Expression sort1a = env.getRelation("Sort1A");
		// Expression sort1b = env.getRelation("Sort1B");
		Expression sort2a = env.getRelation("Sort2A");
		Expression sort2b = env.getRelation("Sort2B");

		// Now we can have lots of functions induced by quantifiers (and
		// multiplicities.)

		// f:A->B, x:A. Two atoms.
		f = x.eq(y).forSome(x.oneOf(sort2)).forAll(y.oneOf(sort1)).and(
				sort1.some());
		MQuery test5 = new MQuery(f, env);
		if (test5.runQuery().getCeilingComputed() != 2)
			MEnvironment.writeErrLine("Test 5a failed!");

		// f:A->B, x:B. One atom.
		f = x.eq(y).forSome(x.oneOf(sort2)).forAll(y.oneOf(sort1)).and(
				sort2.some());
		test5 = new MQuery(f, env);
		if (test5.runQuery().getCeilingComputed() != 1)
			MEnvironment.writeErrLine("Test 5b failed!");

		// f:A->B, x:B y:B. Two atoms.
		f = x.eq(y).forSome(x.oneOf(sort2)).forAll(y.oneOf(sort1)).and(
				Formula.TRUE.forSome(z.oneOf(sort2))).and(
				Formula.TRUE.forSome(z2.oneOf(sort2)));
		test5 = new MQuery(f, env);
		if (test5.runQuery().getCeilingComputed() != 2)
			MEnvironment.writeErrLine("Test 5c failed!");

		// f:A->B, x:B, y:B, z:A. Four atoms.
		f = x.eq(y).forSome(x.oneOf(sort2)).forAll(y.oneOf(sort1)).and(
				Formula.TRUE.forSome(z.oneOf(sort2))).and(
				Formula.TRUE.forSome(z2.oneOf(sort2))).and(
				Formula.TRUE.forSome(z3.oneOf(sort1)));
		test5 = new MQuery(f, env);
		if (test5.runQuery().getCeilingComputed() != 4)
			MEnvironment.writeErrLine("Test 5d failed!");

		// Special cases:

		// f:A->subA. No ground. Cannot be sure here (HU has zero terms!)
		f = x.eq(y).forSome(x.oneOf(sort1a)).forAll(y.oneOf(sort1));
		test5 = new MQuery(f, env);
		if (test5.runQuery().getCeilingComputed() != 0)
			MEnvironment.writeErrLine("Test 5e failed!");

		// F:subA->A same as above.
		f = x.eq(y).forSome(x.oneOf(sort1)).forAll(y.oneOf(sort1a));
		test5 = new MQuery(f, env);
		if (test5.runQuery().getCeilingComputed() != 0)
			MEnvironment.writeErrLine("Test 5f failed!");

		// Unproductive function and a constant
		f = x.eq(y).forSome(x.oneOf(sort1)).forAll(y.oneOf(sort1a)).and(
				sort1.some());
		test5 = new MQuery(f, env);
		if (test5.runQuery().getCeilingComputed() != 1)
			MEnvironment.writeErrLine("Test 5g failed!");

		// f:subA->A. x:subA. subA <= A.
		// Constant in subA is also in A. (same term, don't double count)
		// Then the function application is in A
		f = x.eq(y).forSome(x.oneOf(sort1)).forAll(y.oneOf(sort1a)).and(
				sort1a.some());
		test5 = new MQuery(f, env);
		if (test5.runQuery().getCeilingComputed() != 2)
			MEnvironment.writeErrLine("Test 5h failed!");

		// f:A->subA. x:A. (inf; f obviously cyclic)
		f = x.eq(y).forSome(x.oneOf(sort1a)).forAll(y.oneOf(sort1)).and(
				sort1.some());
		test5 = new MQuery(f, env);
		if (test5.runQuery().getCeilingComputed() != -1)
			MEnvironment.writeErrLine("Test 5i failed!");

		// f:A->subA x:subA. (inf; f obviously cyclic, and subA is a subtype of
		// A.)
		// (This tests that finding productive functions considers inclusions)
		f = x.eq(y).forSome(x.oneOf(sort1a)).forAll(y.oneOf(sort1)).and(
				sort1a.some());
		test5 = new MQuery(f, env);
		if (test5.runQuery().getCeilingComputed() != -1)
			MEnvironment.writeErrLine("Test 5j failed!");

		// cycle detection with overlapping types
		// Sort2a and Sort2b may overlap! No disjoint constraint.

		// f:Sort2a->Sort2b. x:2a. only 2 terms -- x, f(x) -- though they get
		// propagated
		f = x.eq(y).forSome(x.oneOf(sort2b)).forAll(y.oneOf(sort2a)).and(
				sort2a.some());
		test5 = new MQuery(f, env);
		if (test5.runQuery().getCeilingComputed() != 2)
			MEnvironment.writeErrLine("Test 5k failed!");

		// f:Sort2a->Sort2b. x:2b. Func is unproductive. Only 1 term (the
		// constant)
		f = x.eq(y).forSome(x.oneOf(sort2b)).forAll(y.oneOf(sort2a)).and(
				sort2b.some());
		test5 = new MQuery(f, env);
		if (test5.runQuery().getCeilingComputed() != 1)
			MEnvironment.writeErrLine("Test 5l failed!");

		// f:Sort2a->Sort2b. x:2. Another unproductive function.
		f = x.eq(y).forSome(x.oneOf(sort2b)).forAll(y.oneOf(sort2a)).and(
				sort2.some());
		test5 = new MQuery(f, env);
		if (test5.runQuery().getCeilingComputed() != 1)
			MEnvironment.writeErrLine("Test 5m failed!");

		// make sure we have distinct functions even if declared identically.
		// one level of "identity"
		f = Formula.TRUE.forSome(z.oneOf(sort1)).and(
				Formula.TRUE.forSome(z.oneOf(sort1)));
		test5 = new MQuery(f, env);
		if (test5.runQuery().getCeilingComputed() != 2)
			MEnvironment.writeErrLine("Test 5n failed!");

		// what if we have more than one level of identity? Make sure we get 2
		// atoms...

		// Manager will notice that they are the same formula, and only have 1
		// in the AND in the first place.
		// f = MGFormulaManager.makeAnd(f, f);
		// Similarly, the simplifier will take f = f.and(f) and return f.
		// So we need to be very inefficient to test this:

		f = Formula.TRUE.forSome(z.oneOf(sort1)).or(
				Formula.TRUE.forAll(z.oneOf(sort1)));
		Formula f2 = Formula.TRUE.forSome(z.oneOf(sort1)).or(
				Formula.TRUE.forAll(z.oneOf(sort1)));
		f = f.and(f2);
		// NOW we have 2 "different" subformulas.

		test5 = new MQuery(f, env);
		// test5.debug_verbosity = 3;
		if (test5.runQuery().getCeilingComputed() != 2)
			MEnvironment.writeErrLine("Test 5o failed!");

		// Term ids in the above must be *different*
		// TODO convert this test?
		/*
		 * WalkASTForFunctionsV vis = new WalkASTForFunctionsV(env);
		 * Set<FuncStruct> funcs = f.accept(new NNFConverterV()).accept(vis);
		 * //for(FuncStruct fx : funcs) // MEnvironment.writeErrLine(fx.getID());
		 * List<FuncStruct> lfuncs = new ArrayList<FuncStruct>(funcs);
		 * if(lfuncs.size() != 2 ||
		 * lfuncs.get(0).getID().equals(lfuncs.get(1).getID()))
		 * MEnvironment.writeErrLine("Test 5p (unique identifiers) failed!");
		 */

		// *********************************************************************************
		// Now need a vocab with multiple layers of subtypes to make sure the
		// DFS extends properly.
		// 3rd vocab:
		env = new MVocab();

		// 2 top level types
		env.addSort("Sort1");
		env.addSort("Sort2");

		// Sort 2 has 3 child sorts.
		env.addSubSort("Sort2", "Sort2a");
		env.addSubSort("Sort2", "Sort2b");
		env.addSubSort("Sort2", "Sort2c");

		// 2a and 2b may overlap. b and c are disjoint.
		//env.axioms.addConstraintDisjoint("Sort2b", "Sort2c");

		// Each child sort has a child sort.
		env.addSubSort("Sort2a", "Sort2ax");
		env.addSubSort("Sort2b", "Sort2bx");
		env.addSubSort("Sort2c", "Sort2cx");

		sort1 = env.getRelation("Sort1");
		sort2 = env.getRelation("Sort2");
		sort2a = env.getRelation("Sort2a");
		sort2b = env.getRelation("Sort2b");
		Expression sort2c = env.getRelation("Sort2c");
		Expression sort2ax = env.getRelation("Sort2ax");
		Expression sort2bx = env.getRelation("Sort2bx");
		Expression sort2cx = env.getRelation("Sort2cx");

		// f:2c->2cx g:2bx->2b x:2 y:2bx. The function f is unproductive.
		// Only one constant since sort2bx.some() covers sort2.some()
		// + 1 func application
		f = x.eq(y).forSome(x.oneOf(sort2cx)).forAll(y.oneOf(sort2c)).and(
				x.eq(y).forSome(x.oneOf(sort2b)).forAll(y.oneOf(sort2bx))).and(
				sort2.some()).and(sort2bx.some());
		MQuery test6 = new MQuery(f, env);
		if (test6.runQuery().getCeilingComputed() != 2)
			MEnvironment.writeErrLine("Test 6a failed!");

		// Non-infinite with non trivial function combinations
		// f:1->2c. g:2c->2b. x:1 y:2 -- 2 constants, both funcs productive
		// x, f(x), g(f(x)), y.
		// TODO: y could be eliminated by a coverage argument. (f(x) is in
		// sort2, and y is from sort2.some().)
		f = x.eq(y).forSome(x.oneOf(sort2c)).forAll(y.oneOf(sort1)).and(
				x.eq(y).forSome(x.oneOf(sort2b)).forAll(y.oneOf(sort2c))).and(
				sort1.some()).and(sort2.some());
		test6 = new MQuery(f, env);
		if (test6.runQuery().getCeilingComputed() != 4)
			MEnvironment.writeErrLine("Test 6b failed!");		
		
		// f:1->2c. g:2a->2b. x:1
		// g is unproductive. only 2 terms
		f = x.eq(y).forSome(x.oneOf(sort2c)).forAll(y.oneOf(sort1)).and(
				x.eq(y).forSome(x.oneOf(sort2b)).forAll(y.oneOf(sort2a))).and(
				sort1.some());
		test6 = new MQuery(f, env);
		if (test6.runQuery().getCeilingComputed() != 2)
			MEnvironment.writeErrLine("Test 6c failed!");

		// Cycle of longer length
		// f:1->2a, g:2a->2c h:2c->1 x:1 (inf)
		f = x.eq(y).forSome(x.oneOf(sort2a)).forAll(y.oneOf(sort1)).and(
				x.eq(y).forSome(x.oneOf(sort2c)).forAll(y.oneOf(sort2a))).and(
				x.eq(y).forSome(x.oneOf(sort1)).forAll(y.oneOf(sort2c))).and(
				sort1.some());
		test6 = new MQuery(f, env);
		if (test6.runQuery().getCeilingComputed() != -1)
			MEnvironment.writeErrLine("Test 6d failed!");

		// Test more exotic multiplicity locations

		// lone, with ground (lone uses universals only, so no term or function
		// induced...)
		// f:1->2ax. x:2bx. lone 1.
		f = x.eq(y).forSome(x.oneOf(sort2ax)).forAll(y.oneOf(sort1)).and(
				sort2bx.some()).and(sort1.lone());
		test6 = new MQuery(f, env);
		if (test6.runQuery().getCeilingComputed() != 1)
			MEnvironment.writeErrLine("Test 6e failed!");

		// not no
		// not no(1) -- should give us 1 atom.
		f = sort1.no().not();
		test6 = new MQuery(f, env);
		if (test6.runQuery().getCeilingComputed() != 1)
			MEnvironment.writeErrLine("Test 6f failed!");

		// not lone -- should give us two atoms
		f = sort1.lone().not();
		test6 = new MQuery(f, env);
		if (test6.runQuery().getCeilingComputed() != 2)
			MEnvironment.writeErrLine("Test 6g failed!");

		// not one -- same thing (since terms are generated worst-case)
		f = sort1.one().not();
		test6 = new MQuery(f, env);
		if (test6.runQuery().getCeilingComputed() != 2)
			MEnvironment.writeErrLine("Test 6h failed!");

		// not one encased in a universal -- two functions induced. Add a ground
		// term and should get *2* plus that ground.
		f = sort1.one().not().forAll(x.oneOf(sort2)).and(sort2.one());
		test6 = new MQuery(f, env);
		if (test6.runQuery().getCeilingComputed() != 3)
			MEnvironment.writeErrLine("Test 6i failed!");

		// We use the hash code of the nodes (Decl, not the variable!) to
		// simulate alpha renaming.
		// Test to see if two instances of the same VARIABLE will be treated
		// differently.
		f = Formula.TRUE.forSome(x.oneOf(sort1)).forAll(y.oneOf(sort2)).and(
				Formula.TRUE.forSome(x.oneOf(sort1)).forAll(y.oneOf(sort2)))
				.and(sort2.some());
		test6 = new MQuery(f, env);
		// test6.debug_show_all_formula = true;
		if (test6.runQuery().getCeilingComputed() != 3)
			MEnvironment.writeErrLine("Test 6j (simulated alpha renaming) failed!");

		// *********************************************************************************
		// Same vocab as before, but with a custom predicate constrained to be a
		// total function.
		// We also extend the non-disjoint-constrained branch to test "deeper"
		// related sort detection.
		env = new MVocab();

		// 2 top level types
		env.addSort("Sort1");
		env.addSort("Sort2");

		// Sort 2 has 3 child sorts.
		env.addSubSort("Sort2", "Sort2a");
		env.addSubSort("Sort2", "Sort2b");
		env.addSubSort("Sort2", "Sort2c");

		// 2a and 2b may overlap. b and c are disjoint.
		//env.axioms.addConstraintDisjoint("Sort2b", "Sort2c");

		// Each child sort has a child sort.
		env.addSubSort("Sort2a", "Sort2ax");
		env.addSubSort("Sort2b", "Sort2bx");
		env.addSubSort("Sort2c", "Sort2cx");
		env.addSubSort("Sort2ax", "Sort2axx");
		env.addSubSort("Sort2bx", "Sort2bxx");

		env.addPredicate("F", "Sort1 Sort2");
		env.axioms.addConstraintTotalFunction("F");

		sort1 = env.getRelation("Sort1");
		sort2 = env.getRelation("Sort2");
		sort2a = env.getRelation("Sort2a");
		sort2b = env.getRelation("Sort2b");
		sort2c = env.getRelation("Sort2c");
		sort2ax = env.getRelation("Sort2ax");
		sort2bx = env.getRelation("Sort2bx");
		sort2cx = env.getRelation("Sort2cx");
		Expression sort2axx = env.getRelation("Sort2axx");
		//Expression sort2bxx = env.getRelation("Sort2bxx");

		// Another unproductive function (which would otherwise induce inf. many
		// terms)
		f = x.eq(y).forSome(x.oneOf(sort2a)).forAll(y.oneOf(sort2a)).and(
				sort2.some());
		MQuery test7 = new MQuery(f, env);
		if (test7.runQuery().getCeilingComputed() != 1)
			MEnvironment.writeErrLine("Test 7a failed!");

		// Since 2axx <= 2, infinitely many terms
		f = x.eq(y).forSome(x.oneOf(sort2axx)).forAll(y.oneOf(sort2)).and(
				sort2.some());
		test7 = new MQuery(f, env);
		if (test7.runQuery().getCeilingComputed() != -1)
			MEnvironment.writeErrLine("Test 7b failed!");

		// Harmless
		f = x.eq(y).forSome(x.oneOf(sort2)).forAll(y.oneOf(sort2b)).and(
				sort2b.some());
		test7 = new MQuery(f, env);
		// test7.debug_show_all_formula = true;
		if (test7.runQuery().getCeilingComputed() != 2)
			MEnvironment.writeErrLine("Test 7c failed!");

		// Make sure that "coverage" logic works correctly
		// Not yet in.
		// f = Formula.TRUE.forSome(x.oneOf(sort1)).forAll(y.oneOf(sort2))
		// .and(sort2.one()).and(sort1.one());
		// test7 = new MGQuery(f, env);
		// if(test7.runQuery().get_hu_ceiling() != 2)
		// MEnvironment.writeErrLine("Test 7d failed!");

		// We have a total function constraint -- F: 1 -> 2
		// We have a formula inducing G: 2 -> 1
		// and a ground term of sort 1.
		f = Formula.TRUE.forSome(x.oneOf(sort1)).forAll(y.oneOf(sort2)).and(
				sort1.one());
		test7 = new MQuery(f, env);
		// test7.debug_show_all_formula = true;
		if (test7.runQuery().getCeilingComputed() != -1)
			MEnvironment.writeErrLine("Test 7e failed!");

		// But don't be overeager (no starter term)
		f = Formula.TRUE.forSome(x.oneOf(sort1)).forAll(y.oneOf(sort2));
		test7 = new MQuery(f, env);
		// test7.debug_show_all_formula = true;
		if (test7.runQuery().getCeilingComputed() != 0)
			MEnvironment.writeErrLine("Test 7e(1) failed!");

		// 2 "one"s in the same sort. We want to be smart enough to detect that
		// they cover
		// each other, and only need 1 in sort A (but still need a 2nd in sort 2
		// for the total-function relation.)
		f = sort1.one().and(sort1.one());
		test7 = new MQuery(f, env);
		if (test7.runQuery().getCeilingComputed() != 2)
			MEnvironment.writeErrLine("Test 7f failed!");

		// Don't want to count 2 explicit existentials as covering each other!
		// (Only multiplicities with
		// single induced GROUND existentials can be covered.)
		// (Need to include 2 for sort 1, and another 2 for their F-images.)
		f = Formula.TRUE.forSome(x.oneOf(sort1)).and(
				Formula.TRUE.forSome(y.oneOf(sort1)));
		test7 = new MQuery(f, env);
		if (test7.runQuery().getCeilingComputed() != 4)
			MEnvironment.writeErrLine("Test 7g failed!");

		// Sub coverage
		f = sort2.one().and(sort2a.one());
		test7 = new MQuery(f, env);
		// test7.debug_show_all_formula = true;
		if (test7.runQuery().getCeilingComputed() != 1)
			MEnvironment.writeErrLine("Test 7h failed!");

		// Cycles caused by interaction between a total function and something
		// else.
		// F goes from 1 to 2.
		// OK without the constraints formula from the vocab!
		// But NOT ok with the constraint included (which happens by default
		// now).
		f = Formula.TRUE.forSome(x.oneOf(sort1)).forAll(y.oneOf(sort2)).and(
				sort1.some());
		test7 = new MQuery(f, env);
		if (test7.runQuery().getCeilingComputed() != -1)
			MEnvironment.writeErrLine("Test 7i failed!");

		// functions induced by "total function" constraints look like this:
		// fFC1_4579880([sort1]): sort2
		// This shows that the types have been resolved (F: Sort1 -> Sort2)

		// *********************************************************************************
		// What about subset constraints? That is, the user has said P is a
		// subset of R, even though P's parent type is S.
		// (Make sure this and types interact well.)

		// some2cx, f(some2cx) func-constraint(f(some2cx))
		f = Formula.TRUE.forSome(x.oneOf(sort1)).forAll(y.oneOf(sort2cx)).and(
				sort2cx.some());
		test7 = new MQuery(f, env);
		if (test7.runQuery().getCeilingComputed() != 3)
			MEnvironment.writeErrLine("Test 8a failed!");


		f = Formula.TRUE.forSome(x.oneOf(sort2axx)).forAll(y.oneOf(sort2cx))
				.and(sort2cx.some());
		test7 = new MQuery(f, env);
		// test7.debug_verbosity = 2;
		if (test7.runQuery().getCeilingComputed() != 2)
			MEnvironment.writeErrLine("Test 8b failed!");

		// MEnvironment.writeErrLine(env.buildSuperSetSet(env.getType("sort2axx")));
		// MEnvironment.writeErrLine(env.buildSuperSetSet(env.getType("sort2cx")));
		// MEnvironment.writeErrLine(env.setsSubset.get("sort2axx"));

		// *OR* add new type 2d, with 2axx < 2d < 2b, and they must be disjoint.
		// (Test multi-subset)
		// Hack to remove the constraint we just added
		//env.axioms.setsSubset.get("sort2axx").remove("sort2bxx");
		// Changes
		env.addSubSort("sort2", "sort2d");
		//env.axioms.addConstraintSubset("sort2axx", "sort2d");
		//env.axioms.addConstraintSubset("sort2d", "sort2bxx");
		f = Formula.TRUE.forSome(x.oneOf(sort2axx)).forAll(y.oneOf(sort2cx))
				.and(sort2cx.some());
		test7 = new MQuery(f, env);
		if (test7.runQuery().getCeilingComputed() != 2)
			MEnvironment.writeErrLine("Test 8c failed!");

		// *********************************************************************************
		// Tests to make sure use of sort names as predicates doesn't break
		// soundness/completeness.
		// Many more of these tests in FormulaSigInfo.

		env = new MVocab();

		// 3 top level types
		env.addSort("Sort1");
		env.addSort("Sort2");
		env.addSort("Sort3");

		sort1 = env.getRelation("Sort1");
		sort2 = env.getRelation("Sort2");
		Relation sort3 = env.getRelation("Sort3");

		env.addSubSort("Sort1", "Sort1a");
		env.addSubSort("Sort1", "Sort1b");
		env.addSubSort("Sort1", "Sort1c");
		sort1a = env.getRelation("Sort1a");
		Relation sort1b = env.getRelation("Sort1b");
		Relation sort1c = env.getRelation("Sort1c");
		
		env.addPredicate("P", "Sort1");
		Relation P = env.getRelation("P");

		// OLD approach to sorts-as-predicates was:
		// f: 2->3. EXISTS c: sort1 AND sort2. (treats as two different
		// constants)
		// c1:1, c2:2, f(c2):3.

		// NEW handling:
		// z is in sort1, but also sort 2. same term.
		// f(z) is in sort3.
		f = Formula.TRUE.forSome(x.oneOf(sort3)).forAll(y.oneOf(sort2)).and(
				z.in(sort2).forSome(z.oneOf(sort1)));
		MQuery test9 = new MQuery(f, env);
		if (test9.runQuery().getCeilingComputed() != 2)
			MEnvironment.writeErrLine("Test 9a failed!");			
		
		// Same thing, using subsorts (this is really the same test as 9a,
		// actually?)
		// something in 1a that is in 1b too; and f:1b->1c.
		f = Formula.TRUE.forSome(x.oneOf(sort1c)).forAll(y.oneOf(sort1b)).and(
				z.in(sort1b).forSome(z.oneOf(sort1a)));
		test9 = new MQuery(f, env);
		if (test9.runQuery().getCeilingComputed() != 2)
			MEnvironment.writeErrLine("Test 9b failed!");

		// Only one constant (z in sort2). It gets locally coerced to sort1. 
		// And there's a global coercion from sort1 to sort1a.
		// Since we are aware of disjointness, needs to be sort1a and not, say, sort3 (due to disj <-> incomparable)
		Formula f1 = y.in(sort1a).forAll(y.oneOf(sort1));
		f2 = z.in(sort1).forSome(z.oneOf(sort2));
		f = f1.and(f2);
		test9 = new MQuery(f, env);
		MPreparedQueryContext result = test9.runQuery();
		if (result.getCeilingComputed() != 1 ||
				result.ceilingsSufficient.get("Sort1a").intValue() != 1 ||
				result.herbrandBounds.getGlobalSAPFunctions().size() != 1 ||
				result.herbrandBounds.getLocalSAPFunctions().size() != 1)
			MEnvironment.writeErrLine("Test 9c failed!");
		result.getTotalIterator().hasNext();		
				
		// (and (forall y (exists x (P x))) (exists x (P x)))
		// If RHS is evaluated first, don't end up with
		// 2 functions instead of 1 function and 1 constant.
		// (See FormulaSigInfo.java:WalkAST:visit(QuantifiedFormula q))
		f1 = x.in(P).forSome(x.oneOf(sort1));
		f2 = f1.forAll(y.oneOf(sort1));
		f = f2.and(f1); // VITAL that this is f2, not restating f2
		test9 = new MQuery(f, env);
		result = test9.runQuery();
		if (result.herbrandBounds.getSkolemFunctions().size() != 1 ||
				result.herbrandBounds.getSkolemConstants().size() != 1)
			MEnvironment.writeErrLine("Test 9d failed!");
		
		
		
		MEnvironment.writeErrLine("----- End MQuery Tests -----");

		/*
		 * Relation A = Relation.unary("A"); Formula testf =
		 * x.in(A).forSome(x.oneOf(Expression.UNIV));
		 *
		 * LinkedList<String> atoms = new LinkedList<String>(); atoms.add("1");
		 * //atoms.add("2");
		 *
		 * Universe u = new Universe(atoms); Bounds b = new Bounds(u);
		 * b.bound(A, u.factory().allOf(1));
		 *
		 * Options opt = new Options(); opt.setLogTranslation(2);
		 *
		 *
		 * try { Translation t = Translator.translate(testf, b, opt);
		 * MEnvironment.writeErrLine(t.numPrimaryVariables());
		 * Iterator<TranslationRecord> it = t.log().replay();
		 * while(it.hasNext()) { MEnvironment.writeErrLine(it.next()); }
		 * MEnvironment.writeErrLine(t.log()); } catch(Exception e) {
		 * MEnvironment.writeErrLine(e); }
		 */

	}

	
	
	// ************
	// New parser will build up an MExploreCondition object.
	// Create a query from it.

	public static MQuery createFromExplore(
			String queryID,
			MExploreCondition mpc,
			List<String> publish,
			Map<String, String> sortsForPublish,
			Map<String, Set<List<MTerm>>> includeMap,
			Boolean bTupling, Integer iDebugLevel, Integer iCeiling)
			throws MUserException
			{

		// TODO, repurpose these exceptions in context of new qry language
		// (give them fields for row/col...)

		// Given all 3 parameters
		// outmod default: ""
		// publish default: null
		// note that null is DIFFERENT from an empty list!
		// null means publish everything in arbitrary order.
		// empty list means publish NOTHING

		// *****************
		// Validation
		// *****************

		MCommunicator.writeToLog("ENTERING CREATION OF QUERY:");
		MCommunicator.writeToLog("Fmla: "+mpc.fmla);
		MCommunicator.writeToLog("Idb out map: "+includeMap.toString());
		MCommunicator.writeToLog("tup: "+bTupling);

		// includeMap is always indexed now. INCLUDE no longer takes relation names, but requires
		// atomic fmlas, same as SHOW REALIZED.
		// TODO: include should be part of SHOW, not EXPLORE.

		// **********************************************************
		// (1) Assemble a combined vocabulary
		// **********************************************************
		MVocab uber = null;

		for (MIDBCollection p : mpc.seenIDBCollections) {
			if (uber == null)
				uber = p.vocab;
			else			
				uber = uber.combineWith(p.vocab);
		}

		// No IDBS, no UNDER clause?
		if (uber == null) {
			// no vocab! Error out.
			throw new MGEUnknownIdentifier("A query that mentions neither policies nor prior queries must use the UNDER clause to give some policy context to the query.");
		}

		// **********************************************************
		// (2) Check for invalid EDBs
		// **********************************************************
		for (Relation r : mpc.madeEDBs)
		{
			// If it's a known predicate, it's ok.
			// Vocab combination forces only one arity for each R, so should only appear once.
			String relName = r.name();
			if (uber.predicates.keySet().contains(relName))
				continue;
			
			try 
			{
				// Does it have a valid sort arity? 
				// (May have 2 predicates with same name and different arity, so this is needed)
				if(r.arity() > 1)
					throw new MGEUnknownIdentifier("Unknown predicate " + r + " of arity "+r.arity()+". The given policies were unaware of this EDB name.");

				// Finally check for the right name
				uber.getSortForExpression(r);
				
			} catch (MGEUnknownIdentifier e) {
				throw new MGEUnknownIdentifier("Unknown predicate " + r + " of arity "+r.arity()+". The given policies were unaware of this EDB name.");
			}
		}

		// **********************************************************
		// At this point, we have the query formula (w/ proper variables)
		// but no prefix or sorts on the vars. 

		// !!! XXX this should go
		// first, bind placeholders properly
		// E.g. (A=x) will be converted to A(x) if A is a lone/one sort
		//mpc.resolvePlaceholders(uber);
		
		// Turn A=x into A(x) -- if able. The func changes the map given.
		//MExploreCondition.resolveMapPlaceholders(uber, includeMap);
		
		// !!! XXX this should go
		// include formulas need to be well-formed, so they figure into sort inference
		//mpc.inferFromInclude(uber, includeMap);
		
		Formula qryFormula = mpc.fmla;

		// !!! XXX this should go, use given vector of vars instead?
		// mpc has given us a bunch of assertions. Now we need to
		// (a) unpack the ones that do not refer to sort EDBs, now that we have
		// signatures of the predicates
		// (b) resolve them.
		//Map<Variable, Expression> freeVars = new HashMap<Variable, Expression>();
		//handleSortAssertions(uber, mpc, freeVars);
		
		if(iDebugLevel >= 3)
		{
			MEnvironment.outWriter.println("DEBUG: Query Formula = "+ qryFormula);
			MEnvironment.outWriter.println("DEBUG: MPC made EDBs: "+mpc.madeEDBs);
			MEnvironment.outWriter.println("DEBUG: MPC saw IDB Collections: "+mpc.seenIDBCollections);
			MEnvironment.outWriter.println("DEBUG: MPC terms: "+mpc.terms);
		}
		
		Set<Variable> freeVarsUnsorted = qryFormula.accept(new FreeVariableCollectionV());		
		
		
		// (3) assemble prefix
		// **********************************************************

		// For saved query use
		Formula idbFormula = qryFormula;
		List<Variable> varOrdering = new ArrayList<Variable>();


		// ***
		// idbFormula: has free vars, for use later
		// qryFormula: all quantified, for use now

		// Need to make sure the ordering in publish is preserved, so can't just
		// iterate freeVars.keySet().
		
		// publish will always be passed, however, even if it is empty, e.g.:
		// let F[] be ...;
		

		List<String> prefixVarOrder = new ArrayList<String>();

		MCommunicator.writeToLog("\n\n");
		MCommunicator.writeToLog("creating query from explore: \n");
		MCommunicator.writeToLog("publish = "+publish+"\n");
		MCommunicator.writeToLog("freeVars = "+freeVarsUnsorted+"\n");
		MCommunicator.writeToLog("\n\n");
		
		// FIRST: Published vars
		for (String vname : publish)
		{
			Variable v = MFormulaManager.makeVariable(vname);
			varOrdering.add(v);
			
			// Make sure that this variable actually appears in the query.
			// Throw an exception if it is garbage.
			if(!freeVarsUnsorted.contains(v))
			{
				throw new MGEUnknownIdentifier("The variable "+v+" appeared in the query's free variable list, did not appear in the condition.");
			}
			
			Expression theSort = uber.getSort(sortsForPublish.get(vname)).rel;
			Decl d = MFormulaManager.makeOneOfDecl(v, theSort);
			qryFormula = MFormulaManager.makeExists(qryFormula, d);
			prefixVarOrder.add(v.name());
		}


		//System.err.println(freeVarsUnsorted);
		//System.err.println(publish);
		//System.err.println(sortsForPublish);

		// There should be NO unpublished free variables. If there are, throw an error. 
		// (need to get the sort for them somehow!)
		// quantifiers are now inserted explicitly in the query, not "inferred".
		// - TN 5/11
		
		for (Variable v : freeVarsUnsorted)
		{
			if (!publish.contains(v.name()))			
				throw new MGEUnknownIdentifier("The query formula had a free variable "+v.name()+" that was not declared or typed.");
		}

		//MEnvironment.writeErrLine("Temp debug info: "+qryFormula);
		//MEnvironment.writeErrLine(prefixVarOrder);

		// Reverse because we are building the quantifiers from inside out
		Collections.reverse(prefixVarOrder);

		MQuery result;
		// Tupling is deprecated (possibly temporary) as of September 2011 -- TN
		//if(bTupling) {
		//	result = new MTuplingQuery(uber, qryFormula, mpc.seenIDBCollections);
		//}
		//else {
			result = new MQuery(uber, qryFormula, mpc.seenIDBCollections);	
		//}
		

		//MEnvironment.outStream.println("Query created. Fmla = ");
		//qryFormula.accept(new FormulaIndentPrintV());
		
		// **********************************
		// Handle INCLUDE parameters
		// TODO remove this. include now at SHOW rather than EXPLORE level
		/*for(String dbname : includeMap.keySet())
		{
			// Indexings if any
			Set<List<String>> indexingsToAdd = new HashSet<List<String>>();
			
			try
			{				
			
				for(List<MTerm> indexing : includeMap.get(dbname))
				{
					// Can't add directly: user has provided a vector of identifiers.
					// addIDBOutputIndexing expects an _indexing_ into the tupled ordering.
					// Need to use our knowledge (from above) about ordering of the prefix.

					// prefixVarOrder

					List<String> nIndexing = new ArrayList<String>(indexing.size());
					for(String varname : indexing)
					{
						// Look for this var
						int ii = prefixVarOrder.indexOf(varname);
						// if not found, error.
						if(ii < 0)
						{
							throw new MGEArityMismatch("Unrecognized identifier in INCLUDE indexing: "+varname);
						}
						nIndexing.add(String.valueOf(ii+1));
					}

					indexingsToAdd.add(nIndexing);
				}							
			
				// Is this an edb or an idb?
				// EDB?
				if(uber.isSort(dbname) || uber.predicates.containsKey(dbname))
				{
					result.addEDBIncludes(dbname);
					for(List<String> nIndexing : indexingsToAdd)
						result.addEDBIncludesIndexing(dbname, nIndexing);
				}
				else
				{			
					// IDB
					String idbname = dbname;
					
					// add to general list
					result.addIDBOutputs(idbname);
					for(List<String> nIndexing : indexingsToAdd)
						result.addIDBOutputIndexing(dbname, nIndexing);
				} // end of IDB case
			
			}
			catch(MGEArityMismatch e)
			{
				MEnvironment.writeErrLine(e.getLocalizedMessage());
				return null;
			}
			
		}
*/

		result.debug_verbosity = iDebugLevel;

		// Remember our varvector and the inferred sorts (use this query's ID for the idb name).
		result.varOrderings.put(queryID, varOrdering);
		for (Variable v : varOrdering)
		{
			Expression theSort = uber.getSort(sortsForPublish.get(v.name())).rel;
			result.varSorts.put(v, theSort);
		}
		result.putIDB(queryID, idbFormula);
		
		
		MEnvironment.setLast(result);
		MEnvironment.setQueryForID(queryID, result);
		result.name = queryID; // used for saving query result, query knows its own ID
		
		// Populate exprToTerm
		MEnvironment.writeToLog("\nQuery created with ID "+queryID+". Terms were: "+mpc.terms);
		for(MTerm t : mpc.terms)
		{
			MEnvironment.writeToLog("\nQuery condition saw term: "+t.toString());
			result.vocab.exprToTerm.put(t.expr, t);
		}
		
		// MEnvironment.writeErrLine("\nQuery with vector: "+result.varOrdering+" sorts: "+result.varSorts);


		return result;
	}

	/*
	static void handleSortAssertions(MVocab voc, MExploreCondition mpc,
			Map<Variable, Expression> freeVars) throws MGEBadIdentifierName,
			MGEUnknownIdentifier {
		// MREPL.outStream.println(mpc.assertPossible);
		// MEnvironment.writeErrLine(mpc.assertNecessary);
		// MEnvironment.writeErrLine(mpc.assertAtomicNecessary);

		Set<List<Variable>> toDo = new HashSet<List<Variable>>(
				mpc.assertNecessary.keySet());
		toDo.addAll(mpc.assertAtomicNecessary.keySet());
		toDo.addAll(mpc.inferredSorts.keySet());

		for (List<Variable> varvector : toDo)
		{

			MCommunicator.writeToLog("\nProducing sorts for variable vector: "+varvector);

			Set<MVariableVectorAssertion> assertsN = mpc.assertNecessary
					.get(varvector);
			Set<MVariableVectorAssertion> assertsA = mpc.assertAtomicNecessary
					.get(varvector);
			Set<MVariableVectorAssertion> assertsP = mpc.assertSufficient
					.get(varvector);

			Set<MVariableVectorAssertion> allNecessary = new HashSet<MVariableVectorAssertion>(
					assertsN);
			allNecessary.addAll(assertsA);
			
			Set<MVariableVectorAssertion> inferredforthis = new HashSet<MVariableVectorAssertion>(mpc.inferredSorts.get(varvector));
			
			for (Variable v : varvector) 
			{
				// If this is a placeholder variable, ignore it
				if(mpc.knownPlaceholders.contains(v))
					continue;
				
				MSort runningSort;
				if(!freeVars.containsKey(v) || freeVars.get(v).equals(Expression.UNIV))
				{
					// Default to no info
					freeVars.put(v, Expression.UNIV);
					runningSort = null;
				}
				else
					runningSort = voc.getSortForExpression(freeVars.get(v));			
				
				MCommunicator.writeToLog("\n  Producing sort for variable: "+v);
				MCommunicator.writeToLog("\n  (Starting at: "+freeVars.get(v)+")");

				//MEnvironment.writeErrLine("var: "+v);
				
				// Start off with our well-formed formula inferences:
				for(MVariableVectorAssertion a : inferredforthis)
				{
					// If this isn't a sort, skip for now (test arity separately since
					// getSortForExpression tests NAMES. May have a sort and a >1-ary pred.
					if(a.sortExpression.arity() > 1)
						continue;
					
					// a.positive
					//a.sortExpression
					MSort theSort = voc.getSortForExpression(a.sortExpression);
					
					// if this isn't a sort, skip for now
					if(theSort == null)
						continue;
					
					if(runningSort == null)
					{					
						runningSort = theSort;
						freeVars.put(v, a.sortExpression);
						MCommunicator.writeToLog("\n    Taking inference (was null): "+a.sortExpression);
					}
					else
					{
						
						// TODO: we don't have intersection types...
						// So juse follow the first assertion we see
						// Only replace if we have a more specific assertion
						if(voc.isSubOrSubOf(theSort, runningSort))
						{
							runningSort = theSort;
							freeVars.put(v, a.sortExpression);
							MCommunicator.writeToLog("\n    Taking inference (was "+runningSort.name+"): "+a.sortExpression);
						}
						else
							MCommunicator.writeToLog("\n    Ignoring inference: "+a.sortExpression);
					}
				} // end for each inferredforthis				

				for (MVariableVectorAssertion a : allNecessary)
				{
					
					
					//MEnvironment.writeErrLine("assertion: "+a);

					if (!a.positive)
						continue;

					// If it is NECESSARY for x to have sort A
					// Then x must have sort A (or a subsort thereof)

					// If rel is a sort (which means this is a singleton vector)
					// just handle that assertion.
					if (voc.isSort(a.sortExpression.toString()))
					{
						MSort theSort = voc.getSortForExpression(a.sortExpression);
						if (runningSort == null)
						{
							runningSort = theSort;
							freeVars.put(v, a.sortExpression);
							MCommunicator.writeToLog("\n    Taking necessary assertion (was null): "+a.sortExpression);
						}
						else
						{
							// is theSort a subsort of the current running sort?
							// (need to check grandchildren etc. Also use subset constraints.)
							// If not, ignore

							//MEnvironment.writeErrLine("SUB? --- "+runningSort.subsorts);
							//MEnvironment.writeErrLine(theSort);
							if(voc.isSubOrSubOf(theSort, runningSort))
							{
								runningSort = theSort;
								freeVars.put(v, a.sortExpression);
								MCommunicator.writeToLog("\n    Taking necessary assertion (was "+runningSort.name+"): "+a.sortExpression);
							}
							else
								MCommunicator.writeToLog("\n    Ignoring necessary assertion: "+a.sortExpression);

							// else do nothing. (we COULD see which option had
							// the most children? TODO maybe

						} 
					} // end is sort

				} // end for each allnecessary

			} // end for each v in varvector

		}
		
		MCommunicator.writeToLog("\nDone. Assertions are: "+freeVars+")");
	}*/

	public void addIDBOutputs(String idbname)
	{
		//MEnvironment.errorStream.println("adding: "+idbname);
		if(!idbsToAddInFormula.containsKey(idbname))
			idbsToAddInFormula.put(idbname, new HashSet<List<String>>());
		//MEnvironment.errorStream.println(idbOutputIndexing);
	}	
	
	public void addIDBOutputs(Collection<String> idbnames)
	{
		for(String s : idbnames)
			addIDBOutputs(s);
	}

}
