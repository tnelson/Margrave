package edu.wpi.margrave;

import java.lang.management.ManagementFactory;
import java.lang.management.ThreadMXBean;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import kodkod.ast.Decls;
import kodkod.ast.Expression;
import kodkod.ast.Formula;
import kodkod.ast.Relation;
import kodkod.ast.Variable;
import kodkod.instance.Instance;
import kodkod.instance.Tuple;
import kodkod.instance.TupleSet;
import kodkod.instance.Universe;

public class MTuplingQuery extends MQuery
{
	// For instructing tupling to KEEP certain edb indexings, even if they do not appear in the condition
	protected HashMap<String, Set<List<String>>> edbsToForceTupling = new HashMap<String, Set<List<String>>>();;
	
	// Set to true to enable special tupling debug logging.
	//boolean timDebugMode = true;
	boolean timDebugMode = false;

	protected MInternalTupledQuery internalTupledQuery;
	
	MTuplingQuery(MVocab voc, Formula tupledFormula, Set<MIDBCollection> tupledIDBCollections)
	{
		super(voc, tupledFormula, tupledIDBCollections);
	}

	MTuplingQuery(MTuplingQuery previous) 
	throws MGEUnknownIdentifier, MGEBadQueryString, MGEArityMismatch 
	{
		super(previous);

		// Include EDBs to force tupling on.
		
		// "Deep enough" copy
		for(Map.Entry<String, Set<List<String>>> e : previous.edbsToForceTupling.entrySet())
			edbsToForceTupling.put(e.getKey(), new HashSet<List<String>>(e.getValue()));	
	}
	
	public MPreparedQueryContext runQuery()
	{
		ThreadMXBean mxBean = ManagementFactory.getThreadMXBean();
		long totalStartTime = mxBean.getCurrentThreadCpuTime();
		
		PrenexCheckV pren = new PrenexCheckV();
		boolean prenexExistential = myQueryFormula.accept(pren);
				
		MPreparedQueryContext result = doTupling(mxBean, totalStartTime, pren, prenexExistential);
				
		long cputime = (mxBean.getCurrentThreadCpuTime() - totalStartTime) / 1000000;
		
		if (debug_verbosity >= 1) {
			MEnvironment.writeOutLine("Returning Solutions object; time so far:");
			MEnvironment.writeOutLine("Preprocessing: " + msPreprocessingTime
					+ "ms, Tupling: " + internalTupledQuery.msTuplingTime
					+ "ms, pre-Kodkod query processing time: " + cputime
					+ "ms.");
		}
		
		return result;
	}
	
	MPreparedQueryContext doTupling(ThreadMXBean mxBean, 
			long start, PrenexCheckV pren, boolean prenexExistential)
	{
		
		long startTime;
		
		// Try to build a more optimal "tupled" query
		// This tupling always yields a size=1 bound, when it's possible
		// The price: More predicates.

		startTime = mxBean.getCurrentThreadCpuTime();
		if (debug_verbosity >= 2)
			MEnvironment.writeOutLine("DEBUG: Pre-tupling block Time: "
					+ (startTime - start) / 1000000);



		// ********************************************************************
		// If the user wants IDB output with tupling, need all the IDB
		// formulas to be
		// ********** QUANTIFIER FREE ***********.		
		boolean idbs_ok = tuplingCheckIDBsOK();
		// ********************************************************************



		if (debug_verbosity >= 2)
			MEnvironment.writeOutLine("Checked IDB output indexing. Time: "
					+ (mxBean.getCurrentThreadCpuTime() - startTime)
					/ 1000000);
		startTime = mxBean.getCurrentThreadCpuTime();

		List<String> indexedIDBNamesToOutput = new ArrayList<String>();			


		
		// ~~~~~~ !!! ~~~~~~
		
		// ***************************************************************
		// non-emptiness constraints (ONE, SOME)
		// cannot be converted to the same in the new vocab (or we'd get
		// additional existentials AFTER tupling the ones we have now.)
		
		// TODO tupling: one, some. constants!!

		
		

		// ********************************************************************
		// First check to see if the user query is prenex existential-only
		// COULD lift existentials if not in the prefix, but we don't (for
		// now) 
		// OPT lift existentials?
		// Also need to meet other requirements (certain axioms excluded,
		// for instance)

		
		if (idbs_ok && prenexExistential
				&& vocab.axioms.funcPartial.size() == 0
				&& vocab.axioms.funcTotal.size() == 0
				&& vocab.axioms.otherConstraintStrings.size() == 0)
		{
			// Construct new vocabulary and new query formula.
			// (new formula should be the query formula's translation only,
			// since the new vocab will contain new axioms over the new
			// signature.)

			// Tupling for a prenex existential formula: 
			// we get only one sort that represents the type of the entire tuple.
			// First we translate the formula, then add tupling and other axioms.

			if (debug_verbosity >= 2)
				MEnvironment.writeOutLine("DEBUG: Ran prenexcheck. Time: "
						+ (mxBean.getCurrentThreadCpuTime() - startTime)
						/ 1000000);
			startTime = mxBean.getCurrentThreadCpuTime();

			if (debug_verbosity >= 2)
				MEnvironment.writeOutLine("DEBUG: Starting matrix rewrite. ");
			
			MatrixTuplingV mtup = new MatrixTuplingV(pren, vocab);
							
			for(String edbname : edbsToForceTupling.keySet())
				for(List<String> indexing : edbsToForceTupling.get(edbname))
					mtup.forceIncludeEDB(edbname, indexing);		
			
			Formula tupledFormula = pren.matrix.accept(mtup);
			
			if (debug_verbosity >= 3) {
				// Show what the tupled sorts are in detail.
				MEnvironment.writeOutLine("DEBUG: Tupled sorts that appeared in the query body: ");
				for (MSort s : mtup.newvocab.sorts.values()) {
					String childstr = "(TOP!)";
					if (s.parents.size() > 0)
						childstr = "(PARENT: " + s.parents + ")";
					MEnvironment.writeOutLine("  " + s.name + " " + childstr);
				}
				
				MEnvironment.writeOutLine("");
				MEnvironment.writeOutLine("DEBUG: Tupled predicates that appeared in the query body (plus all equalities): ");
				for(String r : mtup.newvocab.predicates.keySet())
					MEnvironment.writeOutLine("  " + r);
				
				MEnvironment.writeOutLine("");
				MEnvironment.writeOutLine("DEBUG: State of mtup caches immediately after matrix tupling (before axioms): ");
				MEnvironment.writeOutLine(mtup.getCacheStringForDebug());
				MEnvironment.writeOutLine("");
			}

			if (debug_verbosity >= 2)
				MEnvironment.writeOutLine("DEBUG: Matrix tupling complete. Time: "
						+ (mxBean.getCurrentThreadCpuTime() - startTime)
						/ 1000000);
			startTime = mxBean.getCurrentThreadCpuTime();
						
			// replaces myIDBCollections in new query creation
			HashMap<String, MIDBCollection> tupledIDBCollections = new HashMap<String, MIDBCollection>();

			// Re-use visitors when possible
			HashMap<MIDBCollection, RelationAndTermReplacementV> initialVisitors = new HashMap<MIDBCollection, RelationAndTermReplacementV>();
			// HashMap equality is by entry equality is by key/value
			// equality, so it's ok to use them as keys here.
			HashMap<HashMap<Variable, Expression>, RelationAndTermReplacementV> indexingVisitors = new HashMap<HashMap<Variable, Expression>, RelationAndTermReplacementV>();

						
			// ******************
			// IDB OUTPUT ("INCLUDE")
			// ******************
			tuplingHandleINCLUDE(pren, indexedIDBNamesToOutput, mtup,
					tupledIDBCollections, initialVisitors, indexingVisitors);
			// *********** END IDBOUTPUT ****************
			
			
			if (debug_verbosity >= 2)
				MEnvironment.writeOutLine("DEBUG: Tupled "
						+ getIDBNamesToOutput().size() + " IDBs (possibly >1 indexing per). Time: "
						+ (mxBean.getCurrentThreadCpuTime() - startTime)
						/ 1000000);
			startTime = mxBean.getCurrentThreadCpuTime();
			
			// tupled sorts will be in mtup.newvocab

			/////////////////////////////////////////////////////////
			// Deal with the abstract constraint
			// For everything abstract in the pre-tupling vocab
			tuplingDoAbstract(mtup);
			/////////////////////////////////////////////////////////



			
			
			/////////////////////////////////////////////////////////
			// Sort of original predicates is respected
			// If P: AxB, P_1,3(z) ---> A_1(z) and B_3(z)
			// This is NOT enforced by the sig of the new predicate in
			// the new vocab. Need to assert these!
			Set<Formula> respected_1 = new HashSet<Formula>();
			tuplingRespectPredSorts(mtup, respected_1);			
			Formula respected_1_fmla = MFormulaManager.makeConjunction(respected_1);
			tupledFormula = MFormulaManager.makeAnd(tupledFormula, respected_1_fmla);

			if(timDebugMode)
				MCommunicator.writeToLog("\n\nrespected_1_fmla:\n"+respected_1_fmla.toString()+"\n");


			
			
					
			/////////////////////////////////////////////////////////
			// Prefix's sort is respected
			// This is "tupling axioms" or \beta in TN's thesis document
			// If we translated forsome x:A | forsome y:B to forsome z:UNIV
			// it must still hold that A_1(z) and B_2(z)
			Set<Formula> respected_2 = new HashSet<Formula>();
			tuplingRespectPrefixSorts(pren, mtup, respected_2);			
			Formula respected_2_fmla = MFormulaManager
					.makeConjunction(respected_2);
			tupledFormula = MFormulaManager.makeAnd(tupledFormula,
					respected_2_fmla);

			if(timDebugMode)
				MCommunicator.writeToLog("\n\nrespected_2_fmla:\n"+respected_2_fmla.toString()+"\n");

			
			if (debug_verbosity >= 2)
				MEnvironment.writeOutLine("DEBUG: Size of tupling axioms to make prefix sort respected: "
								+ measureFullFormulaSize(respected_2_fmla)
								+ " nodes, "
								+ measureFormulaReferences(respected_2_fmla)
								+ " references.");

			if (debug_verbosity >= 2)
				MEnvironment.writeOutLine("DEBUG: Prefix sort respected. Time: "
						+ (mxBean.getCurrentThreadCpuTime() - startTime)
						/ 1000000);
			startTime = mxBean.getCurrentThreadCpuTime();
			
		
			Set<String> nonempty = new HashSet<String>();
			nonempty.addAll(vocab.axioms.setsNonempty);
			nonempty.addAll(vocab.axioms.setsSingleton);

						
		
			
			//////////////////////////////////////////////////////////////
			// LONE (applies to sorts only)
			
			// For each i and j (i != j), and each lone P
			// where Pi and Pj both appear in the query:
			// Assert that (P_i and P_j) implies =i,j
			
			// (Even if =ij doesn't appear in the query proper!)
			
			// Since we now include all equality preds, no reason to
			// worry about transitivity here. 
			
			Set<String> lones = new HashSet<String>();
			lones.addAll(vocab.axioms.setsAtMostOne);
			lones.addAll(vocab.axioms.setsSingleton);

			Formula loneAxiomsFmla = tuplingDoLONE(pren, mtup, lones);
			tupledFormula = MFormulaManager.makeAnd(tupledFormula,
					loneAxiomsFmla);

			if(timDebugMode)
				MCommunicator.writeToLog("\n\nloneaxiomsfmla:\n"+loneAxiomsFmla.toString()+"\n");


			if (debug_verbosity >= 2)
				MEnvironment.writeOutLine("DEBUG: LONE handling. Time: "
						+ (mxBean.getCurrentThreadCpuTime() - startTime)
						/ 1000000);
			startTime = mxBean.getCurrentThreadCpuTime();


			/////////////////////////////////////////////////////////
			// FINALIZE *single* top-level sort. May have added new
			// sorts between the first call and this call, hence this
			// call. DO NOT ADD ANY SORTS AFTER THIS!
			try
			{
				mtup.newvocab.makeThisTheTopLevelSort(pren.tupleTypeName);
			} catch (MGEBadIdentifierName e) {
				throw new MGEBadQueryString(
						"Tupling error: Unable to create unified top-level sort: "
								+ pren.tupleTypeName);
			}

			if (debug_verbosity >= 2)
				MEnvironment.writeOutLine("DEBUG: Finalized top level sort. Time: "
						+ (mxBean.getCurrentThreadCpuTime() - startTime)
						/ 1000000);
			startTime = mxBean.getCurrentThreadCpuTime();

			
			
			
			
			/////////////////////////////////////////////////////////
			// EQUALITY AXIOMS
			// If two components are "equal" (are in a =i,j predicate)
			// then they are isomorphic w/r/t predicates and sorts
			Set<Formula> equalityAxioms = new HashSet<Formula>();

			// [[[ This used to add new equality predicates due to isomorphism. 
			//     It doesn't do that any more. ]]]
			
			Formula equalityAxiomsFmla = tuplingDoEqualityAxioms(pren, mtup,
					equalityAxioms);
			tupledFormula = MFormulaManager.makeAnd(tupledFormula,
					equalityAxiomsFmla);

			if(timDebugMode)
				MCommunicator.writeToLog("\n\nequalityAxiomsFmla:\n"+equalityAxiomsFmla.toString()+"\n");

						
			if (debug_verbosity >= 2)
				MEnvironment.writeOutLine("DEBUG: Size of tupling axioms to deal with express equality: "
								+ measureFullFormulaSize(equalityAxiomsFmla)
								+ " nodes, "
								+ measureFormulaReferences(equalityAxiomsFmla)
								+ " references. " + equalityAxioms.size() + " equality axioms overall.");

			if (debug_verbosity >= 2)
				MEnvironment.writeOutLine("DEBUG: Equality axioms done. Time: "
						+ (mxBean.getCurrentThreadCpuTime() - startTime)
						/ 1000000);
			startTime = mxBean.getCurrentThreadCpuTime();

			
			
			
			
			
			
			
			// ****************************
			// Disjointness (sorts only)
			Set<Formula> eqDisjAxioms = tuplingDoDisjointness(pren, mtup);

			Formula eqDisjAxiomsFmla = MFormulaManager
					.makeConjunction(eqDisjAxioms);
			tupledFormula = MFormulaManager.makeAnd(tupledFormula,
					eqDisjAxiomsFmla);

			if(timDebugMode)
				MCommunicator.writeToLog("\n\neqDisjAxiomsfmla:\n"+eqDisjAxiomsFmla.toString()+"\n");

			
			// !!! APRIL 2011 TN
			// This entire block is un-needed now, but leaving commented out just in case.
			
			// *********************************
			//     TOP LEVEL SORTS DISJOINTNESS
			// If A and B are top level sorts in old vocab, A_i and B_i will
			// NOT be top-level in new vocab,
			// which has a single unified top-level sort based on the
			// prefix.
			// However, we still want to require A & B to be disjoint in all
			// components! (and we must issue other axioms)

			/*
			int iTopLevelCount = 0;
			
			// Get set of old top-level sorts
			Set<String> oldtopnames = new HashSet<String>();
			for (MSort top : vocab.sorts.values()) {
				if (vocab.isSubtype(top))
					continue; // only do this for tops
				oldtopnames.add(top.name);
			}

			if(timDebugMode)
				MCommunicator.writeToLog("\n\noldtopnames:\n"+oldtopnames.toString()+"\n");

			
			// for each index, the set of those top-sorts w/ that index that
			// are used are all pairwise disj
			// {A_1, B_1}, {C_2, D_2, E_2}, etc.)
			for (int ii = 1; ii <= pren.qCount; ii++)
			{
				// For some reason the keys here are strings, not integers...
				String iiStr = Integer.toString(ii);
				Set<String> oldAppearingAti = mtup.getIndexingToPredicates(iiStr);
				oldAppearingAti.retainAll(oldtopnames);


				// ALL top-level sorts that appear at index i are disjoint
				// accumulate the list
				Set<String> new_i_names = new HashSet<String>();
				for(String sname : oldAppearingAti)
				{
					new_i_names.add(sname + "_" + ii);
				}
				if (new_i_names.size() > 1)
				{
					//MEnvironment.errorStream.println(new_i_names);
					mtup.newvocab.axioms.addConstraintDisjoint(new_i_names);
				}



				// ********************************
				// For every OTHER index j
				Set<Formula> eqTopLevelDisjAxioms = new HashSet<Formula>();
				for(int jj = ii+1; jj <= pren.qCount; jj++)
				{
					String jjStr = Integer.toString(jj);
					Set<String> oldAppearingAtj = mtup.getIndexingToPredicates(jjStr);
					oldAppearingAtj.retainAll(oldtopnames);

					// TODO really shouldn't represent these "impossible" equalities at all

					// For every pairing of an old at i and an old at j, if different they must imply !=
					for(String topi : oldAppearingAti)
						for(String topj : oldAppearingAtj)
						{
							if(topi.equals(topj)) continue;

							// Assert !=

							MSort ns1 = mtup.newvocab.fastGetSort(topi+"_"+ii);
							MSort ns2 = mtup.newvocab.fastGetSort(topj+"_"+jj);
							Relation eqpred = mtup.newvocab.getRelation("=_" + ii + ","+ jj);

							Formula thisAxiom = makeDisjointnessEqAxiom(mtup.newvar, ns1.rel, ns2.rel, eqpred);
							eqTopLevelDisjAxioms.add(thisAxiom);

							//MEnvironment.errorStream.println("Asserting "+thisAxiom);

						}

					//MEnvironment.errorStream.println(ii+","+jj+"; "+oldAppearingAti + " : "+oldAppearingAtj);



				}

				Formula eqTopLevelDisjAxiomsFmla = MFormulaManager.makeConjunction(eqTopLevelDisjAxioms);
				tupledFormula = MFormulaManager.makeAnd(tupledFormula,
						eqTopLevelDisjAxiomsFmla);
				iTopLevelCount += eqTopLevelDisjAxioms.size();

				if(timDebugMode)
					MCommunicator.writeToLog("\n\nTopLevelDisjAxioms (pt2):\n"+eqTopLevelDisjAxiomsFmla.toString()+"\n");

				

			}
			*/ 
			// end april 2011 commented out


			
			
			if (debug_verbosity >= 2) {
				MEnvironment.writeOutLine("DEBUG: Disjointness. Time: "
						+ (mxBean.getCurrentThreadCpuTime() - startTime)
						/ 1000000);
				//MEnvironment.writeOutLine("    New disjoints: " + iDisjCount);
				MEnvironment.writeOutLine("    Number of secondary eq/disj axioms added: "
								+ eqDisjAxioms.size());
				//MEnvironment.writeOutLine("    Number of top-level secondary eq/disj axioms added: "+iTopLevelCount);
			}
			startTime = mxBean.getCurrentThreadCpuTime();

			// *****************************
			// add NEW PREFIX to the formula
			// This must be done LAST!
			// *****************************
			Relation tupletyperel = mtup.newvocab
					.getSort(pren.tupleTypeName).rel;
			Decls d = MFormulaManager.makeOneOfDecl(mtup.newvar,
					tupletyperel);
			tupledFormula = MFormulaManager.makeExists(tupledFormula, d);

			if (debug_verbosity >= 2)
				MEnvironment.writeOutLine("Top sorts done. Time: "
						+ (mxBean.getCurrentThreadCpuTime() - startTime)
						/ 1000000);
			startTime = mxBean.getCurrentThreadCpuTime();

			/////////////////////////////////////////////////////////////////////////
			MInternalTupledQuery tupledQuery = new MInternalTupledQuery(mtup.newvocab, tupledFormula,
					tupledIDBCollections);
			
			if(timDebugMode) {
				MCommunicator.writeToLog("\n\nTupled query formula:\n"+tupledFormula.toString()+"\n");
			}
			
			tupledQuery.mySATFactory = mySATFactory;

			internalTupledQuery = tupledQuery; // allow access to creator's vocab, etc. for output
			internalTupledQuery.internalTuplingVisitor = mtup;
			tupledQuery.internalPreTuplingQuery = this;

			if (debug_verbosity >= 2) {
				MEnvironment.writeOutLine("Tupled query object created. Time: "
						+ (mxBean.getCurrentThreadCpuTime() - startTime)
						/ 1000000);
			}
			
			startTime = mxBean.getCurrentThreadCpuTime();

			if (debug_verbosity >= 2) {
				MEnvironment.writeOutLine("DEBUG: Tupling complete. ");

				// Get number of references in the two formulas:
				MEnvironment.writeOutLine("DEBUG: TUPLED FORMULA NODE INSTANCE COUNT ->"
								+ measureFormulaReferences(tupledFormula));
				MEnvironment.writeOutLine("DEBUG: ORIGINAL FORMULA NODE INSTANCE COUNT ->"
								+ measureFormulaReferences(myQueryFormula));

				// Get number of nodes (with repetition) in the two
				// formulas:
				MEnvironment.writeOutLine("DEBUG: TUPLED FORMULA TOTAL SIZE ->"
						+ measureFullFormulaSize(tupledFormula));
				MEnvironment.writeOutLine("DEBUG: ORIGINAL FORMULA TOTAL SIZE ->"
						+ measureFullFormulaSize(myQueryFormula));

				MEnvironment.writeOutLine("DEBUG: TUPLED Pred Count ->"
								+ (mtup.newvocab.sorts.size() + mtup.newvocab.predicates
										.size()));
				MEnvironment.writeOutLine("DEBUG: ORIGINAL Pred Count ->"
						+ (vocab.sorts.size() + vocab.predicates.size()));
				MEnvironment.writeOutLine("DEBUG: (Note that this doesn't distinguish arity: "+
						"adding binary or larger preds could be far worse even if unary pred count is reduced!");

				MEnvironment.writeOutLine("\nDEBUG: Calling runQuery on the new tupled query.");
			}

			tupledQuery.debug_verbosity = debug_verbosity;

			// Need to update with new list
			// But no explicit indexing needed, since we moved that into the relation names.
			tupledQuery.addIDBOutputs(indexedIDBNamesToOutput);

			tupledQuery.msPreprocessingTime = msPreprocessingTime;

			if (mxBean.isCurrentThreadCpuTimeSupported()) {
				tupledQuery.msTuplingTime = (mxBean.getCurrentThreadCpuTime() - start) / 1000000;
			}

			// Return the MQueryResult
			return tupledQuery.runQuery();
		}

		// Failed to tuple! (why?)

		if (!idbs_ok)
			throw new MUnsupportedFormulaException("Could not tuple: IDBs were not quantifier-free.");
		if(!prenexExistential)
			throw new MGETuplingFailure("Could not tuple: Query was not prenex existential.");
		if(vocab.axioms.funcPartial.size() > 0 || vocab.axioms.funcTotal.size() > 0)
			throw new MGETuplingFailure("Could not tuple: The vocabulary contained functional constraints.");
		if(vocab.axioms.otherConstraintStrings.size() > 0)
			throw new MGETuplingFailure("Could not tuple: Custom constraints are not allowed when tupling.");	
		
		throw new MGETuplingFailure("Unexpected tupling failure. Please notify the Margrave maintainer.");
	}

	private Set<Formula> tuplingDoDisjointness(PrenexCheckV pren,
			MatrixTuplingV mtup)
	{		
		/////////////////////////////////////////////////////////////
		// (1) If A and B are incomparable, but have a lower bound C, but C 
		// does not appear at index i, need to add C_i.
		
		// for each idx
		for (int iIndex = 1; iIndex <= pren.qCount; iIndex++)
		{
			String strIndex = String.valueOf(iIndex);
			String strIndexWithUnderscore = "_" + strIndex;

			Set<String> oldPredsForThisIndex = mtup.getIndexingToPredicates(strIndex);

			// OPT Naive algorithm, quadratic in number of appearing sorts...
			
			// for each SORT predicate tupled in this index 
			for (String oldPredName1 : oldPredsForThisIndex)
			{				
				if(!mtup.oldvocab.fastIsSort(oldPredName1))
					continue; 
				
				for(String oldPredName2 : oldPredsForThisIndex)
				{
					if(!mtup.oldvocab.fastIsSort(oldPredName2))
						continue; 
					
					// If pred1 is related to pred2, the same holds in new vocab.
					// If pred 1 is NOT related to pred2, and they have a LB, need to add the LB.
					
					// Can't do this in the visitor since we only add if the LB doesn't appear...

					boolean related = mtup.oldvocab.isSubOrSubOf(oldPredName1, oldPredName2);
					
					Set<MSort> subs1 = mtup.oldvocab.buildSubSortSet(oldPredName1);
					Set<MSort> subs2 = mtup.oldvocab.buildSubSortSet(oldPredName2);
					boolean haveLB = !Collections.disjoint(subs1, subs2);
					
					// If related, can happily ignore disjointness
					if(related)
						continue;										
					
					// Don't add a new LB if there already is one (in the NEW vocab):
					if(mtup.newvocab.possibleOverlap(oldPredName1+strIndexWithUnderscore,
							                         oldPredName2+strIndexWithUnderscore))
						continue;
					
					// LB exists in the OLD vocab, but not the NEW one.
					if(haveLB)
					{
						String newPredName = oldPredName1+"-"+oldPredName2+"-LB"+strIndexWithUnderscore;
						
						mtup.newvocab.addSubSort(newPredName, oldPredName1+strIndexWithUnderscore);
						mtup.newvocab.addSubSort(newPredName, oldPredName2+strIndexWithUnderscore);
					}
															
				} 				
			}
		}
		
		/////////////////////////////////////////////////////////////
		// (2) A_1 and A_2 are not necessarily disjoint (unless x_1 and x_2
		// have disjoint types). Need to add a "sink" lower bound for all
		// such situations. NOT covered by prior block since i != j.
		for (int ii = 1; ii <= pren.qCount; ii++)
		{
			String strIndex1 = String.valueOf(ii);
			Set<String> oldPredsAtIndex1 = mtup.getIndexingToPredicates(strIndex1);
			
			// start 1 higher than ii; shave off lower-right half of grid
			for (int jj = ii+1; jj <= pren.qCount; jj++)
			{
				String strIndex2 = String.valueOf(jj);
				Set<String> oldPredsAtIndex2 = mtup.getIndexingToPredicates(strIndex2);
				
				oldPredsAtIndex2.retainAll(oldPredsAtIndex1);
				for(String oldPredName : oldPredsAtIndex2)
				{
					// Add new sort that is LB for both.
					// Which index does this belong to? For now, just use ii.
					
					String newPredName = oldPredName+"-LB-"+ii+"-"+jj+"_"+ii;
					
					mtup.newvocab.addSubSort(newPredName, oldPredName+"_"+ii);
					mtup.newvocab.addSubSort(newPredName, oldPredName+"_"+jj);
				}												
			}
		}
		
		Set<Formula> eqDisjAxioms = new HashSet<Formula>();
		
		/////////////////////////////////////////////////////////////
		// (3) If A_i disj B_j [after the above lower-bounds have been
		// added!], assert A_i and B_j --> not =_i,j
		for (int iIndex = 1; iIndex <= pren.qCount; iIndex++)
		{
			String strIndex = String.valueOf(iIndex);
			String strIndexWithUnderscore = "_" + strIndex;

			Set<String> oldPredsForThisIndex = mtup.getIndexingToPredicates(strIndex);
			
			for(String oldPredName : oldPredsForThisIndex)
			{
				if(!mtup.oldvocab.fastIsSort(oldPredName))
					continue;
				MSort oldSort = mtup.oldvocab.fastGetSort(oldPredName); 
				Set<MSort> disjoints = mtup.oldvocab.getConciseDisjointSorts(oldSort);				
				
				// TODO *DO* Condensed axioms disjs suffice?
				// symm?
				
				MSort newSort = mtup.newvocab.getSort(oldPredName+ strIndexWithUnderscore);
				
				for(MSort oldDisjoint : disjoints)
				{
					for (int iIndex2 = iIndex + 1; iIndex2 <= pren.qCount; iIndex2++)
					{
						
						String new2 = oldDisjoint.name + "_" + iIndex2;

						// second index -- is this sort tupled into it?
						// don't validate
						if (!mtup.newvocab.fastIsSort(new2))
							continue;
						
						MSort ns1 = newSort;
						MSort ns2 = mtup.newvocab.getSort(new2);
						Relation eqpred = mtup.newvocab.getRelation("=_" + iIndex + "," + iIndex2);

						Formula thisAxiom = makeDisjointnessEqAxiom(mtup.newvar, ns1.rel, ns2.rel, eqpred);
						eqDisjAxioms.add(thisAxiom);
					} // end for iIndex2	
				} // end for each disjoint sort			
			} // end for each old pred at this index								
		} // end for each index iIndex

		
	
		
		
		/// !!! XXX OLD CODE TO BE REMOVED
		/////////////////////////////////////////////////////////////
		/////////////////////////////////////////////////////////////
		/////////////////////////////////////////////////////////////
		
		// disjointness: same construction as sorts themselves
		// Original form: A disj (B1, B2, B3...)

		// for each index
		// for each pred tupled into this index
		// copy disjs of that pred originally (names)
		// intersect w/ preds tupled into this index

		// is it possible for an implied disjointness to be lost when
		// tupling? no, because we carry over all parents 
		// (and constrained supersorts).

		// Avoid reachable check in addConstraintDisjoint by calculating
		// and copying directly.
		// mtup.newvocab.axioms.axiomDisjoints = disjoints we calculate
		// in this loop

/*		HashMap<MSort, Set<MSort>> newDisjoints = new HashMap<MSort, Set<MSort>>();
		int iDisjCount = 0;
		

		for (int iIndex = 1; iIndex <= pren.qCount; iIndex++)
		{
			String strIndex = String.valueOf(iIndex);
			String strIndexWithUnderscore = "_" + strIndex;

			Set<String> oldPredsForThisIndex = mtup.getIndexingToPredicates(strIndex);

			for (String oldPredName : oldPredsForThisIndex)
			{

				MSort oldSort = vocab.getSort(oldPredName);

				// sorts disjoint from oldSort in old vocab
				Set<MSort> oldDisjoints = vocab.axioms.axiomDisjoints.get(oldSort);

				if (oldDisjoints == null)
					continue;

				MSort newSort = mtup.newvocab.getSort(oldPredName
						+ strIndexWithUnderscore);
				newDisjoints.put(newSort, new HashSet<MSort>());

				for (MSort oldDisjoint : oldDisjoints)
				{
					// *******************************************
					// DISJ AXIOM TYPE 1 (equality + disj)

					for (int iIndex2 = iIndex + 1; iIndex2 <= pren.qCount; iIndex2++)
					{
						String new2 = oldDisjoint.name + "_" + iIndex2;

						// second index -- is this sort tupled into it?
						// don't validate
						if (!mtup.newvocab.fastIsSort(new2))
							continue;


						MSort ns1 = newSort;
						MSort ns2 = mtup.newvocab.getSort(new2);
						Relation eqpred = mtup.newvocab.getRelation("=_" + iIndex + "," + iIndex2);

						Formula thisAxiom = makeDisjointnessEqAxiom(mtup.newvar, ns1.rel, ns2.rel, eqpred);
						eqDisjAxioms.add(thisAxiom);

					}

					// *******************************************
					// DISJ AXIOM TYPE 2 (disjointness axioms in the new vocabulary)

					// is the other sort tupled for this index?
					if (oldPredsForThisIndex.contains(oldDisjoint.name))
					{
						MSort newDisjoint = mtup.newvocab
								.getSort(oldDisjoint.name
										+ strIndexWithUnderscore);
						// boolean added =
						// mtup.newvocab.axioms.addConstraintDisjoint(newSort,
						// newDisjoint);
						newDisjoints.get(newSort).add(newDisjoint);
						// if(added)
						iDisjCount++;
					}
				}

			}
		} // end for each index
		mtup.newvocab.axioms.axiomDisjoints = newDisjoints;
*/
		// TODO is it safe to just call vocab.axioms.axiomsDisjoints.get
		// if we store only one way?
		return eqDisjAxioms;
	}

	private Formula tuplingDoEqualityAxioms(PrenexCheckV pren,
			MatrixTuplingV mtup, Set<Formula> equalityAxioms) {
		// Set --> List so we can iterate in order (double-loop inside)
		List<String> equalsNeeded = new ArrayList<String>(
				mtup.equalAxiomsNeeded);
		for(int iNeeds = 0; iNeeds < equalsNeeded.size();iNeeds++)
		{
			String needs = equalsNeeded.get(iNeeds);

			// Needs: "=_"+leftidx+","+rightidx
			// For a specific =_{i,j}
			int needsLastComma = needs.lastIndexOf(",");
			String rightidx = needs.substring(needsLastComma + 1);
			String leftidx = needs.substring(2, needsLastComma);

			Formula isoconj = makePredsIndistinguish(vocab, mtup, leftidx, rightidx);
			Relation eqrel = mtup.newvocab.predicates.get(needs).rel;
			Formula eqFormula = MFormulaManager.makeAtom(mtup.newvar,
					eqrel);

			// Simulated equality leads to isomorphism
			if (!leftidx.equals(rightidx))
			{
				Formula isoimp = MFormulaManager.makeImplication(eqFormula, isoconj);
				if(!Formula.TRUE.equals(isoimp))
					equalityAxioms.add(isoimp);
			}


			// *******************************
			// state transitivity: =1,2 and =3,2 imply =1,3
			for(int iNeedOther = iNeeds+1; iNeedOther < equalsNeeded.size();iNeedOther++)
			{
				String other = equalsNeeded.get(iNeedOther);

				int otherLastComma = other.lastIndexOf(",");
				String rightidx2 = other.substring(otherLastComma + 1);
				String leftidx2 = other.substring(2, otherLastComma);
				
				try
				{
					// Is there a consequent for this transitivity implication?
					// (Always should be yes, now)
					String thirdpred = getThirdEq(mtup.newvocab,
							leftidx, rightidx, leftidx2, rightidx2,
							pren);						

					if (thirdpred.length() > 0)
					{
						Relation eqRel2 = mtup.newvocab.predicates.get(other).rel;
						Relation eqRel3 = mtup.newvocab.predicates.get(thirdpred).rel;

						// =ij and =jk ---> =ik
						Formula eqFormula2 = MFormulaManager.makeAtom(
								mtup.newvar, eqRel2);
						Formula eqFormula3 = MFormulaManager.makeAtom(
								mtup.newvar, eqRel3);
						Formula eqTrans = MFormulaManager
								.makeImplication(
										MFormulaManager.makeAnd(
												eqFormula, eqFormula2),
										eqFormula3);
						equalityAxioms.add(eqTrans);


					} // end if appropriate third pred

				} catch (MGEBadIdentifierName e) {
					throw new MGEBadQueryString(
							"Tupling error: unable to state transitivity of equality w/ indices: "
									+ leftidx + "," + rightidx
									+ " and " + leftidx2 + ","
									+ rightidx2);
				}

			} // end for each =ij (other)

		} // end for each =ij (needs)



		Formula equalityAxiomsFmla = MFormulaManager
				.makeConjunction(equalityAxioms);
		return equalityAxiomsFmla;
	}

	private Formula tuplingDoLONE(PrenexCheckV pren, MatrixTuplingV mtup, Set<String> lones)
	{
		
		//int iLoneEqCounter = 0;
		HashSet<Formula> lone_formulas = new HashSet<Formula>();
		Set<Formula> ij_triggers = new HashSet<Formula>();
		long triggerCount = 0;

		// For each pair of distinct indices
		for (int ii = 1; ii <= pren.qCount; ii++)
		{				
			for (int jj = ii + 1; jj <= pren.qCount; jj++)
			{
				// What LONE sorts were shared between ii and jj? 
				// Much better than checking each lone predicate name.
				// (caching is good. was: 984 ms now: 15ms)
				// will still take a while if there are a lot of shared
				// preds, of course.
				
				Set<String> sharedOldPredNames = new HashSet<String>(mtup.getIndexingToPredicates(String.valueOf(ii)));
				sharedOldPredNames.retainAll(mtup.getIndexingToPredicates(String.valueOf(jj)));
				sharedOldPredNames.retainAll(lones);

				// Build a list of antecedents for consequent =i,j. Each shared sort has a chance to be 
				// an antecedent. Trigger list holds all of them.
				
				// reset trigger list
				ij_triggers.clear();

				for (String lonesort : sharedOldPredNames)
				{
					try
					{
						Relation pi = mtup.newvocab
								.getRelation(lonesort + "_" + ii);
						Relation pj = mtup.newvocab
								.getRelation(lonesort + "_" + jj);

						// If so, assert P_i and P_j --> =ij

						Formula trigger1 = MFormulaManager.makeAtom(
								mtup.newvar, pi);
						Formula trigger2 = MFormulaManager.makeAtom(
								mtup.newvar, pj);
						ij_triggers.add(MFormulaManager.makeAnd(
								trigger1, trigger2));
						triggerCount++;

					} catch (MGEUnknownIdentifier e) {
					} // fall through

				} // each lone

				// Did we get any triggers (antecedents)?
				if (ij_triggers.size() > 0) 
				{
					Formula disj_of_triggers = MFormulaManager.makeDisjunction(ij_triggers);

					// This block below is pointless now that we have all equality predicates by default:
					// [[[
					// Is this a new equality pred that hasn't appeared
					// in the query itself?
					String eqpredname = "=_" + ii + "," + jj;
					
					Relation eqrel = mtup.newvocab.predicates.get(eqpredname).rel;
					Formula iseq = MFormulaManager.makeAtom(mtup.newvar, eqrel);
					lone_formulas.add(MFormulaManager.makeImplication(disj_of_triggers, iseq));

					if (debug_verbosity >= 2) {
						MEnvironment.writeOutLine("DEBUG: LONE resulted in ---> ");
						MEnvironment.writeOutLine(disj_of_triggers
								+ "\n  THEN " + iseq);
					}
				}

			} // jj
		} // ii

		Formula loneAxiomsFmla = MFormulaManager
				.makeConjunction(lone_formulas);
		return loneAxiomsFmla;
	}

	private void tuplingRespectPrefixSorts(PrenexCheckV pren,
			MatrixTuplingV mtup, Set<Formula> respected_2) {
		int index = 0;
		for (String sortname : pren.tupleTypeConstruct.split(" "))
		{
			// Always increment, even if a continue is reached.
			// (Starts with 1, technically)
			index++;
			
			try
			{			
				// Do nothing. No axiom needed, since no sort for this element.
				if("univ".equals(sortname))
					continue;

				// Perhaps this indexed sort hasn't been mentioned in the formula matrix. Add it.
				String newsortname = sortname + "_" + index;
				if (!mtup.newvocab.isSort(newsortname))
				{
					mtup.newvocab.addSort(newsortname);

					// must also cache this sort in mtup's 2 caches

					String iiStr = Integer.toString(index);

					//mtup.addToCaches(sortname, "_"+index, newsortname);							
					// May need to add parents too
					mtup.addSortWithSupers(mtup.newvocab, vocab, sortname, "_"+index);
				}

				// Assert the axiom for this index
				Relation r = mtup.newvocab.getRelation(newsortname);
				respected_2.add(MFormulaManager.makeAtom(mtup.newvar, r));

			} catch (MGEBadIdentifierName e) {
				throw new MGEBadQueryString("Tupling error: Relation "
						+ sortname + "_" + index
						+ " was not created properly.");
			}
		
		} // end of for each index's sort				
	}

	private void tuplingRespectPredSorts(MatrixTuplingV mtup,
			Set<Formula> respected_1) {
		for(String origpred : vocab.predicates.keySet())
		{
			Set<String> newindexings = mtup.getPredicateToIndexings(origpred);
		
			MPredicate theOrigPred = vocab.predicates.get(origpred);
																	
			List<MSort> origtypes = theOrigPred.type;
			
			for(String newindexing : newindexings)
			{
				String idxStr = newindexing.substring(newindexing.lastIndexOf("_") + 1);
				String[] indices = idxStr.split(",");
			
				Set<Formula> forthisindexing = new HashSet<Formula>();
				
				for(int ii = 0; ii < indices.length ;ii++)
				{
					String newsortname = origtypes.get(ii).name + "_" + indices[ii];

					// Perhaps this indexed sort hasn't been mentioned in the formula matrix. Add it.
					// (and its parents, if need be.)
					if (!mtup.newvocab.isSort(newsortname))
						mtup.addSortWithSupers(mtup.newvocab, vocab, origtypes.get(ii).name, "_"+indices[ii]);

					// z in A_2
					Relation r = mtup.newvocab.getRelation(newsortname);
					forthisindexing.add(MFormulaManager.makeAtom(mtup.newvar, r));
				}
				
				
				
				Relation newpred = mtup.newvocab.getRelation(newindexing);
				Formula antecedent = MFormulaManager.makeAtom(mtup.newvar, newpred);
				Formula consequent = MFormulaManager.makeConjunction(forthisindexing);
				respected_1.add( MFormulaManager.makeImplication(antecedent, consequent));
				
				//if(timDebugMode)
					MCommunicator.writeToLog("\n\nrespect_1 for antecedent:\n"+antecedent+" was "+consequent+"\n");											
				//MEnvironment.writeErrLine("\n\nrespect_1 for antecedent:\n"+antecedent+" was "+consequent+"\n");
			}
			
		}
	}

	private void tuplingDoAbstract(MatrixTuplingV mtup) {
		for (String relName : vocab.assertSortOrdering(vocab.axioms.setsAbstract))
		{
			MSort oldSort = vocab.getSort(relName);

			// Get the list of tupled extensions of relName
			Set<String> newPreds = mtup.getPredicateToIndexings(relName);
								
			if (debug_verbosity >= 3)
				MEnvironment.writeOutLine("DEBUG: Examining abstract sort "+relName);
			
			if (newPreds == null)
				continue;

			for (String newName : newPreds)
			{
				// Carry over the abstract constraint.

				String idxStr = newName.substring(newName.lastIndexOf("_") + 1);
				
				// Child sorts will *always* be added at indexing <i> if the 
				// (abstract) parent appears at indexing <i>. We may need to 
				// add children if any have not appeared already:
				for (MSort oldChild : oldSort.subsorts)
				{
					// Add if missing
					if (!mtup.newvocab.isSort(oldChild.name + "_" + idxStr))
					{
						String newchildname = oldChild.name + "_" + idxStr;
						mtup.newvocab.addSubSort(newName, newchildname);
						
						// Not using mtup.addSortWithSupers since here we already have the parent
						mtup.addToCaches(oldChild.name, "_"+idxStr, newchildname);
						
						if (debug_verbosity >= 3)
							MEnvironment.writeOutLine("  DEBUG: Abstract sort "
									+ relName + " at index " + idxStr
									+ ": Forced addition of tupled child "
									+ oldChild.name + "_" + idxStr);
						
					}
					else
					{
						if (debug_verbosity >= 3)
							MEnvironment.writeOutLine("  DEBUG: Abstract sort "
									+ relName + " at index " + idxStr
									+ ": child "
									+ oldChild.name + "_" + idxStr +" already existed.");
					}


				}


				
				// Carry the constraint over to the new vocabulary:
				mtup.newvocab.axioms.addConstraintAbstract(newName);
			}
		} // end for everything abstract (in proper order)
	}

	private boolean tuplingCheckIDBsOK()
	{
		ContainsQuantifiersCheckV qfreecheck = new ContainsQuantifiersCheckV();
		
		for (String idbname : getIDBNamesToOutput())
		{
			// Make certain that we have an indexing for this IDB.
			if (!idbsToAddInFormula.containsKey(idbname))
				throw new MGEUnknownIdentifier(
						"Query with tupling enabled lacked indexing for IDB to output: "
								+ idbname);

			// Get the IDB formula
			String[] split = idbname.split(MEnvironment.sIDBSeparatorRegExp);
			String polName = split[0].toLowerCase();

			if (split.length < 2)
				throw new MGEUnknownIdentifier(
						"IDB names for output must be of the form policyname:idbname.");

			String internal_idbname = idbname.substring(
					polName.length() + 1).toLowerCase(); // include :

			if (!myIDBCollections.containsKey(polName))
				throw new MGEUnknownIdentifier("Unknown IDB collection: "
						+ polName);
			MIDBCollection coll = myIDBCollections.get(polName);

			if (!coll.containsIDB(internal_idbname))
				throw new MGEUnknownIdentifier("Unknown IDB: "
						+ internal_idbname + " in collection: " + polName);

			Formula idbformula = coll.getIDB(internal_idbname);

			// If there is a quantifier...
			if (idbformula.accept(qfreecheck))
				return false;							
		}
		return true;
	}

	private void tuplingHandleINCLUDE(
			PrenexCheckV pren,
			List<String> indexedIDBNamesToOutput,
			MatrixTuplingV mtup,
			HashMap<String, MIDBCollection> tupledIDBCollections,
			HashMap<MIDBCollection, RelationAndTermReplacementV> initialVisitors,
			HashMap<HashMap<Variable, Expression>, RelationAndTermReplacementV> indexingVisitors) {
		if (getIDBNamesToOutput().size() > 0)
		{
			for (String idbname : getIDBNamesToOutput())
			{
				// We want to tuple this idb formula, so it can be
				// included in output.
				idbname = idbname.toLowerCase();

				// Get the IDB formula
				int firstColon = idbname.indexOf(':');
				String polName = idbname.substring(0, firstColon);
				String internal_idbname = idbname
						.substring(firstColon + 1); // include :

				// These should all be valid, would have errored out
				// beforehand otherwise.
				MIDBCollection coll = myIDBCollections.get(polName);
				Formula originalIDBFormula = coll.getIDB(internal_idbname);

				// ***
				// Convert from the original policy/custom's vocab to
				// the ubervocab being used for this query.
				// !!! TODO is this even needed anymore?
				RelationAndTermReplacementV vis;
				if (initialVisitors.containsKey(coll))
					vis = initialVisitors.get(coll);
				else {
					vis = MIDBCollection.getReplacementVisitor(
							coll.vocab, vocab);
					initialVisitors.put(coll, vis);
				}
				originalIDBFormula = originalIDBFormula.accept(vis);


				// for each indexing
				Set<List<String>> indexings = idbsToAddInFormula.get(idbname);
				for(List<String> user_indexing : indexings)
				{
					// Fresh copy of the original formula
					Formula idbformula = originalIDBFormula;


					// Assemble suffix for this indexing
					String idbnameSuffix = "_";
					boolean first = true;
					for(String iStr : user_indexing)
					{
						if(first)
						{
							idbnameSuffix += iStr;
							first = false;
						}
						else
							idbnameSuffix += ","+iStr;
					} // end String iStr : user_indexing

					HashMap<Variable, Expression> newvars = new HashMap<Variable, Expression>();


					// MREPL.outStream.println("pren.revIndexing, then user_indexing for idb: "+idbname);
					// MREPL.outStream.println(pren.revIndexing);
					// MREPL.outStream.println(user_indexing);

					int ii = 0;
					for (Variable v : coll.varOrderings.get(idbname))
					{
						// We are at the ii-th variable in the IDB's
						// input vector.
						// If the user-supplied ordering was
						// 1,2,3,...etc, we could just:
						// Variable v2 =
						// pren.revIndexing.get(String.valueOf(ii));
						// Instead we need to use the value the user
						// gave.
						// (Note: revindexing is 1-based, which is why
						// the user-supplied ordering is.)

						int iIndex = Integer.parseInt(user_indexing
								.get(ii));
						Variable v2 = pren.revIndexing.get(String
								.valueOf(iIndex));

						// If the query didn't provide enough variables
						// to support this:
						if (v2 == null)
							throw new MGEArityMismatch("Could not tuple " + idbname + " for output; query prefix was insufficiently wide. ");

						// Replace v with v2, if they are different objects
						if (v != v2)
							newvars.put(v, v2);

						ii++;
					} // end Variable v : coll.varOrdering


					// ***
					// Still placeholder vars at this point, just the
					// correct ones.
					// It still remains to put this IDB in the context of
					// the tuple vector.
					// (Placeholder reqVars need to be replaced with the
					// quantified vars from the
					// query. Note that the IDB may not use them all.)
					if (newvars.size() > 0) // Don't even bother if there's
											// no replacements to make
					{
						RelationAndTermReplacementV renaming;
						if (indexingVisitors.containsKey(newvars))
						{
							renaming = indexingVisitors.get(newvars);
						}
						else
						{
							renaming = new RelationAndTermReplacementV(
									new HashMap<Relation, Relation>(),
									newvars);
							indexingVisitors.put(newvars, renaming);
						}

						idbformula = idbformula.accept(renaming);
					}

					// ***
					// Use same MatrixTuplingV as we used for the main
					// formula. (Re-use cached work.)
					Formula new_idbformula = idbformula.accept(mtup);
					// MREPL.outStream.println(idbformula);
					// MREPL.outStream.println(new_idbformula);

					// Store this new formula for the new query's use.
					if (!tupledIDBCollections.containsKey(polName))
						tupledIDBCollections.put(polName,
								new MInternalIDBCollection(polName,
										mtup.newvocab));

					String new_internal_name = internal_idbname + idbnameSuffix;

					indexedIDBNamesToOutput.add(polName+MEnvironment.sIDBSeparator+new_internal_name);

					tupledIDBCollections.get(polName).putIDB(new_internal_name, new_idbformula);
					tupledIDBCollections.get(polName).varOrderings.put(new_internal_name, new ArrayList<Variable>());
					tupledIDBCollections.get(polName).varOrderings.get(new_internal_name).add(mtup.newvar);
					// ^^^ No sort yet (calculated below), but we know the variable
					
					

					//MEnvironment.errorStream.println("Tupling IDB: "+new_internal_name);
					//MEnvironment.errorStream.println(new_idbformula.hashCode());

					//MEnvironment.errorStream.println(tupledIDBCollections.keySet());
					//MEnvironment.errorStream.println(tupledIDBCollections.get(polName).idbs);
					//MEnvironment.errorStream.println(new_internal_name);


				} // end for each indexing on this name
			} // for each idbname to output

		} // if we have idbs to include in output
	}

	private Formula makeDisjointnessEqAxiom(Variable newvar, Relation rel,
			Relation rel2, Relation eqpred)
	throws MGEManagerException
	{

		Formula atom1 = MFormulaManager.makeAtom(newvar, rel);
		Formula atom2 = MFormulaManager.makeAtom(newvar, rel2);
		Formula literal3 = MFormulaManager.makeNegation(
				MFormulaManager.makeAtom(newvar, eqpred));

		return MFormulaManager
				.makeImplication(MFormulaManager
						.makeAnd(atom1, atom2),
						literal3);
	}


	protected String getThirdEq(MVocab newvocab, String leftidx,
			String rightidx, String leftidx2, String rightidx2, PrenexCheckV vis)
			throws MGEBadIdentifierName {
		// the two indices for the third equality pred (if it exists)
		String left3, right3;

		if (leftidx.equals(leftidx2)) {
			// rightidx and rightidx2
			left3 = rightidx;
			right3 = rightidx2;
		} else if (rightidx.equals(leftidx2)) {
			// leftidx and rightidx2
			left3 = leftidx;
			right3 = rightidx2;
		} else if (leftidx.equals(rightidx2)) {
			// rightidx and leftidx2
			left3 = rightidx;
			right3 = leftidx2;
		} else if (rightidx.equals(rightidx2)) {
			// leftidx and leftidx2
			left3 = leftidx;
			right3 = leftidx2;
		} else
			// otherwise no match between these = preds, so no transitivity to
			// state
			return "";


		// There's a match: Does one of the two possible pred names exist?
		if (newvocab.predicates.containsKey("=_" + left3 + "," + right3))
			return "=_" + left3 + "," + right3;
		if (newvocab.predicates.containsKey("=_" + right3 + "," + left3))
			return "=_" + right3 + "," + left3;

		// obsolete April 2010
		/*
		// There's a match but no current equality predicate to cover it.
		// Create one.
		newvocab.addPredicate("=_" + left3 + "," + right3, vis.tupleTypeName); // name,
																				// not
																				// construct
*/
		return "";
	}

	protected Formula makePredsIndistinguish(MVocab old, MatrixTuplingV mtup,
			String leftidx, String rightidx)
			throws MGEManagerException, MGEBadIdentifierName, MGEUnknownIdentifier
	{
		Set<Formula> isoconj_formulas = new HashSet<Formula>();
		
		// What must hold if x_i = x_j?
		
		// First: which new predicates even use index i?
		Set<String> newUseI = mtup.getNewRelationsUsingIndex(leftidx);
		
		// Start pulling out elements of newUseI and deal with them. 
		// One iteration MAY deal with multiple elements. Keep track of double-dipping.
		Set<String> done = new HashSet<String>();
		for(String usesI : newUseI)
		{
			if(timDebugMode)
				MEnvironment.writeErrLine("Computing axioms for predicate: "+ usesI);
			
			
			// dealt with already?
			if(done.contains(usesI))
				continue;			
			done.add(usesI);
			
			// Extract indexing
			String idxStr = usesI.substring(usesI.lastIndexOf("_") + 1);
			String oldpredstr = usesI.substring(0, usesI.lastIndexOf("_"));
			String[] indexing = idxStr.split(",");
			
			// Easy case: only one index (sort or unary predicate)
			if(indexing.length == 1)
			{
				MSort sort1 = mtup.newvocab.fastGetSort(usesI);
				MSort sort2 = mtup.newvocab.fastGetSort(oldpredstr + "_" + rightidx);
				
				if(sort1 != null && sort2 != null)
				{
					// If we get this far, this sort was tupled for both indices.
					Formula insort1 = MFormulaManager.makeAtom(mtup.newvar, sort1.rel);
					Formula insort2 = MFormulaManager.makeAtom(mtup.newvar, sort2.rel);
					isoconj_formulas.add(MFormulaManager.makeIFF(insort1, insort2));
				}
				else if(sort1 == null)
				{
					// Wasn't a sort, but a unary predicate
					Relation rel1 = mtup.newvocab.getRelation(usesI);
					Relation rel2 = mtup.newvocab.getRelation(oldpredstr + "_" + rightidx);
					
					if(rel1 != null && rel2 != null)
					{
						Formula inpred1 = MFormulaManager.makeAtom(mtup.newvar, rel1);
						Formula inpred2 = MFormulaManager.makeAtom(mtup.newvar, rel2);
						isoconj_formulas.add(MFormulaManager.makeIFF(inpred1, inpred2));
					}
				} 
			} // end of easy unary case
			
			else // start of harder >1-ary case
			{
				// For each possible mutant predicate name (substituting i's for j's in the indexing)
				// If it appears, assert <-> and add to isoconj_formulas (and mark it as done)
			
				Set<String> toMakeEquiv = getUsedMutantPredicates(mtup, leftidx, rightidx, oldpredstr, indexing);
				
				// mutations will include the current predicate. don't assert tautologies.
				toMakeEquiv.remove(usesI);
				
				for(String mutant : toMakeEquiv)
				{

					done.add(mutant);
					
					Relation rel1 = mtup.newvocab.getRelation(usesI);
					Relation rel2 = mtup.newvocab.getRelation(mutant);
					
					if(rel1 != null && rel2 != null)
					{
						Formula inpred1 = MFormulaManager.makeAtom(mtup.newvar, rel1);
						Formula inpred2 = MFormulaManager.makeAtom(mtup.newvar, rel2);
						isoconj_formulas.add(MFormulaManager.makeIFF(inpred1, inpred2));
					}
					
				} // end for each mutant								
			} // end of harder >1-ary case	
		} // end of for each usesI

		if(timDebugMode)
		{
			MEnvironment.writeErrLine("");
			MEnvironment.writeErrLine(leftidx +"="+rightidx + ": "+isoconj_formulas);
			MEnvironment.writeErrLine("");
		}
		
		return MFormulaManager.makeConjunction(isoconj_formulas);
		
	}

	private Set<String> getUsedMutantPredicates(MatrixTuplingV mtup,
			String leftidx, String rightidx, String oldpredstr,
			String[] indexing) 
	{
	
		// There is probably a better way to do this. If this code gets used long term,
		// should at least stop using strings everywhere. - TN 08/10
		
		List<Integer> locationsOfI = new ArrayList<Integer>();
		for(int ii = 0; ii< indexing.length;ii++)
			if(indexing[ii].equals(leftidx))
				locationsOfI.add(ii);
		if(locationsOfI.size() < 1)
			return new HashSet<String>(); // should not happen, but just in case
		
		// We have locationsOfI.size() i's in the string. Each may or may not be changed.
		// Must have at least one change. So (2^num -1) possible mutations.
					
		Set<String> validMutations = mutatePredicatesRecursive(mtup, oldpredstr, indexing, locationsOfI, 0, rightidx); 
		return validMutations;
				
	}

	private Set<String> mutatePredicatesRecursive(MatrixTuplingV mtup, String oldname, String[] indexing, List<Integer> locationsOfI, int posn, String rightidx)
	{	
		Set<String> result = new HashSet<String>();
	
		// posn is the index into the list where we're at
		
		// done! we have a single mutation to report
		if(posn >= locationsOfI.size())
		{
			String indexingstring = "";
			for(int ii = 0; ii < indexing.length;ii++)
			{
				if(indexingstring.length() != 0)
					indexingstring += ","+indexing[ii];
				else
					indexingstring = indexing[ii];
			}
			
			String theMutation = oldname + "_" + indexingstring; 
			
			// DEBUG ONLY
			if(timDebugMode)				
				MEnvironment.writeErrLine("Mutation: "+theMutation);
			
			
			
			if(!mtup.newvocab.predicates.containsKey(theMutation))
			{
				if(timDebugMode)
					MEnvironment.writeErrLine("(Unused!)");
				return result; // this mutation isn't used by the new vocab. ignore.
			}
			
			result.add(theMutation);			
			return result; 
		}
		
		// not done. we still have changes to make. recurse.
		// ---- first: no change
		Set<String> result0 = mutatePredicatesRecursive(mtup, oldname, indexing, locationsOfI, posn+1, rightidx);
		
		// ---- second: change
		String before = indexing[posn];
		indexing[posn] = rightidx;
		Set<String> result1 = mutatePredicatesRecursive(mtup, oldname, indexing, locationsOfI, posn+1, rightidx);;
		indexing[posn] = before;
		
		result0.addAll(result1);
		return result0;		
	}

	protected MSolutionInstance processTupledSolutionForThis(MSolutionInstance partialInstance)
	{
		// Given a TUPLED solution, produce a PRE-tupling solution.
		// Assume this (query) is the tupled query and this.preTuplingQuery is the original query

		// varOrdering is only an ordering on the PUBLISHED variables in the query.
		// We need the entire ordering generated for the tupling.

		// Safety check: This _is_ a post-tupled query, right?
		
		if(internalTupledQuery == null)
			return partialInstance;
		

		// (1) Look at the equalities, decide what the universe is
		HashMap<String, String> idxToAtom = new HashMap<String, String>();
		HashMap<String, String> idxToVar = new HashMap<String, String>();

		List<String> annotations = new ArrayList<String>();
	
		Map<String, Variable> revIndexing = internalTupledQuery.internalTuplingVisitor.pv.revIndexing;
		
		
		// Non-optimized implementation of union-find below
		// (see Wikipedia for better algorithms)
		Map<String, Set<String>> mySets = new HashMap<String, Set<String>>();
		Map<String, String> myRepresentative = new HashMap<String, String>();
		for(String idx : revIndexing.keySet())
		{
			Set<String> singletonSet = new HashSet<String>();
			singletonSet.add(idx);
			mySets.put(idx, singletonSet);
			myRepresentative.put(idx, idx);
		}

		MCommunicator.writeToLog("\nIn conversion from tupled to pre-tupled model. Tupled model is: "+partialInstance.getFacts());
		
		// Take the quotient dictated by the equalities		
		for(Relation r : partialInstance.getFacts().relations())
		{
			// TODO kludge; rel may start with = anyway, but this is a problem with the names already
			if(r.name().startsWith("="))
			{
				// Only one atom, so dont need to check the atom's identity
				if(partialInstance.getFacts().relationTuples().get(r).size() > 0)
				{
					// Here is an equality to deal with...
					
					int needsLastComma = r.name().lastIndexOf(",");
					String rightidx = r.name().substring(needsLastComma + 1);
					String leftidx = r.name().substring(2, needsLastComma);

					String leftrep = myRepresentative.get(leftidx);
					String rightrep = myRepresentative.get(rightidx);

					// leftrep is "taking over" from rightrep (not just rightidx)
					
					// Merge the two sets under left's representative
					mySets.get(leftrep).addAll(mySets.get(rightrep));
					
					// now left's rep becomes representative for right's representative										
					// and also everything that rightrep used to be rep for
					// (otherwise, may have a chain of representation to follow):
					for(String newClient : mySets.get(rightrep))
					{
						myRepresentative.put(newClient, leftrep);
					}
				}
			}
		}

		// Need to make sure the sets are ordered consistently across multiple indices
		Map<String, List<String>> myLists = new HashMap<String, List<String>>();
		for(String idx : mySets.keySet())
		{
			myLists.put(idx, new ArrayList<String>(mySets.get(idx)));
		}
		
		MCommunicator.writeToLog("\n\n  myRep: "+myRepresentative);
		MCommunicator.writeToLog("\n\n  myLists: "+myLists);
		
		// Make the universe and populate index to atoms map
		Set<String> vals = new HashSet<String>();
		for(String idx : revIndexing.keySet())
		{
			// What is the name for atom idx?
			// e.g., x=y=z (if 1,2,3 in same set)
			
			// Convert the set to something more readable
			String aName = "";
			
			String rep = myRepresentative.get(idx);
			for(String anIndexInSet : myLists.get(rep)) // use myLists, not mySets
			{
				String varName = revIndexing.get(anIndexInSet).name();
				
				if(aName.length() < 1)
					aName = aName + varName;
				else
					aName = aName + "=" + varName;
			}
						
			idxToAtom.put(idx, aName);
			idxToVar.put(idx, revIndexing.get(idx).name());
			vals.add(aName);
		}
		
		Universe u = new Universe(vals);
		
		MCommunicator.writeToLog("\n  conversion had universe: "+vals);

		Instance newInstance = new Instance(u);
		Instance dontcare = new Instance(u);


		//MEnvironment.errorStream.println(partialInstance.getFacts());

		// Now fill the relations in the new instance
		for(Relation r : partialInstance.getFacts().relations())
		{
			if(r.name().startsWith("="))
				continue;

		
			int underscoreIndex = r.name().lastIndexOf("_");
			
			// Some special tupled preds don't have an indexing
			String preunderscore = "";
			if(underscoreIndex >= 0)
				preunderscore = r.name().substring(0, underscoreIndex);
		
			/// *****************
			/// IDB OUTPUT
			/// *****************

			// Does the pre-tupling part of this relname appear in our list of IDB names to output?
			if(internalTupledQuery.getIDBNamesToOutput().contains(preunderscore))
			{
				
				// Indexing is after the underscore.
				String[] indexing = r.name().substring(underscoreIndex+1).split(",");

				List<String> theTuple = new ArrayList<String>();
				for(String s : indexing)
				{
					theTuple.add(idxToVar.get(s));
				}


				// 	Cannot just add, may be too many tuples (in full relation) to fit in maxint
				// if so kodkod throws an exception.
				// instance.add(r, theTuples);

				// Instead, we apply duct tape.
				// Annotate the result!
				// Only ONE tuple because of how we restrict to a certain indexing
				// Also: strip the indexing from r.name().
				if(partialInstance.getFacts().relationTuples().get(r).size() > 0)					
					annotations.add(preunderscore + "("+ theTuple+")");
				//else
				//	annotations.add("NOT "+preunderscore + "("+ theTuple+")");
				// Don't spam the user with _negative_ IDBs.

			}			
			else if(partialInstance.getFacts().relationTuples().get(r).size() > 0)
			{

				/// *****************
				/// Other relations
				/// *****************
				if(underscoreIndex > 0)
				{
					String idxing = r.name().substring(underscoreIndex+1);
					String relname = r.name().substring(0, underscoreIndex);
					try
					{
						// The pre-tupling relation name 						
						Relation newr = internalTupledQuery.vocab.getRelation(relname);

						// May not be a single atom, since tupling now supports arbitrary arity predicates.
						// Split the indexing by comma, then get.
						String[] splitIndexing = idxing.split(",");
						List<String> tupleAtoms = new ArrayList<String>(splitIndexing.length);						
						for(String idx : splitIndexing)
							tupleAtoms.add(idxToAtom.get(idx));

						// Singleton tuple set
						Tuple t = u.factory().tuple(tupleAtoms);						
						TupleSet theTuples = u.factory().noneOf(splitIndexing.length);
						theTuples.add(t);						
						
						// If we have not made this (pre-tupling) relation yet, do so
						if(newInstance.relationTuples().containsKey(newr))
							theTuples.addAll(newInstance.relationTuples().get(newr));

						// Add the tuple to the pre-tupling relation
						newInstance.add(newr, theTuples);

						//MEnvironment.errorStream.println(newr+": "+instance.relationTuples().get(newr));
					}
					catch(MGEBadIdentifierName e)
					{
						continue; // move on
					}
					catch(MGEUnknownIdentifier e)
					{
						continue;
					}
				} // underscore index > 0
			} // end if relation populated
			else
			{
				// If unpopulated
				// no reason to add a tuple, is there?
			}

		} // for each relation


		// TODO Deal with don't care's here?
		// Left undone: nothing that uses partial models now

		return new MSolutionInstance(newInstance, dontcare, annotations);
	}

	public void addEDBIncludes(String edbname)
	{
		if(!edbsToForceTupling.containsKey(edbname))
			edbsToForceTupling.put(edbname, new HashSet<List<String>>());		
	}
	
	public void addEDBIncludesIndexing(String edbname, List<String> indexing)
	throws MGEArityMismatch, MGEUnknownIdentifier
	{

		// Add to indexing map
		if(!edbsToForceTupling.containsKey(edbname))
			edbsToForceTupling.put(edbname, new HashSet<List<String>>());
		edbsToForceTupling.get(edbname).add(indexing);
	}

}

class MInternalTupledQuery extends MQuery 
{
	protected MatrixTuplingV internalTuplingVisitor;
	protected MTuplingQuery internalPreTuplingQuery;
	
	// time spent on tupling (if any)
	protected long msTuplingTime;

	MInternalTupledQuery(MVocab voc, Formula tupledFormula, Set<MIDBCollection> tupledIDBCollections)
	{
		super(voc, tupledFormula, tupledIDBCollections);
	}
	
	protected MInternalTupledQuery(MVocab prevVocab, Formula nFormula,
			Map<String, MIDBCollection> prevIDBsList)
			throws MGEUnknownIdentifier,
			MGEBadQueryString, MGEArityMismatch {
		super(nFormula, prevVocab);			
		myIDBCollections = new HashMap<String, MIDBCollection>(prevIDBsList);
	}

	
	public MPreparedQueryContext runQuery() throws MBaseException
	{
		// Just invoke MQuery.runQuery()
		return super.runQuery();
	}

	

}
